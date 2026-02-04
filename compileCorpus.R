#!/usr/bin/env Rscript

# build_corpus.R (extended + Anarchist Library support)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(fs)
  library(digest)
})

# ---------------------------
# Config
# ---------------------------

RAW_DIR <- path("data", "raw")
OUT_DIR <- "kropotkin_corpus"
DIRS <- c("theory", "science", "memoir", "essays", "metadata", "tmp")
# Summary chunking (optional, for reporting only)
CHUNK_STATS <- TRUE
CHUNK_MAX_WORDS <- 900
CHUNK_OVERLAP_WORDS <- 80



SOURCES_CSV <- "data/sources.csv"

SOURCES <- readr::read_csv(
  SOURCES_CSV,
  show_col_types = FALSE
) |>
  mutate(
    source_path = path(RAW_DIR, source_path)
  )

# ---------------------------
# Helpers
# ---------------------------

`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

is_manual_source <- function(row) {
  str_starts(row$id, "manual_")
}

count_words <- function(x) {
  x <- str_trim(x)
  if (nchar(x) == 0) return(0L)
  # count "tokens" as whitespace-separated words
  length(str_split(x, "\\s+", simplify = TRUE))
}

chunk_word_windows <- function(text, max_words = 900, overlap = 80) {
  words <- str_split(str_squish(text), "\\s+", simplify = TRUE)
  words <- words[words != ""]
  n <- length(words)
  if (n == 0) return(list())
  if (n <= max_words) return(list(paste(words, collapse = " ")))
  
  chunks <- list()
  i <- 1L
  while (i <= n) {
    j <- min(n, i + max_words - 1L)
    chunks[[length(chunks) + 1L]] <- paste(words[i:j], collapse = " ")
    if (j >= n) break
    i <- max(1L, j - overlap + 1L)
  }
  chunks
}


timestamp_utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

ensure_dirs <- function() {
  dir_create(OUT_DIR)
  walk(DIRS, ~dir_create(path(OUT_DIR, .x)))
}

sha256_file <- function(path) digest(file = path, algo = "sha256")

slugify <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

normalize_text <- function(x) {
  x %>%
    str_replace_all("\r\n", "\n") %>%
    str_replace_all("\r", "\n") %>%
    str_replace_all("\u00a0", " ") %>%         # nbsp
    str_replace_all("[ \t]+\n", "\n") %>%
    str_replace_all("\n{3,}", "\n\n") %>%
    str_trim()
}

header_block <- function(row) {
  paste0(
    "TITLE: ", row$title, "\n",
    "AUTHOR: ", row$author, "\n",
    "YEAR: ", row$year, "\n",
    "SOURCE_ID: ", row$id, "\n",
    "SOURCE_PATH: ", row$source_path, "\n",
    "SOURCE_URL: ", row$source_url, "\n",
    "RETRIEVED_UTC: ", timestamp_utc(), "\n",
    "LICENSE_NOTE: Public domain source (see metadata/manifest.csv)\n",
    "----\n\n"
  )
}

write_bib_entry <- function(row) {
  key <- paste0("kropotkin_", row$year, "_", slugify(row$title))
  paste0(
    "@book{", key, ",\n",
    "  author = {Kropotkin, Peter},\n",
    "  title = {", row$title, "},\n",
    "  year = {", row$year, "},\n",
    "  note = {", row$note, "},\n",
    "  url = {", row$source_url, "}\n",
    "}\n"
  )
}

# ---------------------------
# Cleaning: Gutenberg
# ---------------------------

strip_gutenberg_markers <- function(x) {
  start_pat <- "\\*\\*\\*\\s*START OF (THIS|THE) PROJECT GUTENBERG EBOOK.*\\*\\*\\*"
  end_pat   <- "\\*\\*\\*\\s*END OF (THIS|THE) PROJECT GUTENBERG EBOOK.*\\*\\*\\*"
  
  start_loc <- str_locate(x, regex(start_pat, ignore_case = TRUE))
  end_loc   <- str_locate(x, regex(end_pat, ignore_case = TRUE))
  
  if (!any(is.na(start_loc)) && !any(is.na(end_loc)) && end_loc[1,1] > start_loc[1,2]) {
    str_sub(x, start_loc[1,2] + 1, end_loc[1,1] - 1)
  } else {
    x
  }
}

drop_gutenberg_production_credits <- function(x) {
  credit_pats <- c(
    "(?im)^\\s*produced by.*$",
    "(?im)^\\s*prepared by.*$",
    "(?im)^\\s*transcribed by.*$",
    "(?im)^\\s*scanned by.*$",
    "(?im)^\\s*distributed proofreaders.*$",
    "(?im)^\\s*proofreading team.*$",
    "(?im)^\\s*project gutenberg.*$",
    "(?im)^\\s*www\\.gutenberg\\.org.*$",
    "(?im)^\\s*this ebook.*gutenberg.*$",
    "(?im)^\\s*end of project gutenberg.*$"
  )
  
  out <- x
  for (p in credit_pats) out <- str_replace_all(out, regex(p), "")
  out
}

trim_gutenberg_tail_if_present <- function(x) {
  lines <- str_split(x, "\n", simplify = FALSE)[[1]]
  n <- length(lines)
  if (n < 20) return(x)
  
  start <- max(1, n - 400)
  tail <- lines[start:n]
  
  end_pat <- regex("(?i)^(\\*\\*\\*\\s*)?end of( the)? project gutenberg(\\'s)?( ebook)?\\b", ignore_case = TRUE)
  
  idx <- which(str_detect(tail, end_pat))
  if (!length(idx)) return(x)
  
  cut_at <- (start - 1) + idx[1]
  
  if (cut_at > 1 && str_detect(lines[cut_at - 1], regex("(?i)^\\s*printed by\\b"))) {
    cut_at <- cut_at - 1
  }
  
  paste(lines[1:(cut_at - 1)], collapse = "\n")
}

drop_toc_block <- function(x) {
  lines <- str_split(x, "\n", simplify = FALSE)[[1]]
  if (length(lines) < 50) return(x)
  
  max_scan <- min(length(lines), 800)
  idx_contents <- which(str_detect(lines[1:max_scan], regex("^\\s*contents\\s*$", ignore_case = TRUE)))
  if (!length(idx_contents)) return(x)
  
  i0 <- idx_contents[1]
  after <- (i0 + 1):min(length(lines), i0 + 600)
  if (!length(after)) return(x)
  
  end_candidates <- which(str_detect(
    lines[after],
    regex("^\\s*(chapter\\b|part\\b|introduction\\b|preface\\b|i\\.|ii\\.|iii\\.|iv\\.|v\\.|vi\\.|vii\\.|viii\\.|ix\\.|x\\.|\\d+\\.)", ignore_case = TRUE)
  ))
  if (!length(end_candidates)) return(x)
  
  i1 <- after[end_candidates[1]]
  lines2 <- c(lines[1:(i0 - 1)], lines[i1:length(lines)])
  paste(lines2, collapse = "\n")
}

drop_front_matter_noise <- function(x) {
  out <- x
  out <- str_replace_all(out, regex("(?im)^\\s*illustrations\\s*$.*?(\\n\\n|\\Z)", dotall = TRUE), "\n\n")
  out
}

# ---------------------------
# Cleaning: Anarchist Library
# ---------------------------

drop_al_frontmatter <- function(raw_text, max_scan = 300) {
  # Drops a leading block of "#key ..." lines (and blanks) common in Anarchist Library exports.
  # Robust to the first line being #pubdate, etc.
  lines <- str_split(raw_text, "\n", simplify = FALSE)[[1]]
  n <- length(lines)
  if (n == 0) return(raw_text)
  
  scan_n <- min(n, max_scan)
  
  # Identify the initial contiguous frontmatter-like block: (#something ...) and blank lines
  i <- 1L
  while (i <= scan_n && str_detect(lines[i], regex("^\\s*$"))) i <- i + 1L
  
  # Collect contiguous "#..." / blank lines starting at i
  j <- i
  while (j <= scan_n && (str_detect(lines[j], regex("^\\s*#")) || str_detect(lines[j], regex("^\\s*$")))) {
    j <- j + 1L
  }
  
  # If we didn't actually see any # lines, do nothing
  if (j == i) return(raw_text)
  
  block <- lines[i:(j - 1)]
  
  # Only treat it as AL frontmatter if block contains a #title or #author etc.
  # This prevents stripping content that happens to start with "#".
  looks_like_al <- any(str_detect(block, regex("^\\s*#(title|LISTtitle|pubdate|author|pubdate|source|date|lang|notes|topics|SORTtopics)\\b", ignore_case = TRUE)))
  
  if (!looks_like_al) return(raw_text)
  
  # Drop that block
  paste(lines[j:n], collapse = "\n")
}

convert_al_markup <- function(x) {
  # [[url][label]] -> label ; [[url]] -> url
  x <- str_replace_all(x, "\\[\\[[^\\]]+\\]\\[([^\\]]+)\\]\\]", "\\1")
  x <- str_replace_all(x, "\\[\\[([^\\]]+)\\]\\]", "\\1")
  
  # strip HTML tags like <em>, <strong>, <sup>, etc.
  x <- str_replace_all(x, "<[^>]+>", "")
  
  # remove separator lines "* * *"
  x <- str_replace_all(x, regex("(?m)^\\s*\\*\\s*\\*\\s*\\*\\s*$"), "")
  
  # normalize "*** Heading" to "## Heading" (helps splitting consistency)
  x <- str_replace_all(x, regex("(?m)^\\*\\*\\*\\s*"), "## ")
  
  x
}

clean_anarchist_library <- function(raw_text) {
  x <- raw_text
  x <- drop_al_frontmatter(x)
  x <- convert_al_markup(x)
  
  # some AL exports include minor boilerplate lines; strip conservatively
  x <- x %>%
    str_replace_all("(?im)^\\s*the anarchist library\\s*$", "") %>%
    str_replace_all("\n{3,}", "\n\n")
  
  normalize_text(x)
}

# ---------------------------
# Master cleaner
# ---------------------------

clean_text <- function(row, raw_text) {
  x <- raw_text
  
  # If manual_*, user asserts front/back matter already removed:
  # only normalize whitespace; don't strip or transform.
  if (is_manual_source(row)) {
    return(normalize_text(x))
  }
  
  is_gutenberg <- str_starts(row$id, "pg_") || str_detect(path_file(row$source_path), regex("^\\d", ignore_case = TRUE))
  is_anarchist_library <- str_starts(row$id, "al_") ||
    str_detect(row$source_url, fixed("theanarchistlibrary.org")) ||
    str_detect(x, regex("(?m)^\\s*#title\\b", ignore_case = TRUE))
  
  if (is_gutenberg) {
    x <- strip_gutenberg_markers(x)
    x <- drop_gutenberg_production_credits(x)
    x <- trim_gutenberg_tail_if_present(x)
    x <- drop_toc_block(x)
    x <- drop_front_matter_noise(x)
    x <- normalize_text(x)
    
  } else if (is_anarchist_library) {
    x <- clean_anarchist_library(x)
    
  } else {
    x <- x %>%
      str_replace_all("(?im)^\\s*exported from wikisource.*$", "") %>%
      str_replace_all("(?im)^\\s*from wikisource\\s*$", "") %>%
      str_replace_all("(?im)^\\s*jump to navigation\\s*$", "") %>%
      str_replace_all("(?im)^\\s*jump to search\\s*$", "") %>%
      str_replace_all("\\[[0-9]+\\]", "") %>%
      str_replace_all("\n{3,}", "\n\n") %>%
      normalize_text()
  }
  
  x
}

# ---------------------------
# Splitting: chapter/section splitting
# ---------------------------

split_into_units <- function(row, cleaned_text) {
  lines <- str_split(cleaned_text, "\n", simplify = FALSE)[[1]]
  if (length(lines) < 200) {
    return(tibble(unit_id = "unit_001", unit_title = "full_text", unit_text = cleaned_text))
  }
  
  pats <- list(
    chapter = regex("^\\s*chapter\\s+([ivxlcdm0-9]+)\\b\\.?\\s*(.*)$", ignore_case = TRUE),
    roman  = regex("^\\s*([IVXLCDM]{1,8})\\.\\s+([A-Z][A-Za-z0-9 ,;:'\"\\-()]{0,80})\\s*$", ignore_case = FALSE),
    number = regex("^\\s*(\\d+)\\.\\s+([A-Z].{0,80})\\s*$", ignore_case = FALSE),
    part   = regex("^\\s*part\\s+([ivxlcdm0-9]+)\\b\\.?\\s*(.*)$", ignore_case = TRUE),
    # Also treat our normalized AL headings (## ...) as headings
    md_h2  = regex("^\\s*##\\s+(.{1,80})\\s*$", ignore_case = FALSE)
  )
  
  is_heading_line <- function(i) {
    line <- lines[i]
    if (nchar(str_trim(line)) == 0) return(FALSE)
    if (nchar(line) > 120) return(FALSE)  # allow "## ..." headings a bit longer
    any(map_lgl(pats, ~str_detect(line, .x)))
  }
  
  idx <- which(map_lgl(seq_along(lines), is_heading_line))
  if (length(idx) < 2) {
    return(tibble(unit_id = "unit_001", unit_title = "full_text", unit_text = cleaned_text))
  }
  
  starts <- idx
  ends <- c(idx[-1] - 1, length(lines))
  
  parse_title <- function(line) {
    for (nm in names(pats)) {
      m <- str_match(line, pats[[nm]])
      if (!all(is.na(m))) {
        groups <- m[1, 2:ncol(m)]
        groups <- groups[!is.na(groups)]
        remainder <- if (length(groups) >= 1) groups[length(groups)] else ""
        remainder <- str_trim(remainder)
        if (remainder == "") remainder <- nm
        return(remainder)
      }
    }
    "section"
  }
  
  units <- map2_dfr(seq_along(starts), starts, function(k, s) {
    e <- ends[k]
    block <- paste(lines[s:e], collapse = "\n") %>% normalize_text()
    
    if (nchar(block) < 1200) return(NULL)
    
    heading <- lines[s]
    title <- parse_title(heading)
    
    tibble(
      unit_id = sprintf("unit_%03d", k),
      unit_title = title,
      unit_text = block
    )
  })
  
  if (nrow(units) == 0) {
    tibble(unit_id = "unit_001", unit_title = "full_text", unit_text = cleaned_text)
  } else {
    units
  }
}

# ---------------------------
# Tests: fail build if boilerplate remains
# ---------------------------

boilerplate_patterns <- c(
  "(?i)\\bproject gutenberg\\b",
  "(?i)\\bwww\\.gutenberg\\.org\\b",
  "(?i)\\*\\*\\*\\s*start of (this|the) project gutenberg ebook",
  "(?i)\\*\\*\\*\\s*end of (this|the) project gutenberg ebook",
  "(?i)^\\s*produced by\\b",
  "(?i)^\\s*transcribed by\\b",
  "(?i)distributed proofreaders",
  "(?i)from wikisource",
  "(?i)jump to navigation",
  "(?i)jump to search",
  # Anarchist Library frontmatter should not survive cleaning
  "(?im)^\\s*#(title|author|source|date|lang|notes|topics|sorttopics|listtitle)\\b",
  "(?i)the anarchist library"
)

assert_no_boilerplate <- function(text, context = "") {
  hits <- keep(boilerplate_patterns, ~str_detect(text, regex(.x, multiline = TRUE)))
  if (length(hits)) {
    hit_pat <- hits[[1]]
    loc <- str_locate(text, regex(hit_pat, multiline = TRUE))
    snippet <- str_sub(text, max(1, loc[1] - 200), min(nchar(text), loc[2] + 200))
    stop(
      "Boilerplate test FAILED for: ", context, "\n",
      "Matched pattern: ", hit_pat, "\n",
      "Snippet:\n---\n", snippet, "\n---\n"
    )
  }
  TRUE
}

assert_nontrivial <- function(text, min_chars = 1000, context = "") {
  if (nchar(text) < min_chars) {
    stop("Text too short (", nchar(text), " chars) for: ", context)
  }
  TRUE
}

# ---------------------------
# Build
# ---------------------------

ensure_dirs()

log_path <- path(OUT_DIR, "metadata", "build_log.md")
tests_path <- path(OUT_DIR, "metadata", "tests.md")
writeLines(paste0("# Build log\n\nStarted: ", timestamp_utc(), "\n"), log_path)
writeLines(paste0("# Test log\n\nStarted: ", timestamp_utc(), "\n"), tests_path)

manifest_rows <- list()

for (i in seq_len(nrow(SOURCES))) {
  row <- SOURCES[i, ]
  work_slug <- slugify(row$title)
  work_dir <- path(OUT_DIR, row$genre, work_slug)
  dir_create(work_dir)
  
  message("Reading: ", row$title, " (local_txt)")
  writeLines(paste0("- Reading: ", row$title, " (", row$source_path, ")\n"), log_path)
  
  if (!file_exists(row$source_path)) {
    stop("Missing source file: ", row$source_path)
  }
  
  raw_text <- read_file(row$source_path)
  cleaned <- clean_text(row, raw_text)
  
  ctx <- paste0(row$id, " | ", row$title)
  
  # Always ensure non-trivial
  assert_nontrivial(cleaned, context = paste0(ctx, " (whole cleaned)"))
  
  # Only enforce boilerplate tests if not manual
  if (!is_manual_source(row)) {
    assert_no_boilerplate(cleaned, context = paste0(ctx, " (whole cleaned)"))
    writeLines(paste0("- PASS whole-text tests: ", ctx, "\n"), tests_path)
  } else {
    writeLines(paste0("- SKIP boilerplate tests (manual): ", ctx, "\n"), tests_path)
  }
  
  units <- split_into_units(row, cleaned)
  
  for (u in seq_len(nrow(units))) {
    unit <- units[u, ]
    
    unit_kind <- if (row$genre %in% c("theory", "science", "memoir")) "chapter" else "section"
    unit_title_slug <- slugify(unit$unit_title)
    if (unit_title_slug == "") unit_title_slug <- unit$unit_id
    
    out_file <- sprintf("%s_%s_%s.txt", unit_kind, str_sub(unit$unit_id, 6, 8), unit_title_slug)
    out_path <- path(work_dir, out_file)
    
    final_txt <- paste0(header_block(row), unit$unit_text, "\n")
    
    assert_nontrivial(unit$unit_text, min_chars = 1200, context = paste0(ctx, " | ", out_file))
    if (!is_manual_source(row)) {
      assert_no_boilerplate(unit$unit_text, context = paste0(ctx, " | ", out_file))
    }
    
    writeLines(final_txt, out_path, useBytes = TRUE)
    
    manifest_rows[[length(manifest_rows) + 1]] <- tibble(
      work_id = row$id,
      title = row$title,
      year = row$year,
      author = row$author,
      genre = row$genre,
      source_type = row$source_type,
      source_path = row$source_path,
      source_url = row$source_url,
      note = row$note,
      work_slug = work_slug,
      unit_id = unit$unit_id,
      unit_title = unit$unit_title,
      retrieved_utc = timestamp_utc(),
      out_path = out_path,
      sha256 = sha256_file(out_path),
      n_chars = nchar(final_txt, type = "chars", allowNA = FALSE)
    )
  }
  
  message("  -> wrote ", nrow(units), " unit(s) in: ", work_dir)
  writeLines(paste0("- Wrote units: ", ctx, " (", nrow(units), ")\n"), log_path)
}

manifest <- bind_rows(manifest_rows)

manifest_path <- path(OUT_DIR, "metadata", "manifest.csv")
write_csv(manifest, manifest_path)

# ---------------------------
# Summary statistics
# ---------------------------

# Read all written unit files and compute body stats (exclude provenance header)
strip_provenance_header <- function(txt) {
  # Robustly split on the first "----" line anywhere near the top.
  # Handles \r\n, extra spaces, and both "----\n\n" and "\n----\n" variants.
  x <- txt %>%
    str_replace_all("\r\n", "\n") %>%
    str_replace_all("\r", "\n")
  
  # Find the first delimiter line that is exactly ---- (optionally surrounded by spaces)
  m <- str_locate(x, regex("(?m)^\\s*----\\s*$"))
  if (all(is.na(m))) {
    # No delimiter found; return whole text (better than empty)
    return(x)
  }
  
  # Return everything after the delimiter line
  after <- str_sub(x, m[1,2] + 1, nchar(x))
  str_trim(after)
}

unit_paths <- manifest$out_path
# sanity: do the output files exist?
missing_units <- unit_paths[!file_exists(unit_paths)]
if (length(missing_units)) {
  stop("Summary stats: some out_path files listed in manifest do not exist. Example: ", missing_units[[1]])
}

unit_texts <- map_chr(unit_paths, read_file)
unit_bodies <- map_chr(unit_texts, ~normalize_text(strip_provenance_header(.x)))

empty_frac <- mean(nchar(unit_bodies) == 0)
if (empty_frac > 0.05) {
  # show first few offenders
  offenders <- unit_paths[nchar(unit_bodies) == 0][1:min(5, sum(nchar(unit_bodies) == 0))]
  stop(
    "Summary stats: body extraction produced empty text for ",
    round(empty_frac * 100, 1), "% of units.\n",
    "Example empty units:\n- ", paste(offenders, collapse = "\n- ")
  )
}


stats_df <- manifest %>%
  mutate(
    unit_body = unit_bodies,
    body_chars = nchar(unit_body, type = "chars", allowNA = FALSE),
    body_words = map_int(unit_body, count_words)
  ) %>%
  select(-unit_body)

# Optional: "chunk" stats for downstream fine-tuning windows (report only)
chunk_stats <- NULL
if (isTRUE(CHUNK_STATS)) {
  chunk_counts <- map_int(unit_bodies, ~length(chunk_word_windows(.x, CHUNK_MAX_WORDS, CHUNK_OVERLAP_WORDS)))
  chunk_words  <- map_int(unit_bodies, function(x) {
    ch <- chunk_word_windows(x, CHUNK_MAX_WORDS, CHUNK_OVERLAP_WORDS)
    if (!length(ch)) return(0L)
    sum(map_int(ch, count_words))
  })
  chunk_stats <- tibble(
    out_path = unit_paths,
    est_chunks = chunk_counts,
    est_chunk_words_total = chunk_words
  )
  stats_df <- stats_df %>% left_join(chunk_stats, by = "out_path")
}

# Overall aggregates
overall <- list(
  n_works = n_distinct(stats_df$work_id),
  n_units = nrow(stats_df),
  total_body_chars = sum(stats_df$body_chars),
  total_body_words = sum(stats_df$body_words),
  mean_unit_chars = round(mean(stats_df$body_chars), 1),
  median_unit_chars = as.numeric(median(stats_df$body_chars)),
  min_unit_chars = min(stats_df$body_chars),
  max_unit_chars = max(stats_df$body_chars),
  mean_unit_words = round(mean(stats_df$body_words), 1),
  median_unit_words = as.numeric(median(stats_df$body_words)),
  min_unit_words = min(stats_df$body_words),
  max_unit_words = max(stats_df$body_words)
)

if (isTRUE(CHUNK_STATS)) {
  overall$est_total_chunks <- sum(stats_df$est_chunks, na.rm = TRUE)
  overall$mean_chunks_per_unit <- round(mean(stats_df$est_chunks, na.rm = TRUE), 2)
  overall$chunk_max_words <- CHUNK_MAX_WORDS
  overall$chunk_overlap_words <- CHUNK_OVERLAP_WORDS
}

overall_df <- tibble(metric = names(overall), value = unlist(overall))

# By-genre breakdown
by_genre <- stats_df %>%
  group_by(genre) %>%
  summarise(
    n_works = n_distinct(work_id),
    n_units = n(),
    total_body_chars = sum(body_chars),
    total_body_words = sum(body_words),
    mean_unit_words = round(mean(body_words), 1),
    median_unit_words = as.numeric(median(body_words)),
    .groups = "drop"
  )

# Length distribution quick check
quantiles_chars <- quantile(stats_df$body_chars, probs = c(.05, .25, .5, .75, .95))
quantiles_words <- quantile(stats_df$body_words, probs = c(.05, .25, .5, .75, .95))

# Longest/shortest units for sanity checking
top_long <- stats_df %>%
  arrange(desc(body_words)) %>%
  slice_head(n = 10) %>%
  select(work_id, title, genre, unit_id, unit_title, out_path, body_words, body_chars)

top_short <- stats_df %>%
  arrange(body_words) %>%
  slice_head(n = 10) %>%
  select(work_id, title, genre, unit_id, unit_title, out_path, body_words, body_chars)

# Write CSVs
summary_csv <- path(OUT_DIR, "metadata", "summary_stats.csv")
write_csv(overall_df, summary_csv)

by_genre_csv <- path(OUT_DIR, "metadata", "summary_by_genre.csv")
write_csv(by_genre, by_genre_csv)

units_stats_csv <- path(OUT_DIR, "metadata", "unit_stats.csv")
write_csv(stats_df, units_stats_csv)

top_long_csv <- path(OUT_DIR, "metadata", "top_longest_units.csv")
write_csv(top_long, top_long_csv)

top_short_csv <- path(OUT_DIR, "metadata", "top_shortest_units.csv")
write_csv(top_short, top_short_csv)

# Write a human-readable markdown report
summary_md <- path(OUT_DIR, "metadata", "summary_stats.md")

md <- c(
  "# Corpus summary statistics",
  "",
  paste0("Built: ", timestamp_utc()),
  "",
  "## Overall",
  "",
  paste0("- Works: **", overall$n_works, "**"),
  paste0("- Units (chapter/section files): **", overall$n_units, "**"),
  paste0("- Total words (unit bodies): **", format(overall$total_body_words, big.mark = ","), "**"),
  paste0("- Total characters (unit bodies): **", format(overall$total_body_chars, big.mark = ","), "**"),
  paste0("- Unit words (min/median/mean/max): **", overall$min_unit_words, " / ", overall$median_unit_words, " / ", overall$mean_unit_words, " / ", overall$max_unit_words, "**"),
  paste0("- Unit chars (min/median/mean/max): **", overall$min_unit_chars, " / ", overall$median_unit_chars, " / ", overall$mean_unit_chars, " / ", overall$max_unit_chars, "**")
)

if (isTRUE(CHUNK_STATS)) {
  md <- c(md,
          "",
          "## Estimated training chunks (report-only)",
          "",
          paste0("- Chunk window: **", CHUNK_MAX_WORDS, "** words, overlap **", CHUNK_OVERLAP_WORDS, "** words"),
          paste0("- Estimated total chunks: **", format(overall$est_total_chunks, big.mark = ","), "**"),
          paste0("- Mean chunks per unit: **", overall$mean_chunks_per_unit, "**")
  )
}

md <- c(md,
        "",
        "## Quantiles (unit body length)",
        "",
        paste0("- Chars (5/25/50/75/95%): **", paste(format(quantiles_chars, big.mark=","), collapse = " / "), "**"),
        paste0("- Words (5/25/50/75/95%): **", paste(format(quantiles_words, big.mark=","), collapse = " / "), "**"),
        "",
        "## By genre",
        ""
)

# Render by-genre as simple markdown lines (no tables)
for (g in seq_len(nrow(by_genre))) {
  r <- by_genre[g,]
  md <- c(md,
          paste0("- **", r$genre, "**: works=", r$n_works,
                 ", units=", r$n_units,
                 ", words=", format(r$total_body_words, big.mark=","),
                 ", chars=", format(r$total_body_chars, big.mark=","))
  )
}

md <- c(md,
        "",
        "## Outputs",
        "",
        paste0("- `metadata/summary_stats.csv` (overall metrics)"),
        paste0("- `metadata/summary_by_genre.csv`"),
        paste0("- `metadata/unit_stats.csv` (per-unit stats)"),
        paste0("- `metadata/top_longest_units.csv`"),
        paste0("- `metadata/top_shortest_units.csv`")
)

writeLines(md, summary_md, useBytes = TRUE)

message("Summary stats written:\n- ", summary_md, "\n- ", summary_csv, "\n- ", by_genre_csv, "\n- ", units_stats_csv)


bib_path <- path(OUT_DIR, "metadata", "sources.bib")
bib <- map_chr(seq_len(nrow(SOURCES)), ~write_bib_entry(SOURCES[.x, ])) %>% paste(collapse = "\n")
writeLines(bib, bib_path, useBytes = TRUE)

licenses_path <- path(OUT_DIR, "metadata", "licenses.md")
licenses_txt <- paste0(
  "# Licensing notes\n\n",
  "- This corpus is assembled from public-domain English sources (Project Gutenberg, Wikisource, and Anarchist Library exports where applicable).\n",
  "- Each output file includes a provenance header and is hashed in metadata/manifest.csv.\n",
  "- The build fails if common boilerplate patterns remain.\n",
  "- If you add additional texts (e.g., letters), ensure translations/editions are public domain in your jurisdiction.\n\n",
  "Built: ", timestamp_utc(), "\n"
)
writeLines(licenses_txt, licenses_path)

writeLines(paste0("\nFinished: ", timestamp_utc(), "\n"), log_path)
writeLines(paste0("\nFinished: ", timestamp_utc(), "\n"), tests_path)

message("\nDone.\n- Manifest: ", manifest_path, "\n- BibTeX:   ", bib_path, "\n- Tests:    ", tests_path)
