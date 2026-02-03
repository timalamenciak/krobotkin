#!/usr/bin/env Rscript

# build_corpus.R (extended)
# Reproducible, citeable Kropotkin corpus builder (public domain English sources)
#
# Extensions implemented:
# 1) More aggressive removal of Gutenberg/Wikisource boilerplate + TOC + production credits
# 2) Chapter/section splitting into many smaller files (better for fine-tuning)
# 3) Unit-test style checks that FAIL the build if boilerplate remains
#
# Outputs:
#   kropotkin_corpus/
#     theory/<work_slug>/chapter_*.txt
#     science/<work_slug>/chapter_*.txt
#     memoir/<work_slug>/chapter_*.txt
#     essays/<work_slug>/section_*.txt
#     metadata/manifest.csv, metadata/sources.bib, metadata/build_log.md, metadata/tests.md
#
# Usage:
#   Rscript build_corpus.R

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

SOURCES <- tribble(
  ~id, ~title, ~year, ~author, ~genre, ~source_type, ~source_path, ~source_url, ~note,
  "pg_4341", "Mutual Aid: A Factor of Evolution", 1902, "Peter Kropotkin", "science",
  "local_txt", path(RAW_DIR, "4341.txt"), "https://www.gutenberg.org/ebooks/4341", "Project Gutenberg download (local file)",
  
  "pg_23428", "The Conquest of Bread", 1892, "Peter Kropotkin", "theory",
  "local_txt", path(RAW_DIR, "23428-0.txt"), "https://www.gutenberg.org/ebooks/23428", "Project Gutenberg download (local file)",
  
  "pg_64353", "Fields, Factories and Workshops", 1899, "Peter Kropotkin", "science",
  "local_txt", path(RAW_DIR, "64353-0.txt"), "https://www.gutenberg.org/ebooks/64353", "Project Gutenberg download (local file)",
  
  "pg_73882", "Memoirs of a Revolutionist", 1906, "Peter Kropotkin", "memoir",
  "local_txt", path(RAW_DIR, "73882-0.txt"), "https://www.gutenberg.org/ebooks/73882", "Project Gutenberg download (local file)",
  
  "pg_31104", "The Place of Anarchism in Socialistic Evolution", 1887, "Peter Kropotkin", "essays",
  "local_txt", path(RAW_DIR, "31104.txt"), "https://www.gutenberg.org/ebooks/31104", "Project Gutenberg download (local file)",
  
  "ws_law_authority", "Law and Authority", 1886, "Peter Kropotkin", "essays",
  "local_txt", path(RAW_DIR, "Law_and_Authority.txt"), "https://en.wikisource.org/wiki/Law_and_Authority", "Wikisource export (local file)",
  
  "ws_state_historic_role", "The State: Its Historic Role", 1903, "Peter Kropotkin", "essays",
  "local_txt", path(RAW_DIR, "The_State_Its_Historic_Role.txt"), "https://en.wikisource.org/wiki/The_State:_Its_Historic_Role", "Wikisource export (local file)"
)

# ---------------------------
# Helpers
# ---------------------------

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
    str_replace_all("[ \t]+\n", "\n") %>%
    str_replace_all("\u00a0", " ") %>%         # nbsp
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
# Cleaning (1): boilerplate + TOC + credits
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
  # Remove common production/credit blocks and internal end-lines that may remain
  # after START/END marker trimming.
  
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
    # The common offender in your snippet:
    "(?im)^\\s*end of project gutenberg.*$"
  )
  
  out <- x
  for (p in credit_pats) out <- str_replace_all(out, regex(p), "")
  out
}

trim_gutenberg_tail_if_present <- function(x) {
  # If we see a Gutenberg end block near the end, drop from that line onward.
  # Handles variants like:
  #   "End of Project Gutenberg's ..."
  #   "End of the Project Gutenberg EBook of ..."
  #   "*** END OF THE PROJECT GUTENBERG EBOOK ..."
  lines <- str_split(x, "\n", simplify = FALSE)[[1]]
  n <- length(lines)
  if (n < 20) return(x)
  
  start <- max(1, n - 400)
  tail <- lines[start:n]
  
  end_pat <- regex(
    "(?i)^(\\*\\*\\*\\s*)?end of( the)? project gutenberg(\\'s)?( ebook)?\\b",
    ignore_case = TRUE
  )
  
  idx <- which(str_detect(tail, end_pat))
  if (!length(idx)) return(x)
  
  cut_at <- (start - 1) + idx[1]
  
  # Also remove a preceding "Printed by ..." line if it's right before the end marker
  if (cut_at > 1 && str_detect(lines[cut_at - 1], regex("(?i)^\\s*printed by\\b"))) {
    cut_at <- cut_at - 1
  }
  
  paste(lines[1:(cut_at - 1)], collapse = "\n")
}

drop_toc_block <- function(x) {
  # Remove a Table of Contents block when it exists near the top.
  # Heuristic: if "CONTENTS" appears early, drop from it until first likely chapter/section heading.
  lines <- str_split(x, "\n", simplify = FALSE)[[1]]
  if (length(lines) < 50) return(x)
  
  # Search within first ~800 lines for "CONTENTS"
  max_scan <- min(length(lines), 800)
  idx_contents <- which(str_detect(lines[1:max_scan], regex("^\\s*contents\\s*$", ignore_case = TRUE)))
  if (!length(idx_contents)) return(x)
  
  i0 <- idx_contents[1]
  
  # Find end marker after contents: CHAPTER / I. / PART / INTRODUCTION etc.
  after <- (i0 + 1):min(length(lines), i0 + 600)
  if (!length(after)) return(x)
  
  end_candidates <- which(str_detect(lines[after], regex("^\\s*(chapter\\b|part\\b|introduction\\b|preface\\b|i\\.|ii\\.|iii\\.|iv\\.|v\\.|vi\\.|vii\\.|viii\\.|ix\\.|x\\.|\\d+\\.)", ignore_case = TRUE)))
  
  if (!length(end_candidates)) return(x)
  
  i1 <- after[end_candidates[1]]
  
  # Drop lines i0..(i1-1) (leave heading line at i1)
  lines2 <- c(lines[1:(i0 - 1)], lines[i1:length(lines)])
  paste(lines2, collapse = "\n")
}

drop_front_matter_noise <- function(x) {
  # Remove repeated title/author lines and "illustrations" lists often found early.
  out <- x
  out <- str_replace_all(out, regex("(?im)^\\s*illustrations\\s*$.*?(\\n\\n|\\Z)", dotall = TRUE), "\n\n")
  out
}


clean_text <- function(row, raw_text) {
  x <- raw_text
  
  # Treat Gutenberg downloads as Gutenberg; treat Wikisource exports as "light clean".
  is_gutenberg <- str_starts(row$id, "pg_") || str_detect(path_file(row$source_path), regex("^\\d", ignore_case = TRUE))
  
  if (is_gutenberg) {
    x <- strip_gutenberg_markers(x)
    x <- drop_gutenberg_production_credits(x)
    x <- trim_gutenberg_tail_if_present(x)
    x <- drop_toc_block(x)
    x <- drop_front_matter_noise(x)
  } else {
    # Wikisource local exports: strip export stamps and navigation artifacts.
    x <- x %>%
      # Common export stamp lines
      str_replace_all("(?im)^\\s*exported from wikisource.*$", "") %>%
      str_replace_all("(?im)^\\s*from wikisource\\s*$", "") %>%
      # Occasional navigation remnants
      str_replace_all("(?im)^\\s*jump to navigation\\s*$", "") %>%
      str_replace_all("(?im)^\\s*jump to search\\s*$", "") %>%
      # Footnote markers if present
      str_replace_all("\\[[0-9]+\\]", "") %>%
      # Clean up excess blank lines created by removals
      str_replace_all("\n{3,}", "\n\n")
  }
  
  normalize_text(x)
}

# ---------------------------
# Splitting (2): chapter/section splitting
# ---------------------------

split_into_units <- function(row, cleaned_text) {
  # Returns a tibble: unit_id, unit_title, unit_text
  # Heuristics vary by genre + typical Gutenberg formatting.
  
  # Work in lines for heading detection
  lines <- str_split(cleaned_text, "\n", simplify = FALSE)[[1]]
  if (length(lines) < 200) {
    return(tibble(unit_id = "unit_001", unit_title = "full_text", unit_text = cleaned_text))
  }
  
  # Candidate heading patterns (order matters; most specific first)
  pats <- list(
    chapter = regex("^\\s*chapter\\s+([ivxlcdm0-9]+)\\b\\.?\\s*(.*)$", ignore_case = TRUE),
    # Require a roman numeral token followed by a dot + space + TitleCase heading.
    # This prevents matching normal sentences like "Institutions ..."
    roman  = regex("^\\s*([IVXLCDM]{1,8})\\.\\s+([A-Z][A-Za-z0-9 ,;:'\"\\-()]{0,80})\\s*$", ignore_case = FALSE),
    number = regex("^\\s*(\\d+)\\.\\s+([A-Z].{0,80})\\s*$", ignore_case = FALSE),
    part   = regex("^\\s*part\\s+([ivxlcdm0-9]+)\\b\\.?\\s*(.*)$", ignore_case = TRUE)
  )
  
  # Detect headings: require surrounding blank lines-ish to avoid false positives
  is_heading_line <- function(i) {
    line <- lines[i]
    if (nchar(str_trim(line)) == 0) return(FALSE)
    
    # Must be relatively short (headings tend to be)
    if (nchar(line) > 80) return(FALSE)
    
    # Must match one of patterns
    any(map_lgl(pats, ~str_detect(line, .x)))
  }
  
  idx <- which(map_lgl(seq_along(lines), is_heading_line))
  
  # If too few headings found, keep as one unit
  if (length(idx) < 2) {
    return(tibble(unit_id = "unit_001", unit_title = "full_text", unit_text = cleaned_text))
  }
  
  # Build unit boundaries: from each heading to line before next heading
  starts <- idx
  ends <- c(idx[-1] - 1, length(lines))
  
  # Extract title from heading
  parse_title <- function(line) {
    for (nm in names(pats)) {
      m <- str_match(line, pats[[nm]])
      if (!all(is.na(m))) {
        # Capture groups start at column 2
        groups <- m[1, 2:ncol(m)]
        groups <- groups[!is.na(groups)]
        # Prefer the last group as "title-like"
        remainder <- if (length(groups) >= 2) groups[length(groups)] else ""
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
    
    # Skip tiny blocks
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
# Tests (3): fail build if boilerplate remains
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
  "(?i)jump to search"
)

assert_no_boilerplate <- function(text, context = "") {
  hits <- keep(boilerplate_patterns, ~str_detect(text, regex(.x, multiline = TRUE)))
  if (length(hits)) {
    # Provide a short snippet around first hit
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

assert_nontrivial <- function(text, min_chars = 2000, context = "") {
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

# Manifest now tracks per-unit outputs (chapter/section files)
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
  
  # Run tests on whole cleaned text (pre-split)
  ctx <- paste0(row$id, " | ", row$title)
  assert_no_boilerplate(cleaned, context = paste0(ctx, " (whole cleaned)"))
  assert_nontrivial(cleaned, context = paste0(ctx, " (whole cleaned)"))
  
  writeLines(paste0("- PASS whole-text tests: ", ctx, "\n"), tests_path)
  
  # Split into chapters/sections
  units <- split_into_units(row, cleaned)
  
  # Write each unit with provenance header
  for (u in seq_len(nrow(units))) {
    unit <- units[u, ]
    
    # Unit filename: chapter_001_some_title.txt (safe slug)
    unit_kind <- if (row$genre %in% c("theory", "science", "memoir")) "chapter" else "section"
    unit_title_slug <- slugify(unit$unit_title)
    if (unit_title_slug == "") unit_title_slug <- unit$unit_id
    
    out_file <- sprintf("%s_%s_%s.txt", unit_kind, str_sub(unit$unit_id, 6, 8), unit_title_slug)
    out_path <- path(work_dir, out_file)
    
    final_txt <- paste0(header_block(row), unit$unit_text, "\n")
    
    # Unit-level tests: ensure header didn't introduce forbidden strings (it won't),
    # and ensure body is still clean + nontrivial.
    # (We only test the body here so the header can keep "SOURCE_URL" etc.)
    assert_no_boilerplate(unit$unit_text, context = paste0(ctx, " | ", out_file))
    assert_nontrivial(unit$unit_text, min_chars = 1200, context = paste0(ctx, " | ", out_file))
    
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

# Write manifest CSV
manifest_path <- path(OUT_DIR, "metadata", "manifest.csv")
write_csv(manifest, manifest_path)

# Write BibTeX
bib_path <- path(OUT_DIR, "metadata", "sources.bib")
bib <- map_chr(seq_len(nrow(SOURCES)), ~write_bib_entry(SOURCES[.x, ])) %>% paste(collapse = "\n")
writeLines(bib, bib_path, useBytes = TRUE)

# Write licensing notes
licenses_path <- path(OUT_DIR, "metadata", "licenses.md")
licenses_txt <- paste0(
  "# Licensing notes\n\n",
  "- This corpus is assembled from public-domain English sources (Project Gutenberg and Wikisource).\n",
  "- Each output file includes a provenance header and is hashed in metadata/manifest.csv.\n",
  "- The build fails if common Gutenberg/Wikisource boilerplate patterns remain.\n",
  "- If you add additional texts (e.g., letters), ensure translations/editions are public domain in your jurisdiction.\n\n",
  "Built: ", timestamp_utc(), "\n"
)
writeLines(licenses_txt, licenses_path)

writeLines(paste0("\nFinished: ", timestamp_utc(), "\n"), log_path)
writeLines(paste0("\nFinished: ", timestamp_utc(), "\n"), tests_path)

message("\nDone.\n- Manifest: ", manifest_path, "\n- BibTeX:   ", bib_path, "\n- Tests:    ", tests_path)

