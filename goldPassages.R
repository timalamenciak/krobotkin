# This script extracts passages from Kropotkin stratified by keyword.
# These passages can then be used to make instruction set training with Claude.
# make_gold_manifest.R
suppressPackageStartupMessages({
  library(fs); library(stringr); library(readr); library(dplyr); library(tibble); library(purrr); library(digest)
})

IN_DIR <- "kropotkin_corpus"
OUT <- "instruction_data/gold_manifest.csv"
dir_create("instruction_data")

sha256_text <- function(x) digest(x, algo="sha256")

strip_provenance_header <- function(txt) {
  parts <- str_split(txt, "\n----\n", n = 2, simplify = TRUE)
  
  # No separator found
  if (ncol(parts) < 2) return(parts[,1])
  
  a <- parts[,1]
  b <- parts[,2]
  
  # Keep whichever side looks like the main text (usually longer)
  ifelse(nchar(a) >= nchar(b), a, b)
}

normalize_ws <- function(x) {
  x %>% str_replace_all("\r\n","\n") %>% str_replace_all("\r","\n") %>%
    str_replace_all("\u00a0"," ") %>% str_replace_all("[ \t]+"," ") %>%
    str_replace_all("[ \t]*\n[ \t]*","\n") %>% str_replace_all("\n{3,}","\n\n") %>% str_trim()
}
chunk_by_words <- function(text, max_words=900, overlap=80) {
  words <- str_split(text, "\\s+", simplify=FALSE)[[1]]
  n <- length(words)
  if (n <= max_words) return(list(text))
  chunks <- list(); i <- 1L
  while (i <= n) {
    j <- min(n, i + max_words - 1L)
    chunks[[length(chunks)+1L]] <- paste(words[i:j], collapse=" ")
    if (j >= n) break
    i <- max(1L, j - overlap + 1L)
  }
  chunks
}

files <- dir_ls(IN_DIR, recurse=TRUE, regexp="(chapter|section)_.*\\.txt$")

stopifnot(length(files) > 0)
# Predefine schema so df keeps columns even if no rows are collected
df <- tibble(
  source_file = character(),
  chunk_index = integer(),
  passage_sha256 = character(),
  passage = character()
)

rows <- list()
for (f in files) {
  raw <- read_file(f)
  body <- normalize_ws(strip_provenance_header(raw))
  chunks <- chunk_by_words(body, 900, 80)
  for (i in seq_along(chunks)) {
    p <- normalize_ws(chunks[[i]])
    if (nchar(p) < 1200) next
    rows[[length(rows)+1]] <- tibble(
      source_file = as.character(f),
      chunk_index = i,
      passage_sha256 = sha256_text(p),
      passage = p
    )
  }
}

# Bind into the predefined schema (preserves columns even if rows is empty)
if (length(rows) > 0) df <- bind_rows(df, !!!rows)

# Fail early with a clear diagnosis (instead of a confusing mutate error)
if (nrow(df) == 0) {
  cat("Files scanned:", length(files), "\n")
  cat("Passages kept:", nrow(df), "\n")
  stop(
    "No passages collected. Likely all chunks were filtered out by nchar(p) < 1200.\n",
    "Try lowering the threshold (e.g., 600), increasing chunk size, or inspect file sizes."
  )
}


rows <- list()
for (f in files) {
  raw <- read_file(f)
  body <- normalize_ws(strip_provenance_header(raw))
  chunks <- chunk_by_words(body, 900, 80)
  for (i in seq_along(chunks)) {
    p <- normalize_ws(chunks[[i]])
    if (nchar(p) < 400) next
    rows[[length(rows)+1]] <- tibble(
      source_file = as.character(f),
      chunk_index = i,
      passage_sha256 = sha256_text(p),
      passage = p
    )
  }
}

# Three strata by keyword match (tweak keywords if you want)
df <- df %>% mutate(
  stratum = case_when(
    str_detect(passage, regex("\\b(mutual aid|cooperation|solidarity|support)\\b", ignore_case=TRUE)) ~ "mutual_aid",
    str_detect(passage, regex("\\b(state|law|authority|government|parliament|police)\\b", ignore_case=TRUE)) ~ "state_law",
    str_detect(passage, regex("\\b(science|scientific|evolution|method|ethics|nature)\\b", ignore_case=TRUE)) ~ "science_method",
    TRUE ~ "other"
  )
)

pick_n <- function(data, n) {
  # spread across works by taking at most 2 per file first, then fill
  data <- data %>% mutate(file_key = source_file)
  out <- data %>%
    group_by(file_key) %>% slice_head(n = 2) %>% ungroup()
  if (nrow(out) >= n) return(out %>% slice_head(n=n))
  remaining <- anti_join(data, out, by = c("source_file","chunk_index","passage_sha256"))
  bind_rows(out, remaining) %>% slice_head(n=n)
}

gold <- bind_rows(
  pick_n(df %>% filter(stratum=="mutual_aid"), 200),
  pick_n(df %>% filter(stratum=="state_law"), 150),
  pick_n(df %>% filter(stratum=="science_method"), 150)
) %>% distinct(passage_sha256, .keep_all=TRUE)

# Save without full passage if you want smaller; I keep passage for convenience.
write_csv(gold %>% select(source_file, chunk_index, passage_sha256, stratum, passage), OUT)
cat("Wrote gold manifest: ", OUT, " (", nrow(gold), " passages)\n", sep="")
