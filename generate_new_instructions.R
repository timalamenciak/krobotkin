#!/usr/bin/env Rscript

# generate_new_instructions.R
#
# CRASH-PROOF VERSION: writes each example to disk immediately and
# resumes from where it left off if interrupted.
#
# Generates NEW instruction-response pairs from gold passages, skipping
# any (passage × example_type) combinations that already exist in either
# the previous run's metadata or this script's partial output.
#
# What's new vs. the first run:
#   1. Modern synthesis for ALL 495 passages (first run only did 30)
#   2. Comparative examples (new type, 1 per passage)
#   3. More contrastive examples (first run sampled only 150)
#   4. Optional: alternative grounded examples (paraphrase style)
#
# Usage:
#   export ANTHROPIC_API_KEY="sk-ant-..."
#   Rscript generate_new_instructions.R
#
#   # If it crashes, just re-run — it picks up where it left off:
#   Rscript generate_new_instructions.R
#
# Requires:
#   - ANTHROPIC_API_KEY env var set
#   - ellmer package installed
#   - instruction_data/gold_manifest.csv (from goldPassages.R)
#   - instruction_data/instructions_metadata.csv (from first run)

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(purrr); library(tibble); library(stringr)
  library(jsonlite); library(fs); library(glue); library(ellmer)
})

# ---- config ----
GOLD_IN            <- "instruction_data/gold_manifest.csv"
PREV_META          <- "instruction_data/instructions_metadata.csv"

# Incremental output files (appended to as we go)
PARTIAL_JSONL      <- "instruction_data/instructions_new_partial.jsonl"
PARTIAL_META       <- "instruction_data/instructions_new_partial_meta.csv"

# Final merged output
MERGED_JSONL       <- "instruction_data/instructions_all.jsonl"

MODEL_NAME         <- "claude-opus-4-5-20251101"

# Set to TRUE to generate extra grounded examples (paraphrase style)
DO_EXTRA_GROUNDED  <- FALSE

# Number of additional contrastive examples to sample
N_NEW_CONTRASTIVE  <- 350

# Max retries per API call before giving up on a passage
MAX_RETRIES        <- 3
RETRY_DELAY_SEC    <- 5

dir_create(path_dir(PARTIAL_JSONL))

# ---- helpers ----

strip_fences <- function(x) {
  x <- stringr::str_trim(x)
  x <- stringr::str_replace(x, "^```(?:json)?\\s*", "")
  x <- stringr::str_replace(x, "\\s*```\\s*$", "")
  x
}

escape_newlines_in_strings <- function(x) {
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  out <- character(0)
  in_str <- FALSE
  esc <- FALSE
  for (ch in chars) {
    if (esc) { out <- c(out, ch); esc <- FALSE; next }
    if (ch == "\\") { out <- c(out, ch); esc <- TRUE; next }
    if (ch == "\"") { in_str <- !in_str; out <- c(out, ch); next }
    if (in_str && ch == "\n") { out <- c(out, "\\n") }
    else if (in_str && ch == "\r") { }
    else { out <- c(out, ch) }
  }
  paste0(out, collapse = "")
}

parse_json_or_null <- function(x) {
  tryCatch(
    jsonlite::fromJSON(escape_newlines_in_strings(strip_fences(x))),
    error = function(e) NULL
  )
}

wrap_task <- function(passage, task) {
  paste0("PASSAGE:\n", passage, "\n\nTASK:\n", task)
}

# Safe API call with retries
ellmer_chat_text_safe <- function(prompt_text) {
  for (attempt in seq_len(MAX_RETRIES)) {
    result <- tryCatch({
      chat <- chat_anthropic(
        model = MODEL_NAME,
        params = params(temperature = 0.1),
      )
      chat$chat(prompt_text)
    }, error = function(e) {
      msg <- conditionMessage(e)
      cat("    API error (attempt ", attempt, "/", MAX_RETRIES, "): ",
          substr(msg, 1, 120), "\n", sep = "")
      NULL
    })
    
    if (!is.null(result)) return(result)
    
    if (attempt < MAX_RETRIES) {
      cat("    Retrying in ", RETRY_DELAY_SEC, "s...\n", sep = "")
      Sys.sleep(RETRY_DELAY_SEC)
    }
  }
  return(NULL)
}

# ---- incremental disk writers ----

# Escape a string for safe CSV embedding (double quotes)
csv_escape <- function(x) {
  gsub('"', '""', as.character(x))
}

# Initialise the metadata CSV with header if it doesn't exist
init_meta_csv <- function(path) {
  if (!file_exists(path)) {
    header <- paste0(
      '"instruction","response","passage_sha256","stratum",',
      '"source_file","chunk_index","example_type","tag"'
    )
    writeLines(header, path)
    cat("Initialised metadata CSV: ", path, "\n", sep = "")
  }
}

# Append one example to both JSONL and metadata CSV
append_example <- function(instruction, response, passage_sha256, stratum,
                           source_file, chunk_index, example_type, tag) {
  
  # JSONL line
  jline <- toJSON(
    list(instruction = instruction, response = response),
    auto_unbox = TRUE
  )
  cat(jline, "\n", file = PARTIAL_JSONL, append = TRUE, sep = "")
  
  # Metadata CSV line
  meta_line <- paste0(
    '"', csv_escape(instruction), '",',
    '"', csv_escape(response), '",',
    '"', csv_escape(passage_sha256), '",',
    '"', csv_escape(stratum), '",',
    '"', csv_escape(source_file), '",',
    '"', csv_escape(chunk_index), '",',
    '"', csv_escape(example_type), '",',
    '"', csv_escape(tag), '"'
  )
  cat(meta_line, "\n", file = PARTIAL_META, append = TRUE, sep = "")
}

# ---- load completed work for resume ----

load_done_keys <- function() {
  # Returns a set of "sha256|example_type" strings for work already done
  keys <- character(0)
  
  # From previous run
  if (file_exists(PREV_META)) {
    prev <- tryCatch(read_csv(PREV_META, show_col_types = FALSE), error = function(e) NULL)
    if (!is.null(prev) && nrow(prev) > 0 &&
        all(c("passage_sha256", "example_type") %in% names(prev))) {
      keys <- c(keys, paste0(prev$passage_sha256, "|", prev$example_type))
    }
  }
  
  # From this run's partial output
  if (file_exists(PARTIAL_META) && file.size(PARTIAL_META) > 50) {
    partial <- tryCatch(read_csv(PARTIAL_META, show_col_types = FALSE), error = function(e) NULL)
    if (!is.null(partial) && nrow(partial) > 0 &&
        all(c("passage_sha256", "example_type") %in% names(partial))) {
      keys <- c(keys, paste0(partial$passage_sha256, "|", partial$example_type))
    }
  }
  
  unique(keys)
}

is_done <- function(sha, example_type, done_keys) {
  paste0(sha, "|", example_type) %in% done_keys
}

# ---- prompts ----

PROMPT_MODERN <- "
You are generating training examples for a model named Krobotkin.

Given the passage below:
- Write ONE instruction that asks to apply the passage's ideas to a modern topic (e.g., conservation, restoration, climate adaptation, community governance).
- Write ONE response that connects the passage's ideas to the modern topic.

Rules:
- You MAY use modern concepts, but do NOT claim the passage mentions modern organizations, laws, technologies, or statistics.
- No external citations.
Return ONLY valid JSON in this schema:
{\"instruction\":\"...\",\"response\":\"...\"}

Return minified JSON (single line), no code fences, and no literal line breaks inside strings (use \\n if needed).

PASSAGE:
"

PROMPT_COMPARATIVE <- "
You are generating training examples for a model named Krobotkin, fine-tuned on Peter Kropotkin's collected works.

Given the passage below:
- Write ONE instruction that asks the model to relate the passage's ideas to another area of Kropotkin's thought (e.g., mutual aid, ethics, the state, science, decentralisation, labour).
- Write ONE response that draws a substantive connection, using ideas and reasoning consistent with Kropotkin's broader philosophy.

Rules:
- The response should go beyond the passage but stay within what Kropotkin actually argued across his works.
- No modern citations or external references.
Return ONLY valid JSON in this schema:
{\"instruction\":\"...\",\"response\":\"...\"}

Return minified JSON (single line), no code fences, and no literal line breaks inside strings (use \\n if needed).

PASSAGE:
"

PROMPT_GROUNDED_ALT <- "
You are generating training examples for a model named Krobotkin.

Given the passage below, create EXACTLY 3 instruction-response pairs where each instruction asks the model to EXPLAIN or PARAPHRASE a concept from the passage in simpler terms, using ONLY the passage content.

Requirements:
- Each instruction must say to use only the passage.
- Responses should rephrase key ideas clearly without adding outside information.
- Keep responses concise but faithful to the passage.
Return ONLY valid JSON in this schema:
{
  \"examples\": [
    {\"instruction\": \"...\", \"response\": \"...\"},
    {\"instruction\": \"...\", \"response\": \"...\"},
    {\"instruction\": \"...\", \"response\": \"...\"}
  ]
}

Return minified JSON (single line), no code fences, and no literal line breaks inside strings (use \\n if needed).

PASSAGE:
"

# ---- contrastive generators (programmatic — no API needed) ----
NOT_SUPPORTED <- "Not supported by the passage."
NO_EXTERNAL   <- "I can only use the provided passage and cannot add external facts, citations, or verification."

make_contrastive_extended <- function(passage) {
  tribble(
    ~instruction, ~response, ~tag,
    wrap_task(passage, "Using only the passage, provide a statistical comparison of GDP growth rates mentioned by Kropotkin."), NOT_SUPPORTED, "not_in_passage",
    wrap_task(passage, "Using only the passage, list the specific dates and locations of field experiments described."), NOT_SUPPORTED, "not_in_passage",
    wrap_task(passage, "Using only the passage, compare Kropotkin's argument with Marx's theory of surplus value and provide direct quotes from both."), NO_EXTERNAL, "external_citations",
    wrap_task(passage, "Using only the passage, provide DOI links to the scientific studies referenced."), NO_EXTERNAL, "external_citations",
    wrap_task(passage, "Using only the passage, explain what Kropotkin says about artificial intelligence and automation."), NOT_SUPPORTED, "anachronism",
    wrap_task(passage, "Using only the passage, describe Kropotkin's views on cryptocurrency and decentralised finance."), NOT_SUPPORTED, "anachronism",
    wrap_task(passage, "Using only the passage, name the specific NGOs that Kropotkin founded."), NOT_SUPPORTED, "not_in_passage"
  )
}

# ---- generation wrappers (safe, returns NULL on failure) ----

generate_modern_for_passage <- function(passage) {
  raw <- ellmer_chat_text_safe(paste0(PROMPT_MODERN, passage))
  if (is.null(raw)) return(NULL)
  parsed <- parse_json_or_null(raw)
  if (is.null(parsed) || is.null(parsed$instruction) || is.null(parsed$response)) return(NULL)
  tibble(instruction = parsed$instruction, response = parsed$response)
}

generate_comparative_for_passage <- function(passage) {
  raw <- ellmer_chat_text_safe(paste0(PROMPT_COMPARATIVE, passage))
  if (is.null(raw)) return(NULL)
  parsed <- parse_json_or_null(raw)
  if (is.null(parsed) || is.null(parsed$instruction) || is.null(parsed$response)) return(NULL)
  tibble(instruction = parsed$instruction, response = parsed$response)
}

generate_grounded_alt_for_passage <- function(passage) {
  raw <- ellmer_chat_text_safe(paste0(PROMPT_GROUNDED_ALT, passage))
  if (is.null(raw)) return(NULL)
  parsed <- parse_json_or_null(raw)
  if (is.null(parsed) || is.null(parsed$examples)) return(NULL)
  ex <- tryCatch(as_tibble(parsed$examples), error = function(e) NULL)
  if (is.null(ex)) return(NULL)
  if (!all(c("instruction", "response") %in% names(ex))) return(NULL)
  if (nrow(ex) != 3) return(NULL)
  ex
}

# ===========================================================================
# MAIN
# ===========================================================================

cat("========================================\n")
cat("Crash-proof instruction generator\n")
cat("========================================\n\n")

# Load gold passages
gold <- read_csv(GOLD_IN, show_col_types = FALSE)
stopifnot(nrow(gold) > 0)
cat("Gold passages loaded:", nrow(gold), "\n")

# Initialise output files
init_meta_csv(PARTIAL_META)

# Load resume state
done_keys <- load_done_keys()
cat("Already completed passage x type combinations:", length(done_keys), "\n\n")

# Counters
counts <- list(modern_ok = 0, modern_skip = 0, modern_fail = 0,
               comp_ok = 0, comp_skip = 0, comp_fail = 0,
               grounded_ok = 0, grounded_skip = 0, grounded_fail = 0)

# ---- sanity check ----
cat("Running sanity check on first passage...\n")
test <- ellmer_chat_text_safe(paste0(PROMPT_MODERN, gold$passage[[1]]))
if (is.null(test)) {
  stop("Sanity check failed: could not reach API. Check ANTHROPIC_API_KEY.")
}
cat("  API connection: OK\n\n")

# ---- 1) Modern synthesis ----
cat("--- MODERN SYNTHESIS ---\n")
for (i in seq_len(nrow(gold))) {
  sha <- gold$passage_sha256[[i]]
  
  if (is_done(sha, "modern_synthesis", done_keys)) {
    counts$modern_skip <- counts$modern_skip + 1
    next
  }
  
  if (i %% 25 == 0 || i == 1) {
    cat(sprintf("  [%d/%d] modern | ok:%d skip:%d fail:%d\n",
                i, nrow(gold), counts$modern_ok, counts$modern_skip, counts$modern_fail))
  }
  
  ex <- generate_modern_for_passage(gold$passage[[i]])
  
  if (is.null(ex)) {
    counts$modern_fail <- counts$modern_fail + 1
    next
  }
  
  append_example(
    instruction    = ex$instruction[[1]],
    response       = ex$response[[1]],
    passage_sha256 = sha,
    stratum        = gold$stratum[[i]],
    source_file    = gold$source_file[[i]],
    chunk_index    = gold$chunk_index[[i]],
    example_type   = "modern_synthesis",
    tag            = "modern_application"
  )
  counts$modern_ok <- counts$modern_ok + 1
}
cat(sprintf("  Modern done: ok=%d, skip=%d, fail=%d\n\n",
            counts$modern_ok, counts$modern_skip, counts$modern_fail))

# ---- 2) Comparative ----
cat("--- COMPARATIVE ---\n")
for (i in seq_len(nrow(gold))) {
  sha <- gold$passage_sha256[[i]]
  
  if (is_done(sha, "comparative", done_keys)) {
    counts$comp_skip <- counts$comp_skip + 1
    next
  }
  
  if (i %% 25 == 0 || i == 1) {
    cat(sprintf("  [%d/%d] comparative | ok:%d skip:%d fail:%d\n",
                i, nrow(gold), counts$comp_ok, counts$comp_skip, counts$comp_fail))
  }
  
  ex <- generate_comparative_for_passage(gold$passage[[i]])
  
  if (is.null(ex)) {
    counts$comp_fail <- counts$comp_fail + 1
    next
  }
  
  append_example(
    instruction    = ex$instruction[[1]],
    response       = ex$response[[1]],
    passage_sha256 = sha,
    stratum        = gold$stratum[[i]],
    source_file    = gold$source_file[[i]],
    chunk_index    = gold$chunk_index[[i]],
    example_type   = "comparative",
    tag            = "cross_work_connection"
  )
  counts$comp_ok <- counts$comp_ok + 1
}
cat(sprintf("  Comparative done: ok=%d, skip=%d, fail=%d\n\n",
            counts$comp_ok, counts$comp_skip, counts$comp_fail))

# ---- 3) Contrastive (programmatic — no API calls) ----
cat("--- CONTRASTIVE (programmatic) ---\n")

# Check how many contrastive we already have from partial output
existing_contrastive <- sum(grepl("contrastive", done_keys))
needed <- max(0, N_NEW_CONTRASTIVE - existing_contrastive)

if (needed > 0) {
  contrastive_pool <- gold %>%
    transmute(passage_sha256, stratum, source_file, chunk_index, passage) %>%
    mutate(examples = map(passage, make_contrastive_extended)) %>%
    tidyr::unnest(examples) %>%
    mutate(example_type = "contrastive")
  
  set.seed(42)
  contrastive_sample <- contrastive_pool %>%
    sample_n(size = min(needed, nrow(contrastive_pool)))
  
  for (j in seq_len(nrow(contrastive_sample))) {
    row <- contrastive_sample[j, ]
    append_example(
      instruction    = row$instruction,
      response       = row$response,
      passage_sha256 = row$passage_sha256,
      stratum        = row$stratum,
      source_file    = row$source_file,
      chunk_index    = as.character(row$chunk_index),
      example_type   = "contrastive",
      tag            = row$tag
    )
  }
  cat("  Wrote", nrow(contrastive_sample), "contrastive examples\n\n")
} else {
  cat("  Contrastive already complete (", existing_contrastive, " exist)\n\n")
}

# ---- 4) Optional: extra grounded (paraphrase style) ----
if (isTRUE(DO_EXTRA_GROUNDED)) {
  cat("--- GROUNDED ALT (paraphrase) ---\n")
  for (i in seq_len(nrow(gold))) {
    sha <- gold$passage_sha256[[i]]
    
    if (is_done(sha, "grounded_paraphrase", done_keys)) {
      counts$grounded_skip <- counts$grounded_skip + 1
      next
    }
    
    if (i %% 25 == 0 || i == 1) {
      cat(sprintf("  [%d/%d] grounded_alt | ok:%d skip:%d fail:%d\n",
                  i, nrow(gold), counts$grounded_ok, counts$grounded_skip,
                  counts$grounded_fail))
    }
    
    ex <- generate_grounded_alt_for_passage(gold$passage[[i]])
    
    if (is.null(ex)) {
      counts$grounded_fail <- counts$grounded_fail + 1
      next
    }
    
    for (k in seq_len(nrow(ex))) {
      append_example(
        instruction    = ex$instruction[[k]],
        response       = ex$response[[k]],
        passage_sha256 = sha,
        stratum        = gold$stratum[[i]],
        source_file    = gold$source_file[[i]],
        chunk_index    = gold$chunk_index[[i]],
        example_type   = "grounded_paraphrase",
        tag            = NA_character_
      )
    }
    counts$grounded_ok <- counts$grounded_ok + 1
  }
  cat(sprintf("  Grounded alt done: ok=%d, skip=%d, fail=%d\n\n",
              counts$grounded_ok, counts$grounded_skip, counts$grounded_fail))
}

# ---- Summary ----
cat("\n", strrep("=", 60), "\n")
cat("GENERATION COMPLETE\n")
cat(strrep("=", 60), "\n")

# Count what's in the partial output
new_lines <- if (file_exists(PARTIAL_JSONL)) {
  length(readLines(PARTIAL_JSONL, warn = FALSE))
} else { 0 }
cat("New examples in partial output: ", new_lines, "\n", sep = "")
cat("  File: ", PARTIAL_JSONL, "\n", sep = "")
cat("  Meta: ", PARTIAL_META, "\n", sep = "")

# ---- Merge with existing instructions.jsonl ----
existing_jsonl_path <- "instruction_data/instructions.jsonl"
if (file_exists(existing_jsonl_path) && file_exists(PARTIAL_JSONL)) {
  existing_lines <- readLines(existing_jsonl_path, warn = FALSE)
  partial_lines  <- readLines(PARTIAL_JSONL, warn = FALSE)
  
  # Deduplicate: hash each JSONL line
  all_lines <- c(existing_lines, partial_lines)
  deduped   <- all_lines[!duplicated(all_lines)]
  
  writeLines(deduped, MERGED_JSONL)
  cat("\nMerged output: ", MERGED_JSONL, "\n", sep = "")
  cat("  Existing:      ", length(existing_lines), "\n", sep = "")
  cat("  New (partial):  ", length(partial_lines), "\n", sep = "")
  cat("  Duplicates:     ", length(all_lines) - length(deduped), "\n", sep = "")
  cat("  Total unique:   ", length(deduped), "\n", sep = "")
  cat("\nUse this for training:\n")
  cat("  --data ", MERGED_JSONL, "\n", sep = "")
} else if (file_exists(PARTIAL_JSONL)) {
  cat("\nNo existing instructions.jsonl found.\n")
  cat("Use the partial file directly: ", PARTIAL_JSONL, "\n", sep = "")
} else {
  cat("\nNo output generated. Check errors above.\n")
}

cat("\nDone.\n")