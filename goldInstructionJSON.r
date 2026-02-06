# Creates a pilot instruction dataset from gold passages:
# - 1,485 grounded examples (ELLMer, 3 per passage)
# - 500 contrastive examples (programmatic)
# - 100 modern synthesis examples (ELLMer)
# Outputs: instruction_data/instructions.jsonl (instruction/response only)
# Also outputs: instruction_data/instruction_metadata.csv (audit + provenance)

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(purrr); library(tibble); library(stringr)
  library(jsonlite); library(fs); library(glue)
})

# ---- config ----
GOLD_IN <- "instruction_data/gold_manifest.csv"
OUT_JSONL <- "instruction_data/instructions.jsonl"
OUT_META  <- "instruction_data/instructions_metadata.csv"

dir_create(path_dir(OUT_JSONL))

# ---- ELLMer setup ----
# Install/attach ellmer before running:
# install.packages("ellmer") or remotes::install_github(...) depending on your setup
suppressPackageStartupMessages({
  library(ellmer)
})


MODEL_NAME <- "claude-opus-4-5-20251101"  # Set 02-04-2026
#API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")
#Ellmer will automagically pick up on the ANTHROPIC_API_KEY but the above line
#is just a reminder of that.

# ---- helpers ----
write_jsonl <- function(df, path) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  
  for (i in seq_len(nrow(df))) {
    line <- toJSON(list(
      instruction = df$instruction[[i]],
      response    = df$response[[i]]
    ), auto_unbox = TRUE)
    writeLines(line, con = con, sep = "\n", useBytes = TRUE)
  }
}

# Build a standardized instruction wrapper so the model always sees passage context.
wrap_task <- function(passage, task) {
  paste0(
    "PASSAGE:\n", passage, "\n\n",
    "TASK:\n", task
  )
}

# Parse JSON safely
strip_fences <- function(x) {
  x <- stringr::str_trim(x)
  x <- stringr::str_replace(x, "^```(?:json)?\\s*", "")
  x <- stringr::str_replace(x, "\\s*```\\s*$", "")
  x
}

escape_newlines_in_strings <- function(x) {
  # Walk the text and replace literal newlines only when we're inside "..."
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  out <- character(0)
  
  in_str <- FALSE
  esc <- FALSE
  
  for (ch in chars) {
    if (esc) {
      out <- c(out, ch)
      esc <- FALSE
      next
    }
    
    if (ch == "\\") {
      out <- c(out, ch)
      esc <- TRUE
      next
    }
    
    if (ch == "\"") {
      in_str <- !in_str
      out <- c(out, ch)
      next
    }
    
    if (in_str && ch == "\n") {
      out <- c(out, "\\n")
    } else if (in_str && ch == "\r") {
      # drop CR
    } else {
      out <- c(out, ch)
    }
  }
  
  paste0(out, collapse = "")
}

parse_json_or_null <- function(x) {
  tryCatch(
    jsonlite::fromJSON(escape_newlines_in_strings(strip_fences(x))),
    error = function(e) NULL
  )
}
# ---- prompts ----
PROMPT_GROUNDED <- "
You are generating training examples for a model named Krobotkin.

Given the passage below, create EXACTLY 3 instruction–response pairs that can be answered using ONLY the passage.
Requirements:
- Each instruction must explicitly say it should use only the passage.
- The response must not add outside facts, names, dates, or citations not present in the passage.
- Keep responses concise but accurate.
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

PROMPT_MODERN <- "
You are generating training examples for a model named Krobotkin.

Given the passage below:
- Write ONE instruction that asks to apply the passage’s ideas to a modern topic (e.g., conservation, restoration, climate adaptation, community governance).
- Write ONE response that connects the passage’s ideas to the modern topic.

Rules:
- You MAY use modern concepts, but do NOT claim the passage mentions modern organizations, laws, technologies, or statistics.
- No external citations.
Return ONLY valid JSON in this schema:
{\"instruction\":\"...\",\"response\":\"...\"}


Return minified JSON (single line), no code fences, and no literal line breaks inside strings (use \\n if needed).

PASSAGE:
"

# ---- ELLMer callers ----
ellmer_chat_text <- function(prompt_text) {
  
  chat <- chat_anthropic(
    model = MODEL_NAME,
    params = params(temperature = 0.1),
  )
  chat$chat(prompt_text)
}

generate_grounded_for_passage <- function(passage) {
  raw <- ellmer_chat_text(paste0(PROMPT_GROUNDED, passage))
  parsed <- parse_json_or_null(raw)
  
  if (is.null(parsed) || is.null(parsed$examples)) return(NULL)
  
  ex <- as_tibble(parsed$examples)
  if (!all(c("instruction","response") %in% names(ex))) return(NULL)
  if (nrow(ex) != 3) return(NULL)
  
  ex
}

generate_modern_for_passage <- function(passage) {
  raw <- ellmer_chat_text(paste0(PROMPT_MODERN, passage))
  parsed <- parse_json_or_null(raw)
  if (is.null(parsed) || is.null(parsed$instruction) || is.null(parsed$response)) return(NULL)
  
  tibble(instruction = parsed$instruction, response = parsed$response)
}

# ---- contrastive generators (programmatic) ----
NOT_SUPPORTED <- "Not supported by the passage."
NO_EXTERNAL   <- "I can only use the provided passage and cannot add external facts, citations, or verification."

make_contrastive <- function(passage) {
  # We wrap tasks with PASSAGE + TASK so the model learns the conditioning.
  # These tasks are designed to be generally NOT answerable from arbitrary passages.
  tribble(
    ~instruction, ~response, ~tag,
    wrap_task(passage, "Using only the passage, name three modern conservation organizations that apply these ideas."), NOT_SUPPORTED, "not_in_passage",
    wrap_task(passage, "Using only the passage, give two exact page numbers and the publisher of the edition this came from."), NOT_SUPPORTED, "not_in_passage",
    wrap_task(passage, "Using only the passage, provide three APA citations from peer-reviewed articles that support the claims."), NO_EXTERNAL, "external_citations",
    wrap_task(passage, "Using only the passage, verify the historical facts using Wikipedia and include links."), NO_EXTERNAL, "external_citations",
    wrap_task(passage, "Using only the passage, summarize what it says about smartphones and social media."), NOT_SUPPORTED, "anachronism"
  )
}

# ---- load gold passages ----
gold <- read_csv(GOLD_IN, show_col_types = FALSE)
stopifnot(all(c("passage_sha256","stratum","passage","source_file","chunk_index") %in% names(gold)))
stopifnot(nrow(gold) == 495)  # pilot expects 495; relax if needed

# ---- sanity check: one call before the full run ----
sanity_check_grounded <- function(passage) {
  raw <- ellmer_chat_text(paste0(PROMPT_GROUNDED, passage))
  
  parsed <- parse_json_or_null(raw)
  ok <- !is.null(parsed) &&
    !is.null(parsed$examples) &&
    is.data.frame(parsed$examples) &&
    all(c("instruction","response") %in% names(parsed$examples)) &&
    nrow(parsed$examples) == 3
  
  if (!ok) {
    cat("\n--- SANITY CHECK FAILED ---\n")
    cat("Raw (first 1200 chars):\n")
    cat(substr(raw, 1, 1200), "\n")
    stop("Sanity check failed: model output did not parse to 3 {instruction,response} examples. Fix parsing/prompt before continuing.")
  }
  
  message("Sanity check passed: got 3 grounded examples.")
  invisible(TRUE)
}

sanity_check_grounded(gold$passage[[1]])

# ---- 1) Grounded (ELLMer): 3 per passage -> 1,485 ----
set.seed(1)

grounded_list <- vector("list", nrow(gold))
for (i in seq_len(nrow(gold))) {
  ex <- generate_grounded_for_passage(gold$passage[[i]])
  grounded_list[[i]] <- ex
}

grounded_df <- bind_rows(
  lapply(seq_len(nrow(gold)), function(i) {
    ex <- grounded_list[[i]]
    if (is.null(ex)) return(NULL)
    ex %>%
      mutate(
        passage_sha256 = gold$passage_sha256[[i]],
        stratum = gold$stratum[[i]],
        source_file = gold$source_file[[i]],
        chunk_index = gold$chunk_index[[i]],
        example_type = "grounded"
      )
  })
)

# ---- 2) Contrastive (programmatic): sample to 150 total ----
contrastive_pool <- gold %>%
  transmute(
    passage_sha256, stratum, source_file, chunk_index, passage
  ) %>%
  mutate(examples = map(passage, make_contrastive)) %>%
  tidyr::unnest(examples) %>%
  mutate(example_type = "contrastive")

contrastive_df <- contrastive_pool %>%
  sample_n(size = min(150, nrow(contrastive_pool)))

# ---- 3) Modern synthesis (ELLMer): 30 passages (10 per stratum) ----
modern_pick <- gold %>%
  group_by(stratum) %>%
  arrange(runif(n())) %>%          # randomize within stratum
  slice_head(n = 10) %>%
  ungroup()

modern_rows <- vector("list", nrow(modern_pick))
for (i in seq_len(nrow(modern_pick))) {
  ex <- generate_modern_for_passage(modern_pick$passage[[i]])
  modern_rows[[i]] <- ex
}

modern_df <- bind_rows(
  lapply(seq_len(nrow(modern_pick)), function(i) {
    ex <- modern_rows[[i]]
    if (is.null(ex)) return(NULL)
    ex %>%
      mutate(
        passage_sha256 = modern_pick$passage_sha256[[i]],
        stratum = modern_pick$stratum[[i]],
        source_file = modern_pick$source_file[[i]],
        chunk_index = modern_pick$chunk_index[[i]],
        example_type = "modern_synthesis",
        tag = "modern_application"
      )
  })
)

# ---- combine + finalize ----
positive_df <- grounded_df %>%
  transmute(
    instruction = instruction, response = response,
    passage_sha256, stratum, source_file, chunk_index,
    example_type, tag = NA_character_
  )

contrastive_out <- contrastive_df %>%
  transmute(
    instruction, response,
    passage_sha256, stratum, source_file, chunk_index,
    example_type, tag
  )

modern_out <- modern_df %>%
  transmute(
    instruction = instruction, response = response,
    passage_sha256, stratum, source_file, chunk_index,
    example_type, tag
  )

all_meta <- bind_rows(positive_df, contrastive_out, modern_out) %>%
  mutate(example_id = paste0("ex_", row_number()))

# Write JSONL (instruction/response only)
write_jsonl(all_meta %>% select(instruction, response), OUT_JSONL)

# Write metadata CSV (for auditing)
write_csv(all_meta, OUT_META)

cat("Wrote: ", OUT_JSONL, " (", nrow(all_meta), " examples)\n", sep = "")
cat("Wrote: ", OUT_META,  "\n", sep = "")
cat("Breakdown:\n")
print(all_meta %>% count(example_type, stratum))