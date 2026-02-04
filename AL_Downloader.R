# AL_Downloader.R
# Simple script downloads text files from the Anarchist Library.
# Works with compileCorpus.R

library(tidyverse)
library(fs)
library(httr2)
library(stringr)
library(readr)

# ---- config ----
RAW_DIR <- "data/raw/al_downloads"   # change if you like
dir_create(RAW_DIR)

# Paste your Anarchist Library links here (work pages or .muse links)
AL_LINKS <- c(
  "https://theanarchistlibrary.org/library/petr-kropotkin-letter-to-lenin-4-march-1920.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-letter-to-lenin.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-on-order.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-organised-vengeance-called-justice.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-peter-kropotkin-s-last-letter.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-prisons-universities-of-crime.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-process-under-socialism.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-proposed-communist-settlement-a-new-colony-for-tyneside-or-wearside.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-russian-revolution-and-the-soviet-government-letter-to-the-workers-of-wester.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-war.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-crisis-of-socialism.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-emile-pouget-louise-michel-declaration-to-the-tribunal-of-lyons-by-the-accused-a.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-permanence-of-society-after-the-revolution.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-collapse-of-counter-revolutionary-socialism.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-open-letter-to-western-european-workers.muse",
  "https://theanarchistlibrary.org/library/prisons-and-their-moral-influence-on-prisoners.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-anti-militarism-was-it-properly-understood.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-police-cannot-be-the-builders-of-a-new-life.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-letter-to-the-bakunin-centenary-celebration.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-why-should-i-be-moral.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-act-for-yourselves.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-practical-questions.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-use-of-the-strike.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-what-a-strike-is.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-will-the-revolution-be-collectivist.muse",
  "https://theanarchistlibrary.org/library/an-open-letter-of-peter-kropotkin-to-the-western-workingmen.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-municipal-socialism.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-kropotkin-s-definition-of-anarchism-for-the-encyclopedia-britannica.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-a-letter-on-the-present-war.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-communism-and-the-wage-system.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-syndicalism-and-anarchism.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-maxim-gorky.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-spirit-of-revolt.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-christmas-in-prison.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-politics-and-socialism.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-in-memory-of-william-morris.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-war-or-peace.muse",
  "https://theanarchistlibrary.org/library/kropotkin-anarchists-and-trade-unions.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-anarchist-idea-and-its-developments.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-finland-a-rising-nationality.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-communism-and-anarchy.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-revolutionary-government.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-to-max-nettlau.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-commune-of-paris.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-coming-war.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-workers-organisation.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-russian-school-and-the-holy-synod.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-wars-and-capitalism.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-scientific-basis-of-anarchy.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-place-of-anarchism-in-socialistic-evolution.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-wage-system.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-anarchists-and-unions.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-on-the-teaching-of-physiography.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-new-era.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-anarchism-from-the-encyclopaedia-britannica.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-present-crisis-in-russia.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-great-french-revolution-and-its-lesson.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-law-and-authority.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-what-geography-ought-to-be.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-industrial-village.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-an-appeal-to-the-young.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-constitutional-agitation-in-russia2.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-direct-action-of-environment-and-evolution.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-some-of-the-resources-of-canada.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-brain-work-and-manual-work.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-small-communal-experiments-and-why-they-fail.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-modern-state.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-state-its-historic-role.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-terror-in-russia.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-anarchism-its-philosophy-and-ideal.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-revolutionary-studies.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-anarchist-communism-its-basis-and-principles.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-the-great-french-revolution-1789-1793.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-ideals-and-realities-in-russian-literature.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-ethics-origin-and-development.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-fields-factories-and-workshops-or-industry-combined-with-agriculture-and-brain-w.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-words-of-a-rebel-1.muse",
  "https://theanarchistlibrary.org/library/petr-kropotkin-in-russian-and-french-prisons.muse"
)

# ---- helpers ----

# Ensure we have a direct .muse text URL
as_muse_url <- function(url) {
  url <- str_trim(url)
  if (!str_detect(url, "\\.muse$")) paste0(url, ".muse") else url
}

# Build a decent ID from the slug
make_id <- function(muse_url) {
  slug <- muse_url |>
    str_remove("\\.muse$") |>
    str_extract("[^/]+$") |>
    str_replace_all("[^a-zA-Z0-9]+", "_") |>
    str_to_lower()
  
  paste0("al_", slug)
}

# Guess author from the path; special-case Kropotkin
guess_author <- function(muse_url) {
  # e.g. /library/petr-kropotkin-...
  path_bit <- str_extract(muse_url, "/library/[^.]+") %||% ""
  if (str_detect(path_bit, "petr-kropotkin")) return("Peter Kropotkin")
  
  # fallback: take first hyphenated “name” chunk before the title slug
  # (not perfect, but keeps it simple)
  "Unknown"
}

# Guess a readable title from the slug (fallback only)
guess_title <- function(muse_url) {
  slug <- muse_url |>
    str_remove("\\.muse$") |>
    str_extract("[^/]+$") |>
    str_remove("^petr-kropotkin-") |>
    str_replace_all("-", " ") |>
    str_squish()
  
  str_to_title(slug)
}

# Pick a filename for RAW_DIR
make_filename <- function(title) {
  title |>
    str_replace_all("[^A-Za-z0-9]+", "_") |>
    str_replace_all("_+", "_") |>
    str_remove("^_|_$") |>
    paste0(".txt")
}

download_muse <- function(muse_url, dest_path) {
  req <- request(muse_url) |>
    req_user_agent("krobotkin-downloader/0.1 (R httr2)") |>
    req_timeout(60)
  
  resp <- req_perform(req)
  
  if (resp_status(resp) >= 400) {
    stop("Download failed: ", muse_url, " (HTTP ", resp_status(resp), ")", call. = FALSE)
  }
  
  txt <- resp_body_string(resp)
  write_lines(txt, dest_path)
  invisible(dest_path)
}

# ---- main ----

build_anarchist_library_sources <- function(links,
                                            raw_dir = RAW_DIR,
                                            genre = "unknown",
                                            year = NA_integer_) {
  dir_create(raw_dir)
  
  tibble(source_url = links) |>
    mutate(
      source_url  = map_chr(source_url, as_muse_url),
      id          = map_chr(source_url, make_id),
      author      = map_chr(source_url, guess_author),
      title       = map_chr(source_url, guess_title),
      year        = year,
      genre       = genre,
      source_type = "local_txt",
      source_path = map_chr(title, make_filename),
      note        = "Anarchist Library download (local file)"
    ) |>
    mutate(
      downloaded = pmap_lgl(list(source_url, source_path), function(u, p) {
        if (file_exists(p)) return(TRUE)  # don't redownload
        message("Downloading: ", u)
        download_muse(u, p)
        Sys.sleep(1)  # be polite
        TRUE
      })
    ) |>
    select(id, title, year, author, genre, source_type, source_path, source_url, note)
}

# Build new rows and print them
NEW_SOURCES <- build_anarchist_library_sources(AL_LINKS)
SOURCES <- readr::read_csv("data/sources.csv")
SOURCES <- rbind(SOURCES, NEW_SOURCES)
write.csv(SOURCES, "data/updated_sources.csv")