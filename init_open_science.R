# init_open_science.R
# =============================================================================
# PURPOSE:
#   One-time script to create the initial open_science.yml file by reading
#   the open science information already stored in your Zotero BibTeX export
#   (MyPubBetterBibTex.bib). The NOTE field in each Zotero entry is parsed
#   to extract collaboration type, open data/code URLs, preregistration URLs,
#   and preprint flags.
#
#   After running this script once, open_science.yml will be maintained
#   automatically by update_pubs.R whenever a new publication is detected.
#
# HOW TO RUN:
#   1. Make sure MyPubBetterBibTex.bib is up to date in the website folder
#      (export from Zotero with Better BibTeX, checking "Export Notes")
#   2. Open this file in RStudio and click "Source"
#   3. Review the generated open_science.yml for any parsing errors
#
# REQUIRED PACKAGES:
#   bib2df, yaml
#   Install with: install.packages(c("bib2df", "yaml"))
# =============================================================================

library(bib2df)   # for reading the BibTeX file
library(yaml)     # for writing the YAML output

# ── Load configuration ────────────────────────────────────────────────────────

cfg       <- yaml.load_file("config.yml")
SITE_DIR  <- cfg$site_dir
BIB_FILE  <- file.path(SITE_DIR, "MyPubBetterBibTex.bib")
YAML_FILE <- file.path(SITE_DIR, "open_science.yml")

# ── NOTE field parser ─────────────────────────────────────────────────────────
# The NOTE field in Zotero uses semicolon-separated keywords and key:value pairs.
#
# Recognised patterns:
#   laboratory            → adds "laboratory" to collaboration list
#   international         → adds "international" to collaboration list
#   KULeuven              → adds "KULeuven" to collaboration list
#   preprint              → preprint: true
#   psyarxiv              → psyarxiv: true
#   data and code: URL    → open_data and open_code both set to URL
#   code: URL             → open_code: URL
#   data: URL             → open_data: URL
#   preregistration: URL  → preregistration: URL
#
# If no collaboration keyword is found, "laboratory" is used as the default.

parse_bib_note <- function(note) {
  result <- list(
    open_data       = "",
    open_code       = "",
    preregistration = "",
    preprint        = FALSE,
    psyarxiv        = FALSE,
    collaboration   = list("laboratory")  # default when no keyword is found
  )

  if (is.na(note) || !nzchar(trimws(note))) return(result)

  # Split the note by semicolons and remove empty parts
  parts <- trimws(strsplit(note, ";")[[1]])
  parts <- parts[nzchar(parts)]

  collab <- character()

  for (part in parts) {
    part_low <- tolower(trimws(part))

    if (part_low == "laboratory") {
      collab <- c(collab, "laboratory")

    } else if (part_low == "international") {
      collab <- c(collab, "international")

    } else if (part_low == "kuleuven") {
      collab <- c(collab, "KULeuven")

    } else if (part_low == "preprint") {
      result$preprint <- TRUE

    } else if (part_low == "psyarxiv") {
      result$psyarxiv <- TRUE

    } else if (grepl("^data and code:", part_low)) {
      # "data and code: URL" means both open data and open code share the same URL
      url <- trimws(sub("(?i)^data and code:\\s*", "", part))
      result$open_data <- url
      result$open_code <- url

    } else if (grepl("^code:", part_low)) {
      result$open_code <- trimws(sub("(?i)^code:\\s*", "", part))

    } else if (grepl("^data:", part_low)) {
      result$open_data <- trimws(sub("(?i)^data:\\s*", "", part))

    } else if (grepl("^preregistration:", part_low)) {
      result$preregistration <- trimws(sub("(?i)^preregistration:\\s*", "", part))
    }
  }

  # Store collaboration as a list so YAML supports multiple values
  # e.g. a paper can be both "laboratory" and "international"
  if (length(collab) > 0) result$collaboration <- as.list(collab)

  result
}

# ── Read BibTeX file ──────────────────────────────────────────────────────────

message("Reading BibTeX file: ", BIB_FILE)
bib_df <- bib2df(BIB_FILE)
message(sprintf("  Found %d entries in total.", nrow(bib_df)))

# Keep only entries that have a DOI — entries without a DOI cannot be
# matched against CrossRef or ORCID and would be orphaned in the yml file
bib_df <- bib_df[!is.na(bib_df$DOI) & nzchar(trimws(bib_df$DOI)), ]
message(sprintf("  %d entries have a DOI and will be included.", nrow(bib_df)))

# Sort by year descending so the yml file matches the order of the website
bib_df <- bib_df[order(bib_df$YEAR, decreasing = TRUE), ]

# ── YAML file header ──────────────────────────────────────────────────────────
# This header is written at the top of open_science.yml as documentation
# so that you know how to fill in or edit entries in the future.

YAML_HEADER <- c(
  "# open_science.yml — Open science metadata for publications",
  "# ──────────────────────────────────────────────────────────────────────",
  "# HOW TO FILL IN THIS FILE:",
  "#   Each entry corresponds to one publication, identified by its DOI.",
  "#   New entries are added automatically at the top of this file when",
  "#   update_pubs.R detects a new publication on ORCID.",
  "#",
  "# FIELDS:",
  "#   doi:             DOI of the publication — do not edit this line",
  "#   open_data:       URL to public dataset (e.g. https://osf.io/xxxxx)",
  "#                    Leave blank if data is not publicly available.",
  "#   open_code:       URL to public code (e.g. https://github.com/...)",
  "#                    Leave blank if code is not publicly available.",
  "#   preregistration: URL to preregistration (e.g. https://osf.io/xxxxx)",
  "#                    Leave blank if the study was not preregistered.",
  "#   preprint:        true  → a bioRxiv preprint exists for this paper",
  "#                    false → no bioRxiv preprint",
  "#   psyarxiv:        true  → a psyarXiv preprint exists for this paper",
  "#                    false → no psyarXiv preprint",
  "#   collaboration:   A list with one or more of the following values:",
  "#     - laboratory    → displayed as: 'work from the laboratory'",
  "#     - KULeuven      → displayed as: 'work with colleagues from KU Leuven'",
  "#     - international → displayed as: 'work with Belgian/international colleagues'",
  "#   Multiple values are allowed, e.g.: [laboratory, international]"
)

# ── Build and write open_science.yml ─────────────────────────────────────────

# Helper: returns a clean title string for use as a YAML comment.
# bib2df sometimes returns NA (unparseable entry) or titles with leftover
# LaTeX curly braces (e.g. "Motor {Cortex} Activity"). Both are handled here.
clean_title <- function(title) {
  if (is.na(title) || !nzchar(trimws(title))) return(NA_character_)
  trimws(gsub("[{}]", "", title))  # strip LaTeX curly braces
}

message("Parsing notes and writing open_science.yml...")
lines <- c(YAML_HEADER, "")

for (i in seq_len(nrow(bib_df))) {
  doi   <- tolower(trimws(bib_df$DOI[i]))
  title <- clean_title(bib_df$TITLE[i])
  year  <- bib_df$YEAR[i]
  note  <- bib_df$NOTE[i]

  # Write the paper title as a comment above the entry so you can identify
  # which publication you are reading without having to look up the DOI.
  # If the title could not be read from the BibTeX file, the DOI is used
  # as a fallback so the entry always has some identifying comment.
  comment <- if (!is.na(title)) {
    paste0("# ", title, " (", year, ")")
  } else {
    paste0("# (title not found in BibTeX — DOI: ", doi, ")")
  }
  lines <- c(lines, comment)

  os    <- parse_bib_note(note)
  entry <- list(list(
    doi             = doi,
    open_data       = os$open_data,
    open_code       = os$open_code,
    preregistration = os$preregistration,
    preprint        = os$preprint,
    psyarxiv        = os$psyarxiv,
    collaboration   = os$collaboration
  ))

  lines <- c(lines, as.yaml(entry), "")
}

writeLines(lines, YAML_FILE)
message(sprintf("Done! open_science.yml created with %d entries.", nrow(bib_df)))
message("Please review the file and correct any parsing errors before running update_pubs.R.")
