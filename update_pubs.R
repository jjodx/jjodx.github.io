# update_pubs.R
# =============================================================================
# PURPOSE:
#   This script automates updating the publications page of the website.
#   It replaces the manual Zotero BibTeX export step.
#
# WHAT IT DOES (in order):
#   1. Reads configuration from config.yml
#   2. Fetches your list of publications from ORCID (using your ORCID ID)
#   3. For each publication, fetches full details (title, authors, journal,
#      year, volume, pages) from CrossRef using the DOI
#   4. Saves this data locally in pubs_cache.rds (used by Pubs.Rmd to render)
#   5. Compares the fetched publications against open_science.yml:
#        - If open_science.yml does not exist yet: run init_open_science.R first
#        - If it exists: for each NEW publication not yet in the yml, a small
#          popup window lets you fill in the open science details immediately
#   6. Reports any publications missing a PDF in PubFile/ and tells you
#      exactly what filename to use for each one
#   7. Re-renders the entire website
#
# HOW TO RUN:
#   Open this file in RStudio and click "Source", or press Ctrl+Shift+Enter.
#   Run init_open_science.R first if open_science.yml does not exist yet.
#
# REQUIRED PACKAGES:
#   httr, jsonlite, yaml, shiny, miniUI
#   Install with: install.packages(c("httr","jsonlite","yaml","shiny","miniUI"))
# =============================================================================

library(httr)       # for making HTTP requests to ORCID and CrossRef APIs
library(jsonlite)   # for parsing the JSON responses from the APIs
library(yaml)       # for reading config.yml and open_science.yml
library(shiny)      # for the popup window when a new publication is detected
library(miniUI)     # for the compact layout of the popup window

# ── 1. Load configuration ─────────────────────────────────────────────────────
# All user-specific settings are stored in config.yml so this script
# does not need to be edited when moving to a different machine.

cfg         <- yaml.load_file("config.yml")
ORCID_ID    <- cfg$orcid_id
EMAIL       <- cfg$contact_email
SITE_DIR    <- cfg$site_dir
LAB_HEAD    <- cfg$lab_head_name
PDF_BASE    <- cfg$pdf_base_url
PUBFILE_DIR <- cfg$pubfile_dir

# Paths derived from SITE_DIR
YAML_FILE  <- file.path(SITE_DIR, "open_science.yml")
CACHE_FILE <- file.path(SITE_DIR, "pubs_cache.rds")

# Safety check: open_science.yml must exist before this script can run.
# If it does not exist, the user should run init_open_science.R first.
if (!file.exists(YAML_FILE)) {
  stop(
    "open_science.yml not found. ",
    "Please run init_open_science.R first to create it from your BibTeX file."
  )
}

# ── 2. ORCID API ──────────────────────────────────────────────────────────────
# ORCID is a registry of researcher identifiers and their works.
# The public API (no login required) returns a list of works associated
# with a given ORCID ID, including their DOIs.

# Helper: extracts DOI values from an ORCID "external-ids" object.
# ORCID stores identifiers (DOI, PMID, etc.) in a nested list structure;
# this function filters to DOIs only and returns them as lowercase strings.
extract_dois_from_ids <- function(external_ids) {
  if (is.null(external_ids) || is.null(external_ids$`external-id`)) return(character())
  dois <- character()
  for (id in external_ids$`external-id`) {
    if (tolower(id$`external-id-type`) == "doi")
      dois <- c(dois, tolower(trimws(id$`external-id-value`)))
  }
  dois
}

# Fetches DOIs of all works from an ORCID profile.
# ORCID groups works that refer to the same publication together.
# We collect DOIs from all groups regardless of ORCID type, because ORCID
# type labels are unreliable (e.g. eLife reviewed preprints, Preprints.org
# entries, and some journal articles may be labelled "preprint" or other types).
# Filtering to journals/book-chapters is handled downstream by CrossRef types,
# which are more accurate.
# DOIs are stored at the group level; we fall back to the work-summary
# level if no DOIs are found at the group level (older API behaviour).
fetch_orcid_dois <- function(orcid_id) {
  url  <- paste0("https://pub.orcid.org/v3.0/", orcid_id, "/works")
  resp <- GET(url, add_headers(Accept = "application/json"))
  stop_for_status(resp)
  data <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  dois <- character()
  for (group in data$group) {
    group_dois <- extract_dois_from_ids(group$`external-ids`)
    if (length(group_dois) > 0) {
      dois <- c(dois, group_dois)
    } else {
      # Fallback: look inside each individual work summary
      for (s in group$`work-summary`)
        dois <- c(dois, extract_dois_from_ids(s$`external-ids`))
    }
  }
  unique(dois)  # remove duplicates (same paper referenced multiple times)
}

# ── 3. CrossRef API ───────────────────────────────────────────────────────────
# CrossRef is a DOI registration agency that provides rich bibliographic
# metadata for published works. Given a DOI, it returns title, authors,
# journal name, year, volume, and page numbers.
# Using the "polite pool" (by providing an email) gives faster responses.

# Helper: extracts the publication year from a CrossRef metadata object.
# CrossRef stores dates in multiple fields depending on the work type;
# we try them in order of reliability.
get_year_from_crossref <- function(msg) {
  for (field in c("published-print", "published-online", "created")) {
    parts <- msg[[field]]$`date-parts`
    if (!is.null(parts) && length(parts[[1]]) >= 1)
      return(as.integer(parts[[1]][[1]]))
  }
  NA_integer_
}

# Helper: formats a single CrossRef author object into "Family, Given" format.
# CrossRef sometimes has incomplete author records (e.g. organisations),
# so we handle missing given names gracefully.
format_crossref_author <- function(a) {
  if (!is.null(a$family) && !is.null(a$given)) paste0(a$family, ", ", a$given)
  else if (!is.null(a$family))                 a$family
  else if (!is.null(a$name))                   a$name
  else                                         ""
}

# Fetches full bibliographic metadata for a single DOI from CrossRef.
# Returns NULL if the DOI is not found or the request fails.
fetch_crossref_metadata <- function(doi) {
  # The DOI slash must not be percent-encoded in the URL path — CrossRef
  # expects the raw DOI (e.g. 10.1152/jn.00785.2012, not 10.1152%2Fjn.00785.2012)
  url  <- paste0("https://api.crossref.org/works/", doi)
  resp <- GET(url, query = list(mailto = EMAIL))
  if (status_code(resp) != 200) {
    message(paste("  CrossRef: no metadata for DOI:", doi))
    return(NULL)
  }
  msg <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)$message
  # For preprints (type = "posted-content"), container-title is often empty.
  # Fall back to institution name (e.g. "bioRxiv", "Preprints.org") then publisher.
  ct <- msg$`container-title`
  journal <- if (length(ct) > 0 && nzchar(ct[[1]])) {
    ct[[1]]
  } else if (!is.null(msg$institution) && length(msg$institution) > 0 &&
             !is.null(msg$institution[[1]]$name)) {
    msg$institution[[1]]$name
  } else if (!is.null(msg$publisher) && nzchar(msg$publisher)) {
    msg$publisher
  } else {
    NA_character_
  }

  list(
    doi     = doi,
    title   = if (length(msg$title) > 0)  msg$title[[1]]                             else NA_character_,
    authors = if (!is.null(msg$author))    sapply(msg$author, format_crossref_author) else character(),
    journal = journal,
    year    = get_year_from_crossref(msg),
    volume  = if (!is.null(msg$volume))    msg$volume                                 else NA_character_,
    pages   = if (!is.null(msg$page))      msg$page                                   else NA_character_,
    type    = if (!is.null(msg$type))      msg$type                                   else NA_character_
  )
}

# Fetches metadata from OpenAlex as a fallback when CrossRef has no record.
# OpenAlex is a free, open catalogue with broader coverage of older papers.
# Note: OpenAlex returns author names in "Given Family" order rather than
# "Family, Given" — this is acceptable as a fallback since author bolding
# checks for a substring match ("Orban de Xivry") regardless of name order.
fetch_openalex_metadata <- function(doi) {
  url  <- paste0("https://api.openalex.org/works/https://doi.org/", doi)
  resp <- GET(url, query = list(mailto = EMAIL))
  if (status_code(resp) != 200) return(NULL)
  msg  <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  pages <- if (!is.null(msg$biblio$first_page)) {
    if (!is.null(msg$biblio$last_page))
      paste0(msg$biblio$first_page, "-", msg$biblio$last_page)
    else
      msg$biblio$first_page
  } else NA_character_

  # Map OpenAlex type vocabulary to CrossRef equivalents so the type filter works.
  # OpenAlex uses "article" and "preprint"; CrossRef uses "journal-article" and "posted-content".
  # Other types (book-chapter, proceedings-article, etc.) are the same in both.
  oa_type <- if (!is.null(msg$type)) msg$type else NA_character_
  crossref_type <- switch(oa_type,
    "article"  = "journal-article",
    "preprint" = "posted-content",
    oa_type   # keep as-is for book-chapter, proceedings-article, etc.
  )

  list(
    doi     = doi,
    title   = if (!is.null(msg$title))                                 msg$title                                      else NA_character_,
    authors = if (!is.null(msg$authorships))                           sapply(msg$authorships, function(a) a$author$display_name) else character(),
    journal = if (!is.null(msg$primary_location$source$display_name))  msg$primary_location$source$display_name       else NA_character_,
    year    = if (!is.null(msg$publication_year))                      as.integer(msg$publication_year)               else NA_integer_,
    volume  = if (!is.null(msg$biblio$volume))                         msg$biblio$volume                              else NA_character_,
    pages   = pages,
    type    = crossref_type
  )
}

# Tries CrossRef first, then falls back to OpenAlex.
# Returns NULL only if both sources fail.
fetch_metadata <- function(doi) {
  pub <- fetch_crossref_metadata(doi)
  if (!is.null(pub)) return(pub)
  message(sprintf("  → Trying OpenAlex for %s...", doi))
  pub <- fetch_openalex_metadata(doi)
  if (!is.null(pub)) return(pub)
  message(sprintf("  ✗ No metadata found in CrossRef or OpenAlex for %s", doi))
  NULL
}

# ── 4. YAML helpers ───────────────────────────────────────────────────────────
# open_science.yml stores the open science metadata for each publication
# (data/code/preregistration URLs, collaboration type, preprint flags).
# This file is maintained by the user; the script only adds new entries
# at the top — it never modifies existing ones.

# Same header as in init_open_science.R, written when new entries are added.
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

# Reads the open_science.yml file and returns it as a list.
load_open_science <- function(yaml_file) {
  yaml.load_file(yaml_file)
}

# Writes the full list of entries to open_science.yml.
# Each entry is preceded by a comment showing the paper title and year
# so you can identify which publication you are editing without looking up DOIs.
save_open_science <- function(entries, pubs_by_doi, yaml_file) {
  lines <- c(YAML_HEADER, "")
  for (entry in entries) {
    pub <- pubs_by_doi[[tolower(entry$doi)]]
    if (!is.null(pub) && !is.na(pub$title))
      lines <- c(lines, paste0("# ", pub$title, " (", pub$year, ")"))
    lines <- c(lines, as.yaml(list(entry)), "")
  }
  writeLines(lines, yaml_file)
}

# ── 5. Popup window for new publications ──────────────────────────────────────
# When new publications are detected, a single gadget opens in the RStudio
# Viewer panel and loops through all of them one by one internally.
# This avoids the timing issues that occur when launching multiple gadgets
# in a loop. The form resets automatically between publications.
#
# Buttons:
#   "Save & Next"   — save current entry and move to the next publication
#   "Save & Finish" — same, but on the last publication (closes the gadget)
#   "Skip"          — skip current publication without saving (it will appear
#                     again the next time update_pubs.R is run)
#   X (title bar)   — exit early; entries saved so far are kept

pub_gadget_all <- function(pub_infos) {
  n <- length(pub_infos)
  if (n == 0) return(list())

  ui <- miniPage(
    gadgetTitleBar("New Publications"),
    miniContentPanel(
      # Progress indicator and publication details — updated reactively
      uiOutput("pub_info_ui"),
      hr(),
      # Open science checkboxes — URL fields appear only when box is ticked
      checkboxInput("has_data",   "Open data",           FALSE),
      conditionalPanel("input.has_data",
        textInput("open_data", NULL, placeholder = "https://osf.io/...")
      ),
      checkboxInput("has_code",   "Open code",           FALSE),
      conditionalPanel("input.has_code",
        textInput("open_code", NULL, placeholder = "https://github.com/...")
      ),
      checkboxInput("has_prereg", "Preregistration",     FALSE),
      conditionalPanel("input.has_prereg",
        textInput("preregistration", NULL, placeholder = "https://osf.io/...")
      ),
      checkboxInput("preprint",   "Preprint (bioRxiv)",  FALSE),
      checkboxInput("psyarxiv",   "Preprint (psyarXiv)", FALSE),
      hr(),
      # Collaboration — checkboxes allow selecting multiple types simultaneously
      checkboxGroupInput("collaboration", "Collaboration:",
        choices  = c("Laboratory"    = "laboratory",
                     "KU Leuven"     = "KULeuven",
                     "International" = "international"),
        selected = "laboratory",
        inline   = TRUE
      ),
      hr(),
      fluidRow(
        column(4, actionButton("skip",      "Skip",         class = "btn-warning btn-block")),
        column(8, uiOutput("save_btn_ui"))
      )
    )
  )

  server <- function(input, output, session) {
    idx     <- reactiveVal(1)   # index of the publication currently displayed
    entries <- reactiveVal(list())  # accumulates saved entries

    # Render publication details and progress indicator
    output$pub_info_ui <- renderUI({
      pub <- pub_infos[[idx()]]
      tagList(
        p(style = "color: grey; font-size: small;",
          sprintf("Publication %d of %d", idx(), n)),
        h4(pub$title),
        p(em(paste0(pub$journal, " (", pub$year, ")"))),
        p(style = "font-size:small; color:grey",
          paste(pub$authors, collapse = ", "))
      )
    })

    # Label changes to "Save & Finish" on the last publication
    output$save_btn_ui <- renderUI({
      label <- if (idx() < n) "Save & Next" else "Save & Finish"
      actionButton("save_next", label, class = "btn-primary btn-block")
    })

    # Resets all form fields to their defaults before showing the next publication
    reset_form <- function() {
      updateCheckboxInput(session, "has_data",   value = FALSE)
      updateCheckboxInput(session, "has_code",   value = FALSE)
      updateCheckboxInput(session, "has_prereg", value = FALSE)
      updateCheckboxInput(session, "preprint",   value = FALSE)
      updateCheckboxInput(session, "psyarxiv",   value = FALSE)
      updateTextInput(session, "open_data",        value = "")
      updateTextInput(session, "open_code",         value = "")
      updateTextInput(session, "preregistration",   value = "")
      updateCheckboxGroupInput(session, "collaboration", selected = "laboratory")
    }

    # Builds an open_science.yml entry from the current form state
    collect_entry <- function() {
      list(
        doi             = pub_infos[[idx()]]$doi,
        open_data       = if (isTRUE(input$has_data)   && nzchar(trimws(input$open_data)))       trimws(input$open_data)       else "",
        open_code       = if (isTRUE(input$has_code)   && nzchar(trimws(input$open_code)))       trimws(input$open_code)       else "",
        preregistration = if (isTRUE(input$has_prereg) && nzchar(trimws(input$preregistration))) trimws(input$preregistration) else "",
        preprint        = isTRUE(input$preprint),
        psyarxiv        = isTRUE(input$psyarxiv),
        collaboration   = as.list(input$collaboration)
      )
    }

    # Moves to the next publication or closes the gadget if on the last one
    advance <- function(save) {
      if (save) entries(c(entries(), list(collect_entry())))
      if (idx() < n) {
        idx(idx() + 1)
        reset_form()
      } else {
        stopApp(entries())
      }
    }

    observeEvent(input$save_next, advance(save = TRUE))
    observeEvent(input$skip,      advance(save = FALSE))
    # X button: exit early, returning whatever entries have been saved so far
    observeEvent(input$done,      stopApp(entries()))
  }

  runGadget(ui, server, viewer = dialogViewer("New Publications", width = 520, height = 720))
}

# ── 6. PDF helpers ────────────────────────────────────────────────────────────
# PDFs are stored in PubFile/ with a filename derived from the DOI:
#   - dots (.) are replaced by underscores (_)
#   - slashes (/) are replaced by hyphens (-)
#   - .pdf is appended
# Example: DOI 10.1016/j.neuropsychologia.2020.107639
#       → filename: 10_1016-j_neuropsychologia_2020_107639.pdf

doi_to_filename <- function(doi) {
  paste0(gsub("/", "-", gsub("\\.", "_", doi)), ".pdf")
}

# Checks which publications do not yet have a PDF in PubFile/ and
# prints the expected filename for each missing one so you can rename
# your PDF accordingly and place it in the right folder.
report_missing_pdfs <- function(pubs, pubfile_dir) {
  missing <- Filter(function(p) {
    !is.null(p) && !file.exists(file.path(pubfile_dir, doi_to_filename(p$doi)))
  }, pubs)

  if (length(missing) == 0) {
    message("✓ All PDFs are present in PubFile/")
    return(invisible())
  }

  message("\n─── MISSING PDFs ──────────────────────────────────────────────")
  message("  Rename your PDFs as shown below and place them in PubFile/.\n")
  for (p in missing) {
    message(sprintf("  Title:   %s", p$title))
    message(sprintf("  Journal: %s (%s)", p$journal, p$year))
    message(sprintf("  → Save as: PubFile/%s\n", doi_to_filename(p$doi)))
  }
}

# ── Main script ───────────────────────────────────────────────────────────────

# Step 1: fetch the list of DOIs from ORCID
message("Fetching publications from ORCID...")
dois <- fetch_orcid_dois(ORCID_ID)
message(sprintf("  Found %d publications.", length(dois)))

# Step 2: fetch full metadata for each DOI.
# CrossRef is tried first; OpenAlex is used as fallback for any DOI that
# CrossRef does not have. A small delay (0.05s) between requests is added
# to avoid overloading the servers (standard etiquette for API usage).
message("Fetching metadata from CrossRef (with OpenAlex fallback)...")
pubs_list <- setNames(
  lapply(dois, function(doi) { Sys.sleep(0.05); fetch_metadata(doi) }),
  dois
)
# Remove publications whose CrossRef type marks them as conference abstracts
# or other non-journal work. This catches cases where ORCID incorrectly
# classifies a conference abstract as "journal-article" (e.g. Frontiers
# conference abstracts, Journal of Vision ARVO meeting abstracts).
# "posted-content" is CrossRef's type for preprints (bioRxiv, psyarXiv).
ALLOWED_CROSSREF_TYPES <- c("journal-article", "book-chapter", "posted-content")
is_excluded <- function(p) {
  if (is.null(p)) return(FALSE)
  tp <- p$type
  !is.null(tp) && !is.na(tp) && !tp %in% ALLOWED_CROSSREF_TYPES
}
excluded <- Filter(is_excluded, pubs_list)
if (length(excluded) > 0) {
  message(sprintf("  Excluded %d non-journal work(s) based on CrossRef type:", length(excluded)))
  for (p in excluded) message(sprintf("    [%s] %s", p$type, p$doi))
}
pubs_list <- lapply(pubs_list, function(p) if (is_excluded(p)) NULL else p)

# Keep a filtered copy (without NULLs) for lookups by DOI
pubs_by_doi <- Filter(Negate(is.null), pubs_list)

# Step 3: save the metadata to a local cache file.
# Pubs.Rmd reads from this cache to render the publications page,
# so the site can be re-rendered without hitting the APIs every time.
saveRDS(pubs_list, CACHE_FILE)
message(sprintf("  Metadata cached to %s", CACHE_FILE))

# Step 4: compare fetched publications against open_science.yml to find new ones
existing      <- load_open_science(YAML_FILE)
existing_dois <- tolower(sapply(existing, function(e) e$doi))
new_dois      <- setdiff(tolower(dois), existing_dois)

if (length(new_dois) > 0) {
  message(sprintf("\nFound %d new publication(s). Opening form...", length(new_dois)))
  # Filter to publications that have CrossRef metadata; pass them all to a
  # single gadget that loops through them internally (avoids timing issues
  # that occur when launching multiple gadgets sequentially in a for loop)
  new_pub_infos <- Filter(Negate(is.null), pubs_by_doi[new_dois])
  new_entries   <- pub_gadget_all(new_pub_infos)
  if (length(new_entries) > 0) {
    # New entries are prepended so they appear at the top of the yml file
    save_open_science(c(new_entries, existing), pubs_by_doi, YAML_FILE)
    message(sprintf("open_science.yml updated with %d new entry/entries.", length(new_entries)))
  }
} else {
  message("No new publications found.")
}

# Step 5: report any publications missing a PDF in PubFile/
report_missing_pdfs(pubs_list, PUBFILE_DIR)

# # Step 6: re-render the full website
# message("\nRendering site...")
# rmarkdown::render_site(SITE_DIR)
# message("Done!")
