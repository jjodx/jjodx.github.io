# pub_dedup.R
# =============================================================================
# PURPOSE:
#   The same paper can appear on ORCID under several DOIs: a bioRxiv preprint,
#   eLife "reviewed preprint" versions (.1, .2), the final published version,
#   etc. This file groups these versions together so that each paper is shown
#   only once on the publications page.
#
# TWO VERSIONS ARE CONSIDERED THE SAME PAPER WHEN:
#   1. Their titles are identical (ignoring capitals, spaces and punctuation), or
#   2. CrossRef links them (e.g. "has-preprint", "is-version-of"), or
#   3. You confirmed they are the same paper (stored in same_papers.yml).
#      update_pubs.R asks you about titles that are very similar but not
#      identical (e.g. when the title changed between preprint and publication).
#
# USED BY:
#   update_pubs.R (to detect new publications and ask about similar titles)
#   Pubs.Rmd      (to render each paper only once, with a link to its preprint)
# =============================================================================

library(yaml)

# Word-overlap score above which two different titles are considered
# "very similar" and update_pubs.R asks whether they are the same paper.
# (In Sept 2026, real pairs scored 0.58-0.69; unrelated papers at most 0.37.)
SIMILAR_TITLE_THRESHOLD <- 0.5

# ── Title comparison ──────────────────────────────────────────────────────────

# Reduces a title to lowercase letters and digits only, so that
# "Modeling Human ..." and "Modeling human ...." give the same key.
title_key <- function(title) {
  gsub("[^a-z0-9]", "", tolower(title))
}

# Splits a title into its set of unique lowercase words.
title_words <- function(title) {
  unique(strsplit(trimws(gsub("[^a-z0-9]+", " ", tolower(title))), " ")[[1]])
}

# Share of words the two titles have in common (0 = none, 1 = all).
title_similarity <- function(title_a, title_b) {
  a <- title_words(title_a)
  b <- title_words(title_b)
  length(intersect(a, b)) / length(union(a, b))
}

# ── Decisions file (same_papers.yml) ──────────────────────────────────────────
# Stores your answers so you are only asked once per pair of DOIs.

DECISIONS_HEADER <- c(
  "# same_papers.yml — your answers about publications with similar titles",
  "# ──────────────────────────────────────────────────────────────────────",
  "# Written by update_pubs.R when it asks whether two DOIs are the same paper.",
  "#   same:      pairs of DOIs that are versions of the same paper",
  "#              (only the published version is shown on the website)",
  "#   different: pairs of DOIs that are different papers (never asked again)",
  "# You can edit this file by hand, e.g. to undo a wrong answer.",
  ""
)

# Reads same_papers.yml; returns empty lists if the file does not exist yet.
load_decisions <- function(file) {
  empty <- list(same = list(), different = list())
  if (!file.exists(file)) return(empty)
  d <- yaml.load_file(file)
  list(
    same      = lapply(d$same,      function(pair) tolower(unlist(pair))),
    different = lapply(d$different, function(pair) tolower(unlist(pair)))
  )
}

save_decisions <- function(decisions, file) {
  writeLines(c(DECISIONS_HEADER, as.yaml(decisions)), file)
}

# TRUE if the pair (doi_a, doi_b) appears in a list of pairs, in either order.
pair_in_list <- function(doi_a, doi_b, pairs) {
  any(vapply(pairs, function(pair) all(c(doi_a, doi_b) %in% pair), logical(1)))
}

# ── Grouping versions of the same paper ───────────────────────────────────────

# Returns one group number per publication; versions of the same paper share
# the same number. Uses a simple "union-find": each publication points to a
# representative, and linking two publications merges their groups.
group_publications <- function(pubs, decisions) {
  dois   <- vapply(pubs, function(p) p$doi, character(1))
  keys   <- vapply(pubs, function(p) title_key(p$title), character(1))
  parent <- seq_along(pubs)

  find_root <- function(i) {
    while (parent[i] != i) i <- parent[i]
    i
  }
  link <- function(i, j) {
    ri <- find_root(i); rj <- find_root(j)
    if (ri != rj) parent[rj] <<- ri
  }

  for (i in seq_along(pubs)) {
    # Rule 1: identical titles
    for (j in which(keys == keys[i])) link(i, j)
    # Rule 2: DOIs that CrossRef lists as related (preprint, other version)
    for (j in which(dois %in% pubs[[i]]$related_dois)) link(i, j)
  }
  # Rule 3: pairs you confirmed as the same paper
  for (pair in decisions$same) {
    idx <- which(dois %in% pair)
    if (length(idx) == 2) link(idx[1], idx[2])
  }

  vapply(seq_along(pubs), find_root, integer(1))
}

# Version number of a DOI relative to the other DOIs of the same paper:
# 10.7554/elife.109440.3 is version 3 of 10.7554/elife.109440. Returns 0 when
# the DOI is not a numbered version of another DOI in the group.
doi_version <- function(doi, group_dois) {
  for (base in group_dois) {
    if (startsWith(doi, paste0(base, ".")))
      return(suppressWarnings(as.integer(substring(doi, nchar(base) + 2))))
  }
  0L
}

is_preprint <- function(pub) identical(pub$type, "posted-content")

# Orders the versions of one paper from "best to show" to "least good":
# published before preprint, then most recent year, then highest version.
rank_versions <- function(group) {
  dois      <- vapply(group, function(p) p$doi, character(1))
  published <- !vapply(group, is_preprint, logical(1))
  years     <- vapply(group, function(p) as.integer(p$year), integer(1))
  versions  <- vapply(dois, doi_version, integer(1), group_dois = dois)
  years[is.na(years)]       <- 0L
  versions[is.na(versions)] <- 0L
  group[order(-published, -years, -versions)]
}

# Main entry point: returns one publication per paper (the best version).
# Each returned publication gets two extra fields:
#   other_versions — all hidden versions of the same paper
#   preprints      — the hidden versions that are preprints (for links)
deduplicate_publications <- function(pubs, decisions) {
  pubs   <- unname(Filter(Negate(is.null), pubs))
  groups <- group_publications(pubs, decisions)
  lapply(split(pubs, groups), function(group) {
    ranked <- rank_versions(group)
    shown  <- ranked[[1]]
    shown$other_versions <- ranked[-1]
    shown$preprints      <- Filter(is_preprint, ranked[-1])
    shown
  })
}

# ── Similar (but not identical) titles ────────────────────────────────────────

# Lists pairs of publications whose titles are very similar, that are not
# already grouped together, and that you have not answered about yet.
find_similar_pairs <- function(pubs, decisions) {
  pubs   <- unname(Filter(Negate(is.null), pubs))
  groups <- group_publications(pubs, decisions)
  pairs  <- list()
  for (i in seq_along(pubs)) for (j in seq_along(pubs)) {
    if (j <= i || groups[i] == groups[j]) next
    a <- pubs[[i]]; b <- pubs[[j]]
    if (pair_in_list(a$doi, b$doi, decisions$different)) next
    if (title_similarity(a$title, b$title) >= SIMILAR_TITLE_THRESHOLD)
      pairs <- c(pairs, list(list(a, b)))
  }
  pairs
}
