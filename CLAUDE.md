# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Academic website for the NoMAD Laboratory (Neuroscience of Movement in Aging and Disease) at KU Leuven, Belgium. Built with R Markdown's `rmarkdown::render_site()`. Source files are `.Rmd`; rendered output `.html` files live in the same root directory.

## Building the site

Open the project in RStudio and use **Build > Build Website**, or run from R:

```r
setwd("C:/Users/jjorb/Dropbox/website")
rmarkdown::render_site()
```

To render a single page:

```r
rmarkdown::render("index.Rmd")
```

## Site structure

- **`_site.yml`** — site-wide config: navbar, output directory (`.`), HTML theme (`spacelab`/`textmate`)
- **`.Rmd` files** — one per page; each knits to an `.html` with the same base name
- **`images/`** — images referenced in `.Rmd` files
- **`PubFile/`** — PDF files for publications; filenames are derived from DOIs (`.` → `_`, `/` → `-`, then `.pdf` appended)
- **`MyPubBetterBibTex.bib`** — BibTeX export from Zotero (Better BibTeX format), used by `Pubs.Rmd`
- **`site_libs/`** — auto-generated JS/CSS dependencies, do not edit manually

## Publications workflow (`Pubs.Rmd`)

Publications are rendered dynamically from the BibTeX file using the `bib2df` R package. To update publications:

1. In Zotero, select all items in "My Publications"
2. Export → Better BibTeX format, checking "Export Notes"
3. Save as `MyPubBetterBibTex.bib` in the website root
4. Rebuild the site
5. For each new paper, add the PDF to `PubFile/` named as: DOI with `.` replaced by `_` and `/` replaced by `-`, plus `.pdf` extension

Notes in Zotero entries drive the open science icons. Keywords recognized in the NOTE field: `laboratory`, `KULeuven`, `International`/`international`, `data`, `preregistration`, `preprint`, `psyarxiv`, `code`.

## Key R packages required

- `rmarkdown` — site rendering
- `bib2df` — parsing BibTeX for publications page
- `stringi` — string manipulation in `Pubs.Rmd`
- `fontawesome` — inline FA icons in R chunks

## Icons

Pages use two external icon libraries loaded via CDN:
- **Font Awesome 4.7** (`fa-*` classes)
- **Academicons** (`ai-*` classes, e.g. `ai-google-scholar`, `ai-orcid`, `ai-osf`, `ai-biorxiv`, `ai-psyarxiv`)

The `fontawesome` R package is also used in R code chunks for inline SVG icons.
