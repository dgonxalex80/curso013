#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
script_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
script_dir <- if (length(script_arg)) dirname(normalizePath(script_arg)) else getwd()
setwd(script_dir)

source_file <- "presentacion_metodologia_estadistica.Rmd"
output_dir <- "."
html_file <- "presentacion_metodologia_estadistica.html"
pdf_file <- "presentacion_metodologia_estadistica.pdf"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

rmarkdown::render(
  input = source_file,
  output_format = xaringan::moon_reader(
    css = c("default", "xaringan-metodologia.css"),
    nature = list(
      ratio = "16:9",
      highlightStyle = "github",
      highlightLines = TRUE,
      countIncrementalSlides = FALSE,
      slideNumberFormat = "%current% / %total%",
      navigation = list(scroll = FALSE)
    )
  ),
  output_file = basename(html_file),
  output_dir = output_dir,
  envir = new.env(parent = globalenv()),
  clean = TRUE
)

pagedown::chrome_print(
  input = normalizePath(html_file),
  output = normalizePath(pdf_file, mustWork = FALSE),
  browser = pagedown::find_chrome(),
  wait = 2
)

message("Presentación creada:")
message("  HTML: ", html_file)
message("  PDF:  ", pdf_file)
