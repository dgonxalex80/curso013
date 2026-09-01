# Renderizar todo el sitio
rmarkdown::render_site(".")

# Abrir el resultado sin fallar en Positron/RStudio
output_path <- normalizePath("docs/index.html", mustWork = FALSE)

if (file.exists(output_path) && interactive()) {
  opened <- FALSE
  viewer_fun <- getOption("viewer")

  # En algunos entornos (p. ej. Positron) rstudioapi::viewer puede fallar.
  if (is.function(viewer_fun)) {
    opened <- tryCatch({
      viewer_fun(output_path)
      TRUE
    }, error = function(e) FALSE)
  }

  if (!opened) {
    message("Viewer interno no disponible; abriendo en navegador.")
    utils::browseURL(output_path)
  }
}
