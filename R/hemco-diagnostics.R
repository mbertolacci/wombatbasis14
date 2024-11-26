#' @export
hemco_diagnostics <- function(...) {
  structure(list(...), class = 'hemco_diagnostics')
}

#' @export
as.character.hemco_diagnostics <- function(x, ...) {
  sprintf(
    '# Name                Spec   ExtNr Cat Hier Dim OutUnit  LongName
%s',
    paste0(sapply(x, as.character, ...), collapse = '\n')
  )
}

#' @export
print.hemco_diagnostics <- function(x, ...) {
  cat(as.character(x, ...))
}

#' @export
hemco_diagnostic <- function(
  name,
  species,
  extension_number,
  category,
  hierarchy,
  dimensions,
  output_unit,
  long_name
) {
  structure(list(
    name = name,
    species = species,
    extension_number = extension_number,
    category = category,
    hierarchy = hierarchy,
    dimensions = dimensions,
    output_unit = output_unit,
    long_name = long_name
  ), class = 'hemco_diagnostic')
}

#' @export
as.character.hemco_diagnostic <- function(x, .envir = parent.frame(), ...) {
  x_glued <- .glue_recursive(x, .envir)
  sprintf(
    '%s %s %d %d %d %d %s %s',
    x_glued$name,
    x_glued$species,
    x_glued$extension_number,
    x_glued$category,
    x_glued$hierarchy,
    x_glued$dimensions,
    x_glued$output_unit,
    x_glued$long_name
  )
}

#' @export
print.hemco_diagnostic <- function(x, ...) {
  cat(as.character(x, ...))
}
