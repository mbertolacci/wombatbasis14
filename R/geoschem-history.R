#' @export
geoschem_history <- function(
  file_exp_id = './GEOSChem',
  collections
) {
  if (is(collections, 'geoschem_history_collection')) {
    collections <- list(collections)
  }
  structure(list(
    file_exp_id = file_exp_id,
    collections = collections
  ), class = 'geoschem_history')
}

#' @export
as.character.geoschem_history <- function(x, .envir = parent.frame(), ...) {
  glue_if_glue <- function(x) {
    .glue_if_glue(x, .envir)
  }
  sprintf(
    "EXPID: %s

COLLECTIONS: %s,
::
%s
::",
    glue_if_glue(x$file_exp_id),
    paste0(
      sprintf("'%s'", glue_if_glue(sapply(x$collections, getElement, 'name'))),
      collapse = ',\n'
    ),
    paste0(
      sapply(x$collections, as.character, .envir = .envir),
      collapse = '\n::\n'
    )
  )
}

#' @export
print.geoschem_history <- function(x, ...) {
  cat(as.character(x, ...))
}

#' @export
geoschem_history_collection <- function(
  name,
  template = '%y4%m2%d2_%h2%n2z.nc4',
  format = 'CFIO',
  frequency,
  duration,
  mode = c('instantaneous', 'time-averaged'),
  fields
) {
  mode <- match.arg(mode)

  structure(list(
    name = name,
    template = template,
    format = format,
    frequency = frequency,
    duration = duration,
    mode = mode,
    fields = fields
  ), class = 'geoschem_history_collection')
}

#' @export
as.character.geoschem_history_collection <- function(x, .envir = parent.frame(), ...) {
  x_glued <- .glue_recursive(x, .envir)
  sprintf(
    "%1$s.template: '%2$s',
%1$s.format: '%3$s',
%1$s.frequency: %4$s,
%1$s.duration: %5$s,
%1$s.mode: '%6$s',
%1$s.fields: %7$s,",
    x$name,
    x_glued$template,
    x_glued$format,
    x_glued$frequency,
    x_glued$duration,
    x_glued$mode,
    paste0(sprintf("'%s', 'GIGCchem'", x_glued$fields), collapse = ',\n')
  )
}

#' @export
print.geoschem_history_collection <- function(x, ...) {
  cat(as.character(x, ...))
}
