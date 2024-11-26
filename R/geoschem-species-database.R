#' @export
geoschem_species_database <- function(species) {
  structure(list(
    species = species
  ), class = 'geoschem_species_database')
}

#' @export
as.character.geoschem_species_database <- function(x, .envir = parent.frame(), ...) {
  x_glued <- .glue_recursive(x, .envir)
  yaml::as.yaml(x_glued$species, handlers = list(logical = yaml::verbatim_logical))
}

#' @export
read_geoschem_species_database <- function(file) {
  geoschem_species_database(lapply(
    yaml::read_yaml(file),
    function(x) {
      class(x) <- 'geoschem_species_database_entry'
      x
    }
  ))
}

#' @export
geoschem_species_database_entry <- function(...) {
  structure(list(...), class = 'geoschem_species_database_entry')
}
