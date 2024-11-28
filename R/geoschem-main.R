#' @export
geoschem_main_settings <- function(
  simulation,
  grid = geoschem_grid_settings(),
  timesteps = geoschem_timesteps_settings(),
  operations,
  extra_diagnostics = geoschem_extra_diagnostics_settings()
) {
  structure(list(
    simulation = simulation,
    grid = grid,
    timesteps = timesteps,
    operations = operations,
    extra_diagnostics = extra_diagnostics
  ), class = 'geoschem_main_settings')
}

#' @export
print.geoschem_main_settings <- function(x, ...) {
  cat(as.character(x))
}

#' @export
as.character.geoschem_main_settings <- function(x, .envir = parent.frame(), ...) {
  yaml::as.yaml(as.list(x, .envir = .envir), handlers = list(logical = yaml::verbatim_logical))
}

#' @export
as.list.geoschem_main_settings <- function(x, ...) {
  list(
    simulation = as.list(x$simulation, ...),
    grid = as.list(x$grid, ...),
    timesteps = as.list(x$timesteps, ...),
    operations = as.list(x$operations, ...),
    extra_diagnostics = as.list(x$extra_diagnostics, ...)
  )
}

#' @export
geoschem_simulation_settings <- function(
  name,
  start_date,
  end_date,
  root_data_dir,
  met_field,
  species_database_file = './species_database.yml',
  species_metadata_output_file = './geoschem_species_metadata.yml',
  verbose = list(activate = FALSE, on_cores = 'root'),
  use_gcclassic_timers = FALSE
) {
  structure(list(
    name = name,
    start_date = start_date,
    end_date = end_date,
    root_data_dir = root_data_dir,
    met_field = met_field,
    species_database_file = species_database_file,
    species_metadata_output_file = species_metadata_output_file,
    verbose = verbose,
    use_gcclassic_timers = use_gcclassic_timers
  ), class = c('geoschem_simulation_settings', 'geoschem_main_settings_part'))
}

#' @export
as.list.geoschem_simulation_settings <- function(x, .envir = parent.frame(), ...) {
  to_date_format <- function(x) {
    if (is.vector(x)) x else c(
      as.integer(format(x, '%Y%m%d')),
      as.integer(format(x, '%H%M%S'))
    )
  }

  .glue_recursive(list(
    name = x$name,
    start_date = to_date_format(x$start_date),
    end_date = to_date_format(x$end_date),
    root_data_dir = x$root_data_dir,
    met_field = x$met_field,
    species_database_file = x$species_database_file,
    species_metadata_output_file = x$species_metadata_output_file,
    verbose = x$verbose,
    use_gcclassic_timers = x$use_gcclassic_timers
  ), .envir = .envir)
}

#' @export
geoschem_grid_settings <- function(
  resolution = '4.0x5.0',
  number_of_levels = 47L,
  longitude_range = c(-180.0, 180.0),
  center_at_180 = TRUE,
  latitude_range = c(-90.0, 90.0),
  half_size_polar_boxes = TRUE,
  nested_grid_simulation_activate = FALSE,
  nested_grid_buffer_zone_NSEW = c(0L, 0L, 0L, 0L)
) {
  structure(list(
    resolution = resolution,
    number_of_levels = number_of_levels,
    longitude_range = longitude_range,
    center_at_180 = center_at_180,
    latitude_range = latitude_range,
    half_size_polar_boxes = half_size_polar_boxes,
    nested_grid_simulation_activate = nested_grid_simulation_activate,
    nested_grid_buffer_zone_NSEW = nested_grid_buffer_zone_NSEW
  ), class = c('geoschem_grid_settings', 'geoschem_main_settings_part'))
}

#' @export
as.list.geoschem_grid_settings <- function(x, .envir = parent.frame(), ...) {
  .glue_recursive(list(
    resolution = x$resolution,
    number_of_levels = x$number_of_levels,
    longitude = list(
      range = x$longitude_range,
      center_at_180 = x$center_at_180
    ),
    latitude = list(
      range = x$latitude_range,
      half_size_polar_boxes = x$half_size_polar_boxes
    ),
    nested_grid_simulation = list(
      activate = x$nested_grid_simulation_activate,
      buffer_zone_NSEW = x$nested_grid_buffer_zone_NSEW
    )
  ), .envir = .envir)
}

#' @export
geoschem_timesteps_settings <- function(
  transport_timestep_in_s = 600L,
  chemistry_timestep_in_s = 1200L
) {
  structure(list(
    transport_timestep_in_s = transport_timestep_in_s,
    chemistry_timestep_in_s = chemistry_timestep_in_s
  ), class = c('geoschem_timesteps_settings', 'geoschem_main_settings_part'))
}

#' @export
as.list.geoschem_timesteps_settings <- function(x, .envir = parent.frame(), ...) {
  .glue_recursive(list(
    transport_timestep_in_s = x$transport_timestep_in_s,
    chemistry_timestep_in_s = x$chemistry_timestep_in_s
  ), .envir = .envir)
}

#' @export
geoschem_transport_settings <- function(
  activate = TRUE,
  fill_negative_values = TRUE,
  iord_jord_kord = c(3L, 3L, 7L),
  transported_species
) {
  structure(list(
    gcclassic_tpcore = list(
      activate = activate,
      fill_negative_values = fill_negative_values,
      iord_jord_kord = iord_jord_kord
    ),
    transported_species = transported_species
  ), class = c('geoschem_transport_settings', 'geoschem_main_settings_part'))
}

#' @export
geoschem_operations_settings <- function(
  chemistry = list(activate = FALSE),
  convection = list(activate = TRUE),
  dry_deposition = list(activate = FALSE),
  pbl_mixing = list(activate = TRUE, use_non_local_pbl = TRUE),
  wet_deposition = list(activate = FALSE),
  transport
) {
  structure(list(
    chemistry = chemistry,
    convection = convection,
    dry_deposition = dry_deposition,
    pbl_mixing = pbl_mixing,
    wet_deposition = wet_deposition,
    transport = transport
  ), class = 'geoschem_operations_settings')
}

#' @export
as.list.geoschem_operations_settings <- function(x, .envir = parent.frame(), ...) {
  .glue_recursive(list(
    chemistry = x$chemistry,
    convection = x$convection,
    dry_deposition = x$dry_deposition,
    pbl_mixing = x$pbl_mixing,
    wet_deposition = x$wet_deposition,
    transport = as.list(x$transport, .envir = .envir)
  ), .envir = .envir)
}

#' @export
geoschem_extra_diagnostics_settings <- function(
  obspack = list(activate = FALSE),
  planeflight = list(activate = FALSE)
) {
  structure(list(
    obspack = obspack,
    planeflight = planeflight
  ), class = 'geoschem_extra_diagnostics_settings')
}

#' @export
as.list.geoschem_extra_diagnostics_settings <- function(x, .envir = parent.frame(), ...) {
  output <- .glue_recursive(list(
    obspack = x$obspack,
    planeflight = x$planeflight
  ), .envir = .envir)
  class(output) <- NULL
  output
}
