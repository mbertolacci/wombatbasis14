
#' @export
basis_to_runs <- function(
  basis,
  base_run,
  max_run_length,
  postprocess_run,
  postprocess_split_run,
  ...
) {
  runs <- .basis_to_runs_tracer_transport(basis, base_run, ...)

  if (!missing(postprocess_run)) {
    runs <- lapply(runs, postprocess_run)
  }

  if (!missing(max_run_length)) {
    # NOTE(mgnb): the base run is excluded from the split
    runs <- c(
      runs[1],
      do.call(c, lapply(
        runs[2 : length(runs)],
        .split_run_by_period,
        max_run_length
      ))
    )
  }

  if (!missing(postprocess_split_run)) {
    runs <- lapply(runs, postprocess_split_run)
  }

  runs
}

#' @export
write_basis_runs <- function(basis_runs, base_directory, optimise = TRUE) {
  for (basis_run in basis_runs) {
    logger::log_debug('Writing {basis_run$name}')
    path <- file.path(base_directory, basis_run$name)
    if (optimise) {
      basis_run$configuration$hemco$base_emissions <- optimise_base_emissions(
        basis_run$configuration$hemco$base_emissions
      )
    }
    write_geoschem_run(basis_run$configuration, path)
  }
  write.csv(
    unique(do.call(rbind, lapply(basis_runs, getElement, 'mapping'))),
    file.path(base_directory, 'mapping.csv'),
    row.names = FALSE
  )
}

.times_to_hemco_source_time <- function(x) {
  minutes <- lubridate::minute(x)
  if (any(minutes != 0)) {
    '*'
  } else {
    years <- lubridate::year(x)
    months_vary <- any(lubridate::month(x) != 1)
    days_vary <- any(lubridate::day(x) != 1)
    hours_vary <- any(lubridate::hour(x) != 0)
    sprintf(
      '%s/%s/%s/%s',
      if (min(years) == max(years)) {
        min(years)
      } else {
        sprintf('%d-%d', min(years), max(years))
      },
      if (months_vary || days_vary || hours_vary) '1-12' else '1',
      if (days_vary || hours_vary) '1-31' else '1',
      if (hours_vary) '0-23' else '0'
    )
  }
}

.split_run_by_period <- function(
  run,
  max_length = lubridate::period(1, 'years')
) {
  start_date <- run$configuration$main$simulation$start_date
  end_date <- run$configuration$main$simulation$end_date

  if (start_date + max_length >= end_date) {
    return(list(run))
  }

  current_run <- run
  current_start_date <- start_date
  index <- 1L
  runs <- NULL
  while (current_start_date < end_date) {
    current_run$configuration$main$simulation$start_date <- current_start_date
    current_run$configuration$main$simulation$end_date <- min(
      end_date,
      current_start_date + max_length
    )
    previous_run_name <- NULL
    if (index > 1L) {
      previous_run_name <- current_run$name

      current_run$configuration$symlinks[
        startsWith(
          names(current_run$configuration$symlinks),
          'GEOSChem.Restart.'
        )
      ] <- NULL
      current_run$configuration$files[
        startsWith(
          names(current_run$configuration$files),
          'GEOSChem.Restart.'
        )
      ] <- NULL
      restart_filename <- format(
        current_start_date,
        'GEOSChem.Restart.%Y%m%d_%H%Mz.nc4'
      )
      current_run$configuration$symlinks[[file.path(
        'Restarts',
        basename(restart_filename)
      )]] <- file.path(
        '..',
        current_run$name,
        'Restarts',
        restart_filename
      )
    }
    current_run$name <- sprintf('%s_split%02d', run$name, index)
    current_run$split <- list(
      parent = run$name,
      previous = previous_run_name
    )

    runs <- c(runs, list(current_run))

    current_start_date <- current_start_date + max_length
    index <- index + 1L
  }

  runs
}
