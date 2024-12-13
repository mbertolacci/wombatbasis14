#' @export
flux_field <- function(
  name,
  source_file,
  source_variable,
  source_time,
  cre,
  source_dimension,
  source_unit,
  multiply_by = 1
) {
  structure(list(
    name = name,
    source_file = source_file,
    source_variable = source_variable,
    source_time = source_time,
    cre = cre,
    source_dimension = source_dimension,
    source_unit = source_unit,
    multiply_by = multiply_by
  ), class = 'flux_field')
}

#' @export
print.flux_field <- function(x, ...) {
  cat(sprintf('flux_field with name "%s"', x$name))
}

#' @export
flux_basis <- function(
  structure,
  start_time,
  end_time,
  basis_parts,
  flux_fields,
  factor_grids,
  use_mclapply = FALSE,
  ...
) {
  factor_grids <- lapply(factor_grids, function(x) {
    if (is.null(names(x))) names(x) <- as.character(x)
    x
  })

  if (missing(basis_parts)) {
    basis_parts <- .basis_parts_from_structure(structure)
  }
  part_names <- sapply(basis_parts, getElement, 'name')
  stopifnot(length(unique(part_names)) == length(part_names))

  # Check that all fields used in basis parts are present in flux_fields
  flux_field_names <- sapply(flux_fields, getElement, 'name')
  stopifnot(all(sapply(
    basis_parts,
    function(basis_part) {
      all(basis_part$fields %in% flux_field_names)
    }
  )))

  lapply_fn <- if (use_mclapply) parallel::mclapply else lapply

  all_factor_grid_combinations <- do.call(rbind, lapply(basis_parts, function(basis_part) {
    if (length(basis_part$factor_grid_names) == 0) {
      return(NULL)
    }
    output <- expand.grid(lapply(
      factor_grids,
      names
    )[basis_part$factor_grid_names])
    for (factor_grid_name in names(factor_grids)) {
      if (is.null(output[[factor_grid_name]])) {
        output[[factor_grid_name]] <- NA
      }
    }
    as.matrix(output)
  }))
  factor_grid_combinations <- unique(all_factor_grid_combinations)
  
  scaling_grids <- lapply_fn(seq_len(nrow(factor_grid_combinations)), function(i) {
    factor_grid_combination_long <- factor_grid_combinations[i, ]
    factor_grid_combination <- factor_grid_combination_long[!is.na(factor_grid_combination_long)]
    factor_grid_names <- names(factor_grid_combination)

    basis_function_factor_grids <- lapply(
      factor_grid_names,
      function(name) {
        factor_grids[[name]][[
          factor_grid_combination[name]
        ]]
      }
    )
    value <- encode_rle_grid(.truncate_grid_times(
      multiply_grids(basis_function_factor_grids),
      start = TRUE,
      end = FALSE
    ))
    list(
      name = paste0(sprintf(
        '%s%s',
        factor_grid_names,
        factor_grid_combination
      ), collapse = '_'),
      source_time = .scaling_grid_source_time(value),
      cre = .scaling_grid_cre(value),
      value = value
    )
  }, ...)

  scaling_grids_by_name <- scaling_grids
  names(scaling_grids_by_name) <- sapply(scaling_grids, getElement, 'name')

  basis_functions <- do.call(c, lapply(basis_parts, function(basis_part) {
    if (length(basis_part$factor_grid_names) == 0) {
      return(list(
        part_name = basis_part$name,
        basis_function_name = basis_part$name,
        factor_grids = NULL,
        fields = basis_part$fields
      ))
    }

    factor_grid_combinations <- as.matrix(expand.grid(lapply(
      factor_grids,
      names
    )[basis_part$factor_grid_names]))

    lapply(seq_len(nrow(factor_grid_combinations)), function(i) {
      factor_grid_combination <- factor_grid_combinations[i, ]
      factor_grid_names <- names(factor_grid_combination)

      basis_function_factor_grids <- lapply(
        factor_grid_names,
        function(name) {
          factor_grids[[name]][[
            factor_grid_combination[name]
          ]]
        }
      )

      scaling_grid_name <- paste0(sprintf(
        '%s%s',
        factor_grid_names,
        factor_grid_combination
      ), collapse = '_')

      list(
        name = sprintf(
          '%s_%s',
          basis_part$name,
          scaling_grid_name
        ),
        flux_field_names = basis_part$fields,
        scaling_grid_name = scaling_grid_name,
        start_time = .scaling_grid_start_time(
          scaling_grids_by_name[[scaling_grid_name]]$value,
          start_time
        ),
        end_time = end_time
      )
    })
  }))

  structure(list(
    flux_fields = flux_fields,
    basis_functions = basis_functions,
    scaling_grids = scaling_grids
  ), class = 'flux_basis')
}

#' @export
print.flux_basis <- function(x, ...) {
  cat(sprintf(  
    'flux_basis with %d scaling grids across %d basis functions\n',
    length(x$scaling_grids),
    length(x$basis_functions)
  ))
}

#' @export
write_flux_basis <- function(flux_basis, base_directory) {
  dir.create(base_directory, recursive = TRUE, showWarnings = FALSE)

  get_scaling_grid_path <- function(scaling_grid) {
    sprintf('%s.nc', file.path(base_directory, 'scaling-grids', scaling_grid$name))
  }

  output_yaml <- list(
    flux_fields = flux_basis$flux_fields,
    basis_functions = lapply(flux_basis$basis_functions, function(basis_function) {
      basis_function$start_time <- format(basis_function$start_time, '%Y-%m-%d %H:%M:%S')
      basis_function$end_time <- format(basis_function$end_time, '%Y-%m-%d %H:%M:%S')
      basis_function
    }),
    scaling_grids = lapply(flux_basis$scaling_grids, function(scaling_grid) {
      scaling_grid$value <- NULL
      scaling_grid$source_file <- get_scaling_grid_path(scaling_grid)
      scaling_grid$source_variable <- 'value'
      scaling_grid
    })
  )
  yaml::write_yaml(output_yaml, file.path(base_directory, 'flux-basis.yaml'))

  dir.create(file.path(base_directory, 'scaling-grids'), recursive = TRUE, showWarnings = FALSE)
  for (scaling_grid in flux_basis$scaling_grids) {
    write_nc_grid_data(
      scaling_grid$value,
      get_scaling_grid_path(scaling_grid)
    )
  }
}

#' @export
read_flux_basis <- function(path) {
  output <- yaml::read_yaml(path)
  output$flux_fields <- lapply(output$flux_fields, function(flux_field_input) {
    do.call(flux_field, flux_field_input)
  })
  start_times_str <- sapply(output$basis_functions, getElement, 'start_time')
  start_times <- ymd_hms(start_times_str)
  end_times_str <- sapply(output$basis_functions, getElement, 'end_time')
  end_times <- ymd_hms(end_times_str)
  for (i in seq_along(output$basis_functions)) {
    output$basis_functions[[i]]$start_time <- start_times[i]
    output$basis_functions[[i]]$end_time <- end_times[i]
  }
  output
}

.basis_parts_from_structure <- function(structure) {
  variables <- attr(terms(structure), 'variables')
  part_number <- 1
  basis_parts <- lapply(
    variables[2 : length(variables)],
    function(variable) {
      parts <- .split_bars(variable)

      if (length(parts) >= 3) {
        name <- parts[[3]]
      } else {
        name <- sprintf('part%02d', part_number)
        part_number <<- part_number + 1
      }

      fields <- parts[[1]]

      list(
        name = name,
        fields = fields,
        factor_grid_names = if (length(parts) >= 2) parts[[2]] else NULL
      )
    }
  )
}

.split_bars <- function(variable) {
  if (is.name(variable)) {
    list(as.character(variable))
  } else if (variable[[1]] == as.name('|')) {
    c(.split_bars(variable[[2]]), list(all.vars(variable[[3]])))
  } else {
    list(all.vars(variable))
  }
}

.scaling_grid_start_time <- function(grid, default_start_time) {
  if (inherits(grid, 'rle_grid_data')) {
    grid_dimnames <- attr(grid, 'dimension_names')
  } else {
    grid_dimnames <- dimnames(grid)
  }
  has_time_grid <- 'time' %in% names(grid_dimnames)
  if (has_time_grid) {
    min(attr(grid, 'time'))
  } else {
    default_start_time
  }
}

.scaling_grid_source_time <- function(grid) {
  if (inherits(grid, 'rle_grid_data')) {
    grid_dimnames <- attr(grid, 'dimension_names')
  } else {
    grid_dimnames <- dimnames(grid)
  }
  has_time_grid <- 'time' %in% names(grid_dimnames)

  if (!has_time_grid) {
    return('2000/1/1/0')
  }

  times <- attr(grid, 'time')
  minutes <- lubridate::minute(times)
  if (any(minutes != 0)) {
    '*'
  } else {
    years <- lubridate::year(times)
    months_vary <- any(lubridate::month(times) != 1)
    days_vary <- any(lubridate::day(times) != 1)
    hours_vary <- any(lubridate::hour(times) != 0)
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

.scaling_grid_cre <- function(grid) {
  if (inherits(grid, 'rle_grid_data')) {
    grid_dimnames <- attr(grid, 'dimension_names')
  } else {
    grid_dimnames <- dimnames(grid)
  }
  has_time_grid <- 'time' %in% names(grid_dimnames)

  if (has_time_grid) 'RF' else 'C'
}
