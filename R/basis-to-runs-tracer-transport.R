.basis_to_runs_tracer_transport <- function(
  basis,
  base_run,
  groups,
  locked_base_emissions,
  base_species_database_entry,
  max_species_per_run = 12
) {
  all_start_times <- do.call(c, lapply(basis$basis_functions, getElement, 'start_time'))

  start_times <- sort(unique(all_start_times))
  if (missing(groups)) {
    split_groups <- factor(format(all_start_times, '%Y%m%d'))
    split_start_times <- start_times
  } else {
    groups <- factor(groups)
    all_split_groups <- interaction(
      groups,
      format(all_start_times, '%Y%m%d'),
      sep = '_',
      drop = FALSE
    )
    split_groups <- droplevels(all_split_groups)
    split_start_times <- expand.grid(
      group = levels(groups),
      start_time = start_times
    )$start_time[
      levels(all_split_groups) %in% levels(split_groups)
    ]
  }
  by_split_groups <- lapply(
    unname(split(
      basis$basis_functions,
      as.integer(split_groups)
    )),
    function(tracers) {
      tracers <- c(list('background'), tracers)
      unname(split(
        tracers,
        1L + floor((seq_along(tracers) - 1L) / max_species_per_run)
      ))
    }
  )

  flux_field_names <- sapply(basis$flux_fields, getElement, 'name')
  flux_fields_by_name <- basis$flux_fields
  names(flux_fields_by_name) <- flux_field_names

  scaling_grid_names <- sapply(basis$scaling_grids, getElement, 'name')
  scaling_grids_by_name <- basis$scaling_grids
  names(scaling_grids_by_name) <- scaling_grid_names

  c(list(list(
    name = 'base',
    configuration = base_run
  )), do.call(c, lapply(seq_len(nlevels(split_groups)), function(index) {
    parts <- by_split_groups[[index]]

    split_group_end_date <- max(do.call(c, lapply(parts, function(run_basis_functions) {
      do.call(c, lapply(run_basis_functions, function(basis_function) {
        if (is.character(basis_function) && basis_function == 'background') {
          return(as.POSIXct(NA, tz = 'UTC'))
        } else {
          basis_function$end_time
        }
      }))
    })), na.rm = TRUE)

    lapply(seq_along(parts), function(part_index) {
      configuration <- base_run
      run_basis_functions <- parts[[part_index]]
      run_split_group <- levels(split_groups)[index]
      run_name <- sprintf('%s_part%03d', run_split_group, part_index)

      tracer_names <- sprintf(
        'r%04dp%03ds%03d',
        index,
        part_index,
        seq_along(parts[[part_index]])
      )

      configuration$main$simulation$start_date <- split_start_times[[index]]
      configuration$main$simulation$end_date <- split_group_end_date
      configuration$main$operations$transport$transported_species <- tracer_names
      species_database_input <- list()
      for (tracer_index in seq_along(tracer_names)) {
        output <- base_species_database_entry
        output$FullName <- if (is.character(run_basis_functions[[tracer_index]])) {
          'background'
        } else {
          run_basis_functions[[tracer_index]]$name
        }
        species_database_input[[tracer_names[[tracer_index]]]] <- output
      }
      configuration$species_database <- geoschem_species_database(species_database_input)

      scaling_grid_names <- unique(do.call(c, lapply(run_basis_functions, function(basis_function) {
        if (is.character(basis_function) && basis_function == 'background') {
          return(character(0))
        }
        basis_function$scaling_grid_name
      })))

      scale_factors <- lapply(seq_along(scaling_grid_names), function(i) {
        scaling_grid_name <- scaling_grid_names[[i]]
        scaling_grid <- scaling_grids_by_name[[scaling_grid_name]]

        hemco_scale_factor(
          id = hemco_max_scale_factor_id(configuration$hemco) + i,
          name = scaling_grid$name,
          source_file = scaling_grid$source_file,
          source_variable = scaling_grid$source_variable,
          source_time = scaling_grid$source_time,
          cre = scaling_grid$cre,
          source_dimension = 'xy',
          source_unit = '1',
          operator = 1
        )
      })
      names(scale_factors) <- scaling_grid_names

      for (scale_factor in scale_factors) {
        configuration$hemco$scale_factors[[length(configuration$hemco$scale_factors) + 1]] <- scale_factor
      }

      base_emission_fields <- list()
      for (base_emission_field in base_run$hemco$base_emissions) {
        if (base_emission_field$name %in% locked_base_emissions) {
          base_emission_fields <- c(base_emission_fields, list(base_emission_field))
        }
      }
      configuration$hemco$base_emissions <- do.call(
        hemco_base_emissions,
        c(
          base_emission_fields,
          list(
            extra = attr(base_run$hemco$base_emissions, 'extra')
          )
        )
      )
      for (tracer_index in seq_along(run_basis_functions)) {
        basis_function <- run_basis_functions[[tracer_index]]
        if (is.character(basis_function) && basis_function == 'background') next

        for (flux_field_name in basis_function$flux_field_names) {
          flux_field <- flux_fields_by_name[[flux_field_name]]
          flux_field$name <- basis_function$name
          configuration$hemco <- add_flux_field_to_hemco(
            configuration$hemco,
            flux_field,
            species = tracer_names[tracer_index],
            scale_factors = scale_factors[[basis_function$scaling_grid_name]]$id,
            category = 1,
            hierarchy = 1
          )
        }
      }

      configuration$hemco_diagnostics <- do.call(hemco_diagnostics, lapply(
        seq_along(run_basis_functions),
        function(tracer_index) {
          hemco_diagnostic(
            sprintf('Emis_%s', tracer_names[tracer_index]),
            tracer_names[tracer_index],
            -1,
            -1,
            -1,
            3,
            'kg/m2/s',
            tracer_names[tracer_index]
          )
        }
      ))

      configuration$symlinks[['gcclassic']] <- file.path('../base', 'gcclassic')

      list(
        name = run_name,
        split_group = run_split_group,
        configuration = configuration,
        mapping = data.frame(
          basis_function = sapply(run_basis_functions, function(basis_function) {
            if (is.character(basis_function) && basis_function == 'background') {
              sprintf('background_%s', run_split_group)
            } else {
              basis_function$name
            }
          }),
          run = run_name,
          species = tracer_names,
          stringsAsFactors = FALSE
        )
      )
    })
  })))
}
