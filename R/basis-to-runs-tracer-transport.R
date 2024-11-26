.basis_to_runs_tracer_transport <- function(
  basis,
  base_run,
  groups,
  locked_base_emissions,
  base_species_database_entry,
  max_species_per_run = 12
) {
  max_scale_factor_id <- if (length(base_run$hemco$scale_factors) > 0) {
    max(sapply(
      base_run$hemco$scale_factors,
      getElement,
      'id'
    ))
  } else {
    0
  }

  all_start_times <- .basis_function_start_times(basis, base_run)

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

  c(list(list(
    name = 'base',
    configuration = base_run
  )), do.call(c, lapply(seq_len(nlevels(split_groups)), function(index) {
    parts <- by_split_groups[[index]]

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

      base_emission_fields <- list()
      for (tracer_index in seq_along(run_basis_functions)) {
        basis_function <- run_basis_functions[[tracer_index]]
        if (is.character(basis_function) && basis_function == 'background') next
        for (base_emission_field in base_run$hemco$base_emissions) {
          if (base_emission_field$name %in% basis_function$fields) {
            base_emission_field$name <- sprintf(
              '%s_%03d',
              base_emission_field$name,
              tracer_index
            )
            base_emission_field$species <- tracer_names[tracer_index]
            base_emission_field$scale_factors <- c(
              base_emission_field$scale_factors,
              max_scale_factor_id + tracer_index
            )

            base_emission_fields <- c(base_emission_fields, list(base_emission_field))
          }
        }
      }
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

        scaling_grid <- basis_function$scaling_grid

        filename <- sprintf('basis-scaling-%03d.nc', tracer_index)
        has_time_grid <- 'time' %in% names(dimnames(scaling_grid))
        scaling_scale_factor <- hemco_scale_factor(
          id = max_scale_factor_id + tracer_index,
          name = sprintf('BASISSCALE_%03d', tracer_index),
          source_file = filename,
          source_variable = 'value',
          # TODO(mgnb): do better than this
          source_time = if (has_time_grid) {
            .times_to_hemco_source_time(attr(scaling_grid, 'time'))
          } else {
            sprintf(
              '%d/1/1/0',
              lubridate::year(configuration$main$simulation$start_date)
            )
          },
          cre = if (has_time_grid) 'RF' else 'C',
          source_dimension = 'xy',
          source_unit = '1',
          operator = 1
        )
        configuration$hemco$scale_factors[[
          sprintf('scaling_%03d', tracer_index)
        ]] <- scaling_scale_factor

        configuration$files[[filename]] <- scaling_grid
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
