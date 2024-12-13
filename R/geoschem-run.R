#' @export
geoschem_run_configuration <- function(
  main,
  history,
  hemco,
  hemco_diagnostics,
  species_database,
  code_directory,
  makefile = 'default',
  get_run_info = 'default',
  extra_files = list(),
  extra_directories = c(),
  symlinks = list()
) {
  files <- c(list(), extra_files)

  add_file_or_default <- function(filename, contents, mode) {
    if (!is.null(contents)) {
      if (contents == 'default') {
        contents <- paste0(readLines(system.file(
          'extdata',
          filename,
          package = 'wombatbasis14'
        )), collapse = '\n')
      }
      if (!missing(mode)) {
        attr(contents, 'mode') <- mode
      }
      files[[filename]] <<- contents
    }
  }

  if (!missing(code_directory)) {
    symlinks[['CodeDir']] <- code_directory
  }

  structure(list(
    main = main,
    history = history,
    hemco = hemco,
    hemco_diagnostics = hemco_diagnostics,
    species_database = species_database,
    files = files,
    directories = extra_directories,
    symlinks = symlinks
  ), class = 'geoschem_run_configuration')
}

#' @export
print.geoschem_run_configuration <- function(x, ...) {
  cat(sprintf(
    '==================================================
geoschem_config.yml
==================================================
%s

==================================================
HISTORY.rc
==================================================
%s

==================================================
HEMCO_Config.rc
==================================================
%s

==================================================
HEMCO_Diagn.rc
==================================================
%s

==================================================
species_database.yml
==================================================
%s
',
    as.character(x$main),
    as.character(x$history),
    as.character(x$hemco),
    as.character(x$hemco_diagnostics),
    as.character(x$species_database)
  ))
}

#' @export
write_geoschem_run <- function(run_configuration, run_directory) {
  if (dir.exists(run_directory)) {
    warning('Run directory already exists')
  }

  create_dirname <- function(path) {
    dir.create(
      file.path(run_directory, dirname(path)),
      recursive = TRUE,
      showWarnings = FALSE,
      mode = '0755'
    )
  }

  files <- run_configuration$files
  files[['geoschem_config.yml']] <- as.character(run_configuration$main)
  files[['HISTORY.rc']] <- as.character(run_configuration$history)
  files[['HEMCO_Config.rc']] <- as.character(run_configuration$hemco)
  files[['HEMCO_Diagn.rc']] <- as.character(run_configuration$hemco_diagnostics)
  files[['species_database.yml']] <- as.character(run_configuration$species_database)

  for (path in names(files)) {
    create_dirname(path)
    if (is.character(files[[path]])) {
      cat(files[[path]], file = file.path(run_directory, path))
      if ('mode' %in% names(attributes(files[[path]]))) {
        Sys.chmod(file.path(run_directory, path), attr(files[[path]], 'mode'))
      }
    } else if (inherits(files[[path]], 'grid_data') || inherits(files[[path]], 'rle_grid_data')) {
      write_nc_grid_data(files[[path]], file.path(run_directory, path))
    }
  }

  for (directory_name in run_configuration$directories) {
    dir.create(
      file.path(run_directory, directory_name),
      recursive = TRUE,
      showWarnings = FALSE,
      mode = '0755'
    )
  }

  symlinks <- run_configuration$symlinks
  for (path in names(symlinks)) {
    create_dirname(path)
    unlink(file.path(run_directory, path))
    file.symlink(symlinks[[path]], file.path(run_directory, path))
  }

  invisible(NULL)
}

#' @export
hemco_max_scale_factor_id <- function(hemco) {
  if (length(hemco$scale_factors) > 0) {
    max(sapply(
      hemco$scale_factors,
      getElement,
      'id'
    ))
  } else {
    0
  }
}

#' @export
add_flux_field_to_hemco <- function(hemco, flux_field, ...) {
  base_emissions_entry <- do.call(
    hemco_base_emission_field,
    c(flux_field[c(
      'name',
      'source_file',
      'source_variable',
      'source_time',
      'cre',
      'source_dimension',
      'source_unit'
    )], list(...)
  ))
  if (flux_field$multiply_by != 1) {
    max_scale_factor_id <- hemco_max_scale_factor_id(hemco)
    multiply_by_scale_factor <- hemco_scale_factor(
      id = max_scale_factor_id + 1,
      name = sprintf('%s_MULTIPLY_BY', flux_field$name),
      source_file = '-1.0',
      source_variable = NULL,
      source_time = '2000/1/1/0',
      cre = 'C',
      source_dimension = 'xy',
      source_unit = '1',
      operator = 1
    )
    hemco$scale_factors[[length(hemco$scale_factors) + 1]] <- multiply_by_scale_factor
    base_emissions_entry$scale_factors <- c(base_emissions_entry$scale_factors, multiply_by_scale_factor$id)
  }
  hemco$base_emissions[[length(hemco$base_emissions) + 1]] <- base_emissions_entry
  hemco
}
