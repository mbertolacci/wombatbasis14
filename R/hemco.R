#' @export
hemco_configuration <- function(
  settings,
  extension_switches,
  base_emissions = hemco_base_emissions(),
  scale_factors = hemco_scale_factors(),
  masks = hemco_masks()
) {
  structure(list(
    settings = settings,
    extension_switches = extension_switches,
    base_emissions = base_emissions,
    scale_factors = scale_factors,
    masks = masks
  ), class = 'hemco_configuration')
}

#' @export
as.character.hemco_configuration <- function(x, ...) {
  sprintf(
    '%s

%s

%s

%s

%s',
    as.character(x$settings, ...),
    as.character(x$extension_switches, ...),
    as.character(x$base_emissions, ...),
    as.character(x$scale_factors, ...),
    as.character(x$masks, ...)
  )
}

#' @export
print.hemco_configuration <- function(x, ...) {
  cat(as.character(x, ...))
}

#' @export
hemco_settings <- function(
  root_directory,
  meteorology_directory,
  gcap_scenario = 'not_used',
  gcap_vertical_resolution = 47,
  log_file = '*',
  diagnostics_file = 'HEMCO_Diagn.rc',
  diagnostics_prefix = './OutputDir/HEMCO_diagnostics',
  diagnostics_frequency = 'Monthly',
  diagnostics_time_stamp = 'Mid',
  wildcard = '*',
  separator = '/',
  unit_tolerance = 1,
  negative_values = 2,
  only_unitless_scale_factors = FALSE,
  verbose = FALSE,
  verbose_on_cores = c('root', 'all'),
  extra = ''
) {
  verbose_on_cores <- match.arg(verbose_on_cores)
  structure(list(
    root_directory = root_directory,
    meteorology_directory = meteorology_directory,
    gcap_scenario = gcap_scenario,
    gcap_vertical_resolution = gcap_vertical_resolution,
    log_file = log_file,
    diagnostics_file = diagnostics_file,
    diagnostics_prefix = diagnostics_prefix,
    diagnostics_frequency = diagnostics_frequency,
    diagnostics_time_stamp = diagnostics_time_stamp,
    wildcard = wildcard,
    separator = separator,
    unit_tolerance = unit_tolerance,
    negative_values = negative_values,
    only_unitless_scale_factors = only_unitless_scale_factors,
    verbose = verbose,
    verbose_on_cores = verbose_on_cores,
    extra = extra
  ), class = c('hemco_settings', 'hemco_configuration_part'))
}

#' @export
as.character.hemco_settings <- function(x, .envir = parent.frame(), ...) {
  x_glued <- .glue_recursive(x, .envir)
  sprintf(
    '###############################################################################
### BEGIN SECTION SETTINGS
###############################################################################

ROOT: %s
METDIR: %s
GCAPSCENARIO: %s
GCAPVERTRES: %d
Logfile: %s
DiagnFile: %s
DiagnPrefix: %s
DiagnFreq: %s
DiagnTimeStamp: %s
Wildcard: %s
Separator: %s
Unit tolerance: %d
Negative values: %d
Only unitless scale factors: %s
Verbose: %s
VerboseOnCores: %s
%s
### END SECTION SETTINGS ###',
    x_glued$root_directory,
    x_glued$meteorology_directory,
    x_glued$gcap_scenario,
    x_glued$gcap_vertical_resolution,
    x_glued$log_file,
    x_glued$diagnostics_file,
    x_glued$diagnostics_prefix,
    x_glued$diagnostics_frequency,
    x_glued$diagnostics_time_stamp,
    x_glued$wildcard,
    x_glued$separator,
    x_glued$unit_tolerance,
    x_glued$negative_values,
    x_glued$only_unitless_scale_factors,
    x_glued$verbose,
    x_glued$verbose_on_cores,
    x$extra
  )
}

#' @export
hemco_extension_switches <- function(
  ...,
  extra = ''
) {
  structure(
    list(...),
    class = c('hemco_extension_switches', 'hemco_configuration_part'),
    extra = extra
  )
}

#' @export
as.character.hemco_extension_switches <- function(x, .envir = parent.frame(), ...) {
  sprintf(
    '###############################################################################
### BEGIN SECTION EXTENSION SWITCHES
###############################################################################

# ExtNr ExtName           on/off  Species
%s
%s
END SECTION EXTENSION SWITCHES',
    paste0(
      sapply(x, as.character, .envir = .envir, ...),
      collapse = '\n'
    ),
    .glue_if_glue(attr(x, 'extra'), .envir)
  )
}

#' @export
hemco_extension_switch <- function(
  number,
  name,
  enabled,
  species,
  collections
) {
  structure(
    list(
      number = number,
      name = name,
      enabled = enabled,
      species = species,
      collections = collections
    ),
    class = c('hemco_extension_switch', 'hemco_configuration_part')
  )
}

#' @export
as.character.hemco_extension_switch <- function(x, .envir = parent.frame(), ...) {
  x_glued <- .glue_recursive(x, .envir)
  sprintf(
    '%d %s %s %s
%s
# ----------------------------------------------------------------------------',
    x_glued$number,
    x_glued$name,
    if (x_glued$enabled) 'on' else 'off',
    x_glued$species,
    paste0(
      sprintf(
        '    --> %s: %s',
        names(x_glued$collections),
        ifelse(x_glued$collections, 'true', 'false')
      ),
      collapse = '\n'
    )
  )
}

#' @export
hemco_base_emissions <- function(
  ...,
  extra = ''
) {
  structure(
    list(...),
    class = c('hemco_base_emissions', 'hemco_configuration_part'),
    extra = extra
  )
}

#' @export
as.character.hemco_base_emissions <- function(x, .envir = parent.frame(), ...) {
  sprintf(
    '###############################################################################
### BEGIN SECTION BASE EMISSIONS
###############################################################################

# ExtNr Name sourceFile sourceVar sourceTime C/R/E SrcDim SrcUnit Species ScalIDs Cat Hier
%s
%s

### END SECTION BASE EMISSIONS ###',
    paste0(
      sapply(x, as.character, .envir = .envir, ...),
      collapse = '\n'
    ),
    .glue_if_glue(attr(x, 'extra'), .envir)
  )
}

#' @export
hemco_base_emission_collection <- function(
  name,
  fields
) {
  if (is(fields, 'hemco_base_emission_field')) {
    fields <- list(fields)
  }
  stopifnot(
    all(sapply(fields, is, 'hemco_base_emission_field'))
  )
  structure(list(
    name = name,
    fields = fields
  ), class = c('hemco_base_emission_collection', 'hemco_configuration_part'))
}

#' @export
as.character.hemco_base_emission_collection <- function(x, .envir = parent.frame(), ...) {
  sprintf(
    '(((%s
%s
)))%s',
    .glue_if_glue(x$name, .envir),
    paste0(
      sapply(x$fields, as.character, .envir = .envir, ...),
      collapse = '\n'
    ),
    .glue_if_glue(x$name, .envir)
  )
}

#' @export
hemco_base_emission_field <- function(
  extension_number = 0,
  name,
  source_file,
  source_variable,
  source_time,
  cre,
  source_dimension,
  source_unit,
  species,
  scale_factors = NULL,
  category,
  hierarchy
) {
  if (is(scale_factors, 'hemco_scale_factor')) {
    scale_factors <- list(scale_factors)
  }
  scale_factors <- sapply(scale_factors, function(x) {
    if (is(x, 'hemco_scale_factor')) x$id else x
  })
  structure(list(
    extension_number = extension_number,
    name = name,
    source_file = source_file,
    source_variable = source_variable,
    source_time = source_time,
    cre = cre,
    source_dimension = source_dimension,
    source_unit = source_unit,
    species = species,
    scale_factors = scale_factors,
    category = category,
    hierarchy = hierarchy
  ), class = c('hemco_base_emission_field', 'hemco_configuration_part'))
}

#' @export
as.character.hemco_base_emission_field <- function(x, .envir = parent.frame(), ...) {
  x_glued <- .glue_recursive(x, .envir)
  sprintf(
    '%s %s %s %s %s %s %s %s %s %s %s %s',
    .null_as_dash(x_glued$extension_number),
    .null_as_dash(x_glued$name),
    .null_as_dash(x_glued$source_file),
    .null_as_dash(x_glued$source_variable),
    .null_as_dash(x_glued$source_time),
    .null_as_dash(x_glued$cre),
    .null_as_dash(x_glued$source_dimension),
    .null_as_dash(x_glued$source_unit),
    .null_as_dash(x_glued$species),
    if (is.null(x_glued$scale_factors) || length(x_glued$scale_factors) == 0) {
      '-'
    } else {
      paste0(x_glued$scale_factors, collapse = '/')
    },
    .null_as_dash(x_glued$category),
    .null_as_dash(x_glued$hierarchy)
  )
}

#' @export
optimise_base_emissions <- function(x) {
  .optimise_fields <- function(fields) {
    repeatable_fields <- c(
      'source_file', 'source_variable', 'source_time', 'cre',
      'source_dimension', 'source_unit'
    )
    # Reorder fields so that repeatable will be adjacent
    fields <- fields[do.call(order, lapply(
      repeatable_fields,
      function(name) {
        sapply(fields, getElement, name)
      }
    ))]

    get_fields <- function(z) sapply(
      repeatable_fields,
      function(name) getElement(z, name)
    )
    lapply(seq_along(fields), function(i) {
      field <- fields[[i]]
      if (i == 1) return(field)
      if (all(get_fields(field) == get_fields(fields[[i - 1]]))) {
        for (name in repeatable_fields) {
          field[[name]] <- '-'
        }
      }
      field
    })
  }

  if (is(x, 'hemco_base_emissions')) {
    filter_to <- function(name) {
      Filter(function(z) is(z, name), x)
    }
    fields <- .optimise_fields(filter_to('hemco_base_emission_field'))
    collections <- lapply(
      filter_to('hemco_base_emission_collection'),
      optimise_base_emissions
    )

    do.call(
      hemco_base_emissions,
      c(fields, collections, list(extra = attr(x, 'extra')))
    )
  } else if (is(x, 'hemco_base_emission_collection')) {
    x$fields <- .optimise_fields(x$fields)
    x
  } else {
    stop('object class not supported')
  }
}

#' @export
hemco_scale_factors <- function(
  ...,
  extra = ''
) {
  structure(
    list(...),
    class = c('hemco_scale_factors', 'hemco_configuration_part'),
    extra = extra
  )
}

#' @export
as.character.hemco_scale_factors <- function(x, .envir = parent.frame(), ...) {
  sprintf(
    '###############################################################################
### BEGIN SECTION SCALE FACTORS
###############################################################################

# ScalID Name sourceFile sourceVar sourceTime C/R/E SrcDim SrcUnit Oper
%s
%s

### END SECTION SCALE FACTORS ###',
    paste0(
      sapply(x, as.character, .envir = .envir, ...),
      collapse = '\n'
    ),
    .glue_if_glue(attr(x, 'extra'), .envir)
  )
}

#' @export
hemco_scale_factor <- function(
  id,
  name,
  source_file,
  source_variable,
  source_time,
  cre,
  source_dimension,
  source_unit,
  operator,
  mask = NULL
) {
  stopifnot(is.null(mask) || is(mask, 'hemco_mask'))
  structure(list(
    id = id,
    name = name,
    source_file = source_file,
    source_variable = source_variable,
    source_time = source_time,
    cre = cre,
    source_dimension = source_dimension,
    source_unit = source_unit,
    operator = operator,
    mask = mask
  ), class = c('hemco_scale_factor', 'hemco_configuration_part'))
}

#' @export
as.character.hemco_scale_factor <- function(x, .envir = parent.frame(), ...) {
  x_glued <- .glue_recursive(x, .envir)
  sprintf(
    '%d %s %s %s %s %s %s %s %d%s',
    x_glued$id,
    .glue_if_glue(x_glued$name, .envir),
    .null_as_dash(x_glued$source_file),
    .null_as_dash(x_glued$source_variable),
    .null_as_dash(x_glued$source_time),
    .null_as_dash(x_glued$cre),
    x_glued$source_dimension,
    x_glued$source_unit,
    x_glued$operator,
    if (is.null(x_glued$mask)) '' else sprintf(' %s', x_glued$mask$id)
  )
}

#' @export
hemco_masks <- function(
  ...,
  extra = ''
) {
  structure(
    list(...),
    class = c('hemco_masks', 'hemco_configuration_part'),
    extra = extra
  )
}

#' @export
as.character.hemco_masks <- function(x, .envir = parent.frame(), ...) {
  sprintf(
    '###############################################################################
### BEGIN SECTION MASKS
###############################################################################

# ScalID Name sourceFile sourceVar sourceTime C/R/E SrcDim SrcUnit Oper Lon1/Lat1/Lon2/Lat2
%s
%s

### END SECTION MASKS ###',
    paste0(
      sapply(x, as.character, .envir = .envir, ...),
      collapse = '\n'
    ),
    .glue_if_glue(attr(x, 'extra'), .envir)
  )
}

#' @export
hemco_mask <- function(
  id,
  name,
  source_file,
  source_variable,
  source_time,
  cre,
  source_dimension,
  source_unit,
  operator,
  box
) {
  structure(list(
    id = id,
    name = name,
    source_file = source_file,
    source_variable = source_variable,
    source_time = source_time,
    cre = cre,
    source_dimension = source_dimension,
    source_unit = source_unit,
    operator = operator,
    box = box
  ), class = c('hemco_mask', 'hemco_configuration_part'))
}

#' @export
as.character.hemco_mask <- function(x, .envir = parent.frame(), ...) {
  sprintf(
    '%d %s %s %s %s %s %s %s %d %s',
    x$id,
    .glue_if_glue(x$name, .envir),
    .glue_if_glue(x$source_file, .envir),
    .glue_if_glue(x$source_variable, .envir),
    .glue_if_glue(x$source_time, .envir),
    .glue_if_glue(x$cre, .envir),
    .glue_if_glue(x$source_dimension, .envir),
    .glue_if_glue(x$source_unit, .envir),
    x$operator,
    .glue_if_glue(x$box, .envir)
  )
}

#' @export
print.hemco_configuration_part <- function(x, ...) {
  cat(as.character(x, ...))
}

.null_as_dash <- function(value) {
  if (is.null(value)) '-' else value
}
