#' @export
read_hemco_base_emissions_portion <- function(file) {
  lines <- readLines(file)

  output <- NULL
  for (line in lines) {
    if (grepl('^\\s*#', line) || grepl('^\\s*$', line)) next
    parts <- strsplit(line, '\\s+')[[1]]
    output <- c(output, list(hemco_base_emission_field(
      extension_number = parts[1],
      name = parts[2],
      source_file = parts[3],
      source_variable = parts[4],
      source_time = parts[5],
      cre = parts[6],
      source_dimension = parts[7],
      source_unit = parts[8],
      species = parts[9],
      scale_factors = parts[10],
      category = parts[11],
      hierarchy = parts[12]
    )))
  }
  output
}

#' @export
hemco_meteorology_gmao <- function() {
  read_hemco_base_emissions_portion(
    system.file(
      'extdata',
      'HEMCO_Config.rc.gmao_metfields',
      package = 'wombatbasis14'
    )
  )
}

#' @export
hemco_olson_landmap <- function() {
  read_hemco_base_emissions_portion(
    system.file(
      'extdata',
      'HEMCO_Config.rc.olson_landmap',
      package = 'wombatbasis14'
    )
  )
}

#' @export
hemco_yuan_modis_lai <- function() {
  read_hemco_base_emissions_portion(
    system.file(
      'extdata',
      'HEMCO_Config.rc.yuan_modis_lai',
      package = 'wombatbasis14'
    )
  )
}

#' @export
hemco_geos_chem_restart <- function() {
  list(
    hemco_base_emission_field(
      extension_number = '*',
      name = 'SPC_',
      source_file = './Restarts/GEOSChem.Restart.$YYYY$MM$DD_$HH$MNz.nc4',
      source_variable = 'SpeciesRst_?ALL?',
      source_time = '$YYYY/$MM/$DD/$HH',
      cre = 'EFYO',
      source_dimension = 'xyz',
      source_unit = '1',
      species = '*',
      scale_factors = NULL,
      category = 1,
      hierarchy = 1
    )
  )
}

#' @export
hemco_timezones <- function() {
  list(
    hemco_base_emission_field(
      extension_number = '0',
      name = 'TIMEZONES',
      source_file = '$ROOT/TIMEZONES/v2024-02/timezones_vohra_2017_0.1x0.1.nc',
      source_variable = 'UTC_OFFSET',
      source_time = '2017/1-12/1/0',
      cre = 'C',
      source_dimension = 'xy',
      source_unit = 'count',
      species = '*',
      scale_factors = NULL,
      category = NULL,
      hierarchy = 1
    )
  )
}
