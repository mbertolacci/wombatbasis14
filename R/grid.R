DIMENSION_OPTIONS <- c('longitude', 'latitude', 'time')

#' Construct grids over time, longitude, latitude, and all combinations thereof
#' @export
grid_data <- function(x, longitude, latitude, time) {
  x <- as.array(x)

  has_longitude <- !missing(longitude)
  has_latitude <- !missing(latitude)
  has_time <- !missing(time)

  dimension_names <- c(
    if (has_longitude) 'longitude' else NULL,
    if (has_latitude) 'latitude' else NULL,
    if (has_time) 'time' else NULL
  )
  stopifnot(length(dim(x)) == length(dimension_names))
  dimnames(x) <- rep(list(NULL), length(dimension_names))
  names(dimnames(x)) <- dimension_names

  if (has_longitude) attr(x, 'longitude') <- longitude
  if (has_latitude) attr(x, 'latitude') <- latitude
  if (has_time) attr(x, 'time') <- time

  class(x) <- 'grid_data'

  x
}

#' Construct a time grid from a list of dates
#' @export
times_to_grids <- function(times) {
  output <- lapply(seq_along(times), function(i) {
    x <- rep(0, length(times))
    x[i] <- 1
    grid_data(x, time = times)
  })
  names(output) <- names(times)
  output
}

#' Read NetCDF file to a grid format
#' @export
read_nc_grid_data <- function(
  filename,
  variable_name,
  longitude_name = 'lon',
  latitude_name = 'lat',
  time_name = 'time'
) {
  .with_nc_file(list(fn = filename), {
    output <- lapply(variable_name, function(variable_name_i) {
      x <- ncdf4::ncvar_get(fn, variable_name_i, collapse_degen = FALSE)
      var_dim <- fn$var[[variable_name_i]]$dim
      dim_names <- sapply(var_dim, getElement, 'name')

      longitude_dim <- pmatch(longitude_name, dim_names)
      latitude_dim <- pmatch(latitude_name, dim_names)
      time_dim <- pmatch(time_name, dim_names)

      x <- aperm(x, na.omit(c(longitude_dim, latitude_dim, time_dim)))

      do.call(grid_data, c(
        list(x = x),
        if (!is.na(longitude_dim)) {
          list(longitude = var_dim[[longitude_dim]]$vals)
        } else {
          NULL
        },
        if (!is.na(latitude_dim)) {
          list(latitude = var_dim[[latitude_dim]]$vals)
        } else {
          NULL
        },
        if (!is.na(time_dim)) {
          list(time = .ncvar_get_time(fn, dim_names[time_dim]))
        } else {
          NULL
        }
      ))
    })
  })
  if (length(output) == 1) {
    output[[1]]
  } else {
    names(output) <- variable_name
    output
  }
}

#' Write grid to NetCDF file
#' @export
write_nc_grid_data <- function(
  x,
  filename,
  variable_name = 'value',
  variable_unit = '1',
  variable_longname = 'Scaling factor',
  base_time = lubridate::ymd_hms('2000-01-01 00:00:00')
) {
  if (inherits(x, 'rle_grid_data')) {
    x <- decode_rle_grid(x)
  }

  dimension_names <- names(dimnames(x))
  dimensions <- lapply(dimension_names, function(name) {
    values <- attr(x, name)
    if (name == 'time') {
      # Convert date/datetime objects to offsets
      if (all(lubridate::minute(values) == 0)) {
        resolution <- 'hours'
      } else {
        resolution <- 'minutes'
      }
      time_unit <- sprintf(
        '%s since %s',
        resolution,
        format(base_time, format = '%Y-%m-%d 00:00:00')
      )
      values <- as.integer(round(as.double(
        as.POSIXct(values) - base_time,
        units = resolution
      )))
    } else {
      time_unit <- ''
    }

    ncdf4::ncdim_def(
      name,
      vals = values,
      units = c(
        'longitude' = 'degrees_east',
        'latitude' = 'degrees_north',
        'time' = time_unit
      )[name],
      longname = c(
        'longitude' = 'Longitude',
        'latitude' = 'Latitude',
        'time' = 'Time'
      )[name]
    )
  })

  value_variable <- ncdf4::ncvar_def(
    variable_name,
    units = variable_unit,
    longname = variable_longname,
    dim = dimensions,
    prec = 'double',
    compression = 6
  )

  nc_fn <- ncdf4::nc_create(filename, list(value_variable))
  on.exit(ncdf4::nc_close(nc_fn))
  ncdf4::ncvar_put(
    nc_fn,
    value_variable,
    x
  )
  invisible(NULL)
}

#' @export
multiply_grids <- function(...) {
  # NOTE(mgnb): arguments can either be grids, or lists of grids
  args <- list(...)
  grids <- list()
  for (arg in args) {
    grids <- c(grids, if (is.list(arg)) arg else list(arg))
  }

  if (length(grids) == 0) return(NULL)
  if (length(grids) == 1) return(grids[[1]])
  current <- grids[[1]]
  for (i in 2 : length(grids)) {
    current <- .multiply_grid_pair(current, grids[[i]])
  }
  class(current) <- 'grid_data'
  return(current)
}

# .multiply_grid_pair <- function(grid1, grid2) {
#   dimension_names1 <- names(dimnames(grid1))
#   dimension_names2 <- names(dimnames(grid2))

#   # Determine the dimensions of the output
#   output_dimensions <- list()
#   for (dimension_name in dimension_names1) {
#     output_dimensions[[dimension_name]] <- attr(grid1, dimension_name)
#   }
#   for (dimension_name in dimension_names2) {
#     if (dimension_name %in% names(output_dimensions)) {
#       stopifnot(all(
#         output_dimensions[[dimension_name]] == attr(grid2, dimension_name)
#       ))
#     } else {
#       output_dimensions[[dimension_name]] <- attr(grid2, dimension_name)
#     }
#   }
#   dimension_names_output <- DIMENSION_OPTIONS[
#     DIMENSION_OPTIONS %in% union(dimension_names1, dimension_names2)
#   ]
#   output_dim <- sapply(output_dimensions, length)[dimension_names_output]

#   # Expand grid1 to union grid using recycling; put the dims of grid1 first so
#   # they will be recycled. Then reorder to the correct ordering of output
#   # dimensions
#   other_dims1 <- setdiff(dimension_names_output, dimension_names1)
#   output <- aperm(
#     array(grid1, dim = c(dim(grid1), output_dim[other_dims1])),
#     Matrix::invPerm(match(c(
#       dimension_names1,
#       other_dims1
#     ), dimension_names_output))
#   )
#   # Repeat the strategy for grid2, multiplying with grid1
#   other_dims2 <- setdiff(dimension_names_output, dimension_names2)
#   output <- output * aperm(
#     array(grid2, dim = c(dim(grid2), output_dim[other_dims2])),
#     Matrix::invPerm(match(c(
#       dimension_names2,
#       other_dims2
#     ), dimension_names_output))
#   )

#   dimnames(output) <- rep(list(NULL), length(dimension_names_output))
#   names(dimnames(output)) <- dimension_names_output
#   for (name in names(output_dimensions)) {
#     attr(output, name) <- output_dimensions[[name]]
#   }

#   output
# }

.multiply_grid_pair <- function(grid1, grid2) {
  dimension_names1 <- names(dimnames(grid1))
  dimension_names2 <- names(dimnames(grid2))

  # Determine the dimensions of the output
  output_dimensions <- list()
  for (dimension_name in dimension_names1) {
    output_dimensions[[dimension_name]] <- attr(grid1, dimension_name)
  }
  for (dimension_name in dimension_names2) {
    if (dimension_name %in% names(output_dimensions)) {
      stopifnot(all(
        output_dimensions[[dimension_name]] == attr(grid2, dimension_name)
      ))
    } else {
      output_dimensions[[dimension_name]] <- attr(grid2, dimension_name)
    }
  }
  dimension_names_output <- DIMENSION_OPTIONS[
    DIMENSION_OPTIONS %in% union(dimension_names1, dimension_names2)
  ]
  output_dim <- sapply(output_dimensions, length)[dimension_names_output]

  grid1_expanded_dim <- output_dim
  grid1_expanded_dim[!(dimension_names_output %in% dimension_names1)] <- 1L
  grid2_expanded_dim <- output_dim
  grid2_expanded_dim[!(dimension_names_output %in% dimension_names2)] <- 1L

  dim(grid1) <- grid1_expanded_dim
  dim(grid2) <- grid2_expanded_dim

  if (length(dimension_names_output) == 2) {
    output <- broadcasting_2d_array_multiply(grid1, dim(grid1), grid2, dim(grid2))
  } else {
    output <- broadcasting_3d_array_multiply(grid1, dim(grid1), grid2, dim(grid2))
  }
  dim(output) <- output_dim
  dimnames(output) <- rep(list(NULL), length(dimension_names_output))
  names(dimnames(output)) <- dimension_names_output
  for (name in names(output_dimensions)) {
    attr(output, name) <- output_dimensions[[name]]
  }

  output
}

.truncate_grid_times <- function(x, start = TRUE, end = TRUE) {
  if (!('time' %in% names(dimnames(x)))) {
    return(x)
  }
  time_index <- which(names(dimnames(x)) == 'time')
  n_times <- dim(x)[time_index]

  if (start) {
    for (start_index in seq_len(n_times)) {
      if (any(.index_array(x, time_index, start_index) != 0)) {
        break
      }
    }
  } else {
    start_index <- 1
  }

  if (end) {
    if (start_index == n_times) {
      end_index <- n_times
    } else {
      for (last_non_zero_index in rev(seq_len(n_times))) {
        if (any(.index_array(x, time_index, last_non_zero_index) != 0)) {
          break
        }
      }
      if (last_non_zero_index == n_times) {
        end_index <- n_times
      } else {
        end_index <- last_non_zero_index + 1
      }
    }
  } else {
    end_index <- n_times
  }

  output <- .index_array(x, time_index, start_index : end_index)
  for (name in c('longitude', 'latitude')) {
    if (name %in% names(dimnames(x))) {
      attr(output, name) <- attr(x, name)
    }
  }
  attr(output, 'time') <- attr(x, 'time')[start_index : end_index]
  class(output) <- 'grid_data'
  output
}

#' @export
encode_rle_grid <- function(x) {
  structure(
    list(values = rle(as.vector(x))),
    time = attr(x, 'time'),
    longitude = attr(x, 'longitude'),
    latitude = attr(x, 'latitude'),
    dimensions = dim(x),
    dimension_names = dimnames(x),
    class = 'rle_grid_data'
  )
}

#' @export
decode_rle_grid <- function(x) {
  output <- array(
    inverse.rle(x$values),
    dim = attr(x, 'dimensions'),
    dimnames = attr(x, 'dimension_names')
  )
  attr(output, 'time') <- attr(x, 'time')
  attr(output, 'longitude') <- attr(x, 'longitude')
  attr(output, 'latitude') <- attr(x, 'latitude')
  class(output) <- 'grid_data'
  output
}
