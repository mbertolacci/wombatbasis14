# As per http://stackoverflow.com/questions/14500707/select-along-one-of-n-dimensions-in-array
.index_array <- function(x, dim, i, drop = FALSE) {
  # Create list representing arguments supplied to [
  # bquote() creates an object corresponding to a missing argument
  indices <- rep(list(bquote()), length(dim(x)))
  indices[[dim]] <- i

  # Generate the call to [
  call <- as.call(c(
    list(as.name('['), quote(x)),
    indices,
    list(drop = drop)
  ))
  # Finally, evaluate it
  eval(call)
}

.glue_if_glue <- function(x, .envir = parent.frame()) {
  if (inherits(x, 'glue')) {
    glue::glue(x, .envir = .envir)
  } else {
    x
  }
}

.glue_recursive <- function(x, .envir = parent.frame()) {
  if (is.list(x)) {
    lapply(x, .glue_recursive, .envir = .envir)
  } else if (inherits(x, 'glue')) {
    glue::glue(x, .envir = .envir)
  } else {
    x
  }
}
