#' Reset the the environment
#'
#' `the` is an environment inside this package. This function resets it.
#'
#' @return `NULL` (invisibly).
#' @export
reset_the <- function() {
  rm(list = rlang::env_names(the), envir = the)
  return(invisible(NULL))
}

.set_the <- function(...) {
  rlang::env_bind(the, ...)
}
