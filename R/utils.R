log_now <- function() {
  now <- format(
    Sys.time(),
    format = "%F %T"
  )
  return(
    glue::glue("{now} |")
  )
}

.key_get_chill <- function(key_name) {
  tryCatch(
    keyring::key_get(key_name),
    error = function(e) {
      return(NULL)
    }
  )
}

.glue_special <- function(
  ...,
  .sep = " ",
  .open = "{{",
  .close = "}}",
  .envir = rlang::caller_env()
) {
  return(
    glue::glue(
      ...,
      .sep = .sep,
      .open = .open,
      .close = .close,
      .envir = .envir
    )
  )
}
