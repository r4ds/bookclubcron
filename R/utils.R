log_now <- function() {
  now <- format(
    Sys.time(),
    format = "%F %T"
  )
  return(
    glue::glue("{now} |")
  )
}
