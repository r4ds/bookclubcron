log_now <- function() {
  return(
    glue::glue("{lubridate::now()} |")
  )
}
