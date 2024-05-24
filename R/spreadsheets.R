process_announcements <- function() {
  now_utc <- lubridate::now(tzone = "UTC")
  tomorrow_utc <- lubridate::date(now_utc + lubridate::days(1))
  tomorrow_wday_utc <- tomorrow_utc |>
    lubridate::wday(label = TRUE, abbr = FALSE) |>
    as.character()
  now_hour_utc <- lubridate::hour(now_utc)

  clubs_tomorrow <- .active_clubs() |>
    dplyr::filter(
      .data$day_utc == .data$tomorrow_wday_utc,
      .data$hour_utc == .data$now_hour_utc
    )

  clubs_tomorrow |>
    dplyr::bind_cols(
      purrr::map(
        clubs_tomorrow$cohort_id,
        .cohort_meeting_details,
        meeting_date = tomorrow_utc
      ) |>
        purrr::list_rbind()
    )
  stop("Sort out a function for this. Post to the appropriate Slack channel. Watch for 'SKIP', etc.")
}

.active_clubs <- function() {
  .gs4_auth()
  return(
    purrr::quietly(googlesheets4::read_sheet)(
      "1G5KjY77ONuaHj530ttzrhCS9WN4_muYxfLgP3xK24Cc",
      sheet = "Club Metadata",
      col_types = "c"
    )$result |>
      dplyr::filter(
        !is.na(.data$start_date) &
          .data$start_date != "TBD" &
          is.na(.data$end_date)
      ) |>
      dplyr::filter(
        lubridate::as_datetime(
          .data$start_date,
          tz = "America/Chicago"
        ) < lubridate::now()
      ) |>
      dplyr::select(
        "cohort_id",
        "facilitator_id",
        "day_utc",
        "hour_utc",
        "signup_ws_id",
        "book_abbrev",
        "language",
        "book_title",
        "book_authors",
        "book_url"
      )
  )
}

.cohort_signups <- function(cohort_id) {
  .gs4_auth()

  signup_ws_id <- .active_clubs() |>
    dplyr::filter(.data$cohort_id == .env$cohort_id) |>
    dplyr::pull(.data$signup_ws_id)

  return(
    purrr::quietly(googlesheets4::read_sheet)(
      signup_ws_id,
      col_types = "c"
    )$result |>
      dplyr::select(
        tidyselect::any_of(c(
          "topic", "chapter", "date", "presenter"
        ))
      ) |>
      dplyr::mutate(
        date = lubridate::as_date(date)
      )
  )
}

.cohort_meeting_details <- function(cohort_id, meeting_date) {
  return(
    .cohort_signups(cohort_id) |>
      dplyr::filter(date == meeting_date)
  )
}

.gs4_auth <- function() {
  purrr::quietly(googlesheets4::gs4_auth)(
    email = "jonthegeek@gmail.com"
  )
}

