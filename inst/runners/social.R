# TODO:
# - Figure out how to automatically monetize.
# - Take out manual "Update YouTube" step (other than confirmation).
# - Add in auto-posting, with confirmation.
# - Iterate.
# - Turn into an actual function or functions.
# - Add error handling for missing fields.
pkgload::load_all(helpers = FALSE, attach_testthat = FALSE)

process_clubs_manual <- function(max_hours = 29, min_hours = 2) {
  mast_bullets <- character()
  bsky_bullets <- character()
  li_bullets <- character()

  .gs4_auth()
  club_metadata <- googlesheets4::read_sheet(
    "1G5KjY77ONuaHj530ttzrhCS9WN4_muYxfLgP3xK24Cc",
    sheet = "club_metadata_clean"
  )
  day_to_repeat <- lubridate::today(tzone = "UTC")
  if (lubridate::wday(day_to_repeat, week_start = 7) == 1) {
    day_to_repeat <- day_to_repeat - lubridate::days(1)
  }
  this_week <- tibble::tibble(
    date_utc = rep(list(day_to_repeat), 7) |>
      purrr::imap(\(day, i) {
        day - lubridate::days(i - 1)
      }) |>
      unlist() |>
      lubridate::as_date(),
    day_utc = lubridate::wday(date_utc, label = TRUE, abbr = FALSE)
  )

  yesterday_clubs <- club_metadata |>
    dplyr::left_join(this_week, by = "day_utc" ) |>
    dplyr::mutate(
      datetime_utc = lubridate::make_datetime(
        year = lubridate::year(.data$date_utc),
        month = lubridate::month(.data$date_utc),
        day = lubridate::day(.data$date_utc),
        hour = .data$hour_utc
      ),
      datetime_chicago = lubridate::with_tz(.data$datetime_utc, "America/Chicago")
    ) |>
    dplyr::select("cohort_id", "signup_ws_id", "datetime_chicago", "book_title") |>
    dplyr::filter(
      dplyr::between(
        .data$datetime_chicago,
        lubridate::now(tzone = "America/Chicago") - lubridate::hours(max_hours),
        lubridate::now(tzone = "America/Chicago") - lubridate::hours(min_hours)
      )
    ) |>
    dplyr::mutate(
      date_chicago = lubridate::date(.data$datetime_chicago)
    ) |>
    dplyr::arrange(.data$date_chicago)

  if (NROW(yesterday_clubs)) {
    cli::cli_inform(c(
      "Turn on monetization & paste YouTube links into the spreadsheets.",
      i = "You really should grab links automatically."
    ))
    done <- readline("Continue? (enter)")
  }

  for (club_n in seq_len(NROW(yesterday_clubs))) {
    cohort_id <- yesterday_clubs$cohort_id[[club_n]]
    do_club_update <- usethis::ui_yeah(
      "Update {cohort_id}?",
      "Yes", "No", 1, 1, FALSE
    )
    if (do_club_update) {
      ws_id <- yesterday_clubs$signup_ws_id[[club_n]]

      if (is.na(ws_id)) {
        book_title <- yesterday_clubs$book_title[[club_n]] |>
          stringr::str_remove("\\b(C|c)lub\\b") |>
          stringr::str_squish()
        date_chicago <- yesterday_clubs$date_chicago[[club_n]]
        youtube_link <- readline(
          glue::glue("YouTube link for {book_title} Club ({cohort_id}):")
        )
        gs_data <- tibble::tibble(
          mastodon_bullet = glue::glue(
            "🔵 {book_title} Club ({cohort_id}) {youtube_link} #RStats #RShiny"
          ),
          bsky_bullet = mastodon_bullet,
          linked_in_bullet = mastodon_bullet,
          full_you_tube_description = glue::glue(
            "Weekly meeting of the DSLC {book_title} club on {date_chicago}.",
            "Learn more about the club: https://github.com/shiny-meetings/shiny-meetings
",
            "Join the conversation: https://dslc.io",
            .sep = "\n"
          ),
          you_tube_title = glue::glue(
            "DSLC {book_title} Club: Session {date_chicago} ({cohort_id})"
          )
        )
      } else {
        gs_data <- googlesheets4::read_sheet(
          ws_id,
          sheet = "posting"
        ) |>
          janitor::clean_names() |>
          dplyr::filter(
            date <= lubridate::today(),
            !is.na(.data$full_you_tube_description),
            !is.na(.data$you_tube_link)
          ) |>
          dplyr::arrange(dplyr::desc(date)) |>
          head(1)
        if (!NROW(gs_data) || is.na(gs_data$full_you_tube_description)) {
          gs_data <- NULL
          cli::cli_inform("No data for this club this week.")
        }
      }
      if (length(gs_data)) {
        gs_data$full_you_tube_description |> cat()
        done <- readline("Continue? (enter)")
        gs_data$you_tube_title |> cat()
        done <- readline("Continue? (enter)")
        cat("\n")
        if (is.na(gs_data$mastodon_bullet)) {
          stop("Something is wrong with the sheet.")
        }
        mast_bullets <- c(mast_bullets, gs_data$mastodon_bullet)
        bsky_bullets <- c(bsky_bullets, gs_data$bsky_bullet)
        li_bullets <- c(li_bullets, gs_data$linked_in_bullet)
      }
    }
  }

  if (!length(mast_bullets)) {
    mast_bullets <- NULL
  }

  rlang::inform("\n\n\nMASTODON")
  rlang::inform(mast_message_randoms(mast_bullets))
  rlang::inform("\n\n\nBLUESKY")
  rlang::inform(bsky_message_randoms(bsky_bullets))
  rlang::inform("\n\n")
  rlang::inform(bsky_message_randoms())
  rlang::inform("\n\n\nLINKEDIN")
  rlang::inform(li_message_randoms(li_bullets))
  return(invisible(NULL))
}

process_clubs_manual()
# process_clubs_manual(1,1)
# process_clubs_manual(96,1)

rm(process_clubs_manual)
