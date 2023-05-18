#' Check YouTube quota space
#'
#' Check the "dev-monitoring" R4DS Slack channel for warnings about quota usage.
#'
#' @return If there's room, `TRUE` invisibly (error otherwise).
#' @keywords internal
.check_can_upload <- function() {
  slack_channels <- r4ds_slack_channels()

  # Check monitoring channel for quota info.
  monitor_channel_id <- slack_channels$id[
    slack_channels$name == "dev-monitoring"
  ]
  monitoring_channel_msgs <- slackthreads::conversations(
    monitor_channel_id,
    max_results = 2,
    limit = 2
  ) |> purrr::keep(
    \(x) {
      "bot_id" %in% names(x) &&
        x[["bot_id"]] == "B055C7R32LV" &&
        lubridate::as_datetime(as.numeric(x[["ts"]])) > (
          # This probably needs to be refined. Is the quote the last 24 hours? Per
          # calendar day?
          lubridate::now() - lubridate::hours(24)
        ) &&
        x[["text"]] != "Test Notification"
      # Eventually add more info here to check for the alerts I actually care
      # about, but I need an example to exist to make sure I get that right. This
      # is the only message this bot can produce right now, though, so no rush.
    }
  )

  if (length(monitoring_channel_msgs)) {
    cli::cli_abort(
      "{log_now()} YouTube quota reached. Process tomorrow!"
    )
  }

  return(invisible(TRUE))
}

#' Cache or Fetch R4DS Slack channels
#'
#' Fetch public and private R4DS Slack channel information.
#'
#' @inheritParams .fetch_r4ds_youtube_playlists
#' @param refresh Get fresh data?
#'
#' @inherit .fetch_r4ds_youtube_playlists return
#' @export
r4ds_youtube_playlists <- function(n = 50L, refresh = FALSE) {
  if (refresh) {
    .cache_r4ds_youtube_playlists(n)
    return(the$youtube_playlists)
  }

  return(
    rlang::env_cache(
      the,
      "youtube_playlists",
      .fetch_r4ds_youtube_playlists(n)
    )
  )
}

#' Cache R4DS YouTube playlists
#'
#' Set R4DS YouTube playlists in the package `the` environment.
#'
#' @inheritParams .fetch_r4ds_youtube_playlists
#'
#' @return A character vector of playlist IDs, with titles as names, invisibly.
#' @keywords internal
.cache_r4ds_youtube_playlists <- function(n) {
  return(
    rlang::env_bind(
      the,
      youtube_playlists = .fetch_r4ds_youtube_playlists(n)
    )
  )
}

#' Fetch R4DS YouTube Playlists
#'
#' @param n How many playlists do we need? This should ideally be equal to the
#'   number of active clubs.
#'
#' @return A character vector of playlist IDs, with titles as names.
#' @keywords internal
.fetch_r4ds_youtube_playlists <- function(n) {
  # TODO: Once this is implemented in youtubeR, replace this with a call to that
  # function.
  raw_playlists <- youtubeR::yt_call_api(
    endpoint = "playlists",
    query = list(
      part = "snippet",
      mine = TRUE,
      maxResults = n
    )
  )$items

  return(
    rlang::set_names(
      purrr::map_chr(raw_playlists, "id"),
      purrr::map_chr(raw_playlists, list("snippet", "title"))
    )
  )
}

#' Process YouTube videos
#'
#' Update YouTube videos, post them to Slack, etc.
#'
#' @return I'm not sure yet.
#' @export
process_youtube <- function() {
  working_video_path <- fs::path(
    rappdirs::user_cache_dir("bookclubcron"),
    "youtube_video_status",
    ext = "rds"
  )
  if (!fs::file_exists(working_video_path)) {
    cli::cli_abort("{log_now()} No working videos!")
  }
  working_yt_videos <- readRDS(working_video_path)

  # TODO: {youtubeR} endpoint
  yt_video_details_raw <- youtubeR::yt_call_api(
    endpoint = "videos",
    query = list(
      id = paste(working_yt_videos$video_id, collapse = ","),
      part = "contentDetails,status",
      max_results = 50L
    )
  )$items

  # Deal with deleted videos (this almost definitely means there was an extra
  # recording for a meeting).
  if (length(yt_video_details_raw) < length(working_yt_videos$video_id)) {
    returned_video_ids <- purrr::map_chr(yt_video_details_raw, "id")
    # Don't do a "keep the ones that are here" in case something new saved.
    # Instead filter out the known missing one(s).
    missing_videos <- setdiff(
      working_yt_videos$video_id,
      returned_video_ids
    )
    readRDS(working_video_path) |>
      dplyr::filter(!(.data$video_id %in% .env$missing_videos)) |>
      saveRDS(working_video_path)
  }

  yt_video_details <- purrr::map(
    yt_video_details_raw,
    \(this_video) {
      this_row <- which(working_yt_videos$video_id == this_video$id)
      channel_name <- working_yt_videos$channel_name[[this_row]]

      if (this_video$status$uploadStatus == "processed") {
        cohort_number <- working_yt_videos$cohort_number[[this_row]]

        status_tbl <- tibble::tibble(
          channel_name = .env$channel_name,
          cohort_number = .env$cohort_number,
          video_id = this_video$id,
          uploaded_duration = lubridate::duration(
            this_video$contentDetails$duration
          ),
          status = "processed"
        )

        vid_url <- glue::glue(
          "https://studio.youtube.com/video/{this_video$id}/editor"
        )

        if (working_yt_videos$status[[this_row]] == "uploaded") {
          msg <- "{log_now()} {channel_name} is {.href [editable]({vid_url})}!"
          cli::cli_alert_info(msg)
          return(status_tbl)
        }
        if (working_yt_videos$status[[this_row]] == "processed") {
          previous_duration <- working_yt_videos$uploaded_duration[[this_row]]
          if (
            status_tbl$uploaded_duration < previous_duration ||
            this_video$status$privacyStatus == "public"
          ) {
            cli::cli_alert_info("{log_now()} {channel_name} is {.emph DONE!}")

            if (this_video$status$privacyStatus != "public") {
              # Make it public.
              youtubeR::yt_videos_update(
                video_id = this_video$id,
                status = youtubeR::yt_schema_video_status(
                  privacy_status = "public"
                )
              )
            }
            slack_channels <- r4ds_slack_channels()

            slack_msg <- glue::glue(
              "The most recent cohort{cohort_number} meeting: ",
              "https://youtu.be/{this_video$id}"
            )

            if (channel_name %in% slack_channels$name) {
              slackposts::chat_message(
                channel = slack_channels$id[
                  slack_channels$name == channel_name
                ],
                text = slack_msg
              )
            } else {
              cli::cli_alert_warning(c(
                "!" = "{log_now()} Cannot find channel {channel_name}.",
                "!" = "Did you change it?",
                "i" = slack_msg
              ))
            }

            status_tbl$processed_duration <- status_tbl$uploaded_duration
            status_tbl$uploaded_duration <- NULL
            status_tbl$status <- "ready"
            return(status_tbl)
          }
        }
        # No change so no need to return anything.
        return(NULL)
      }
      return(NULL)
    }
  ) |>
    purrr::list_rbind()

  if (nrow(yt_video_details)) {
    readRDS(working_video_path) |>
      dplyr::anti_join(yt_video_details, by = "video_id") |>
      dplyr::bind_rows(
        dplyr::filter(yt_video_details, .data$status != "ready")
      ) |>
      saveRDS(working_video_path)
  }
}
