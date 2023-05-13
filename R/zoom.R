#' Process Zoom recordings
#'
#' Check for Zoom recordings, and post them to YouTube if appropriate.
#'
#' @return I'm not sure yet.
#' @export
process_zoom <- function() {
  # TODO: Consider renaming this to something like "process_new_meetings".

  .check_can_upload()

  last_week <- lubridate::today() - lubridate::weeks(1)

  # TODO: {zoomer} endpoint.
  all_recordings <- httr2::request(
    "https://api.zoom.us/v2/users/yntI9xIgSRCk0LvDdaAtIg/recordings"
  ) |>
    zoomer:::.zoom_req_authenticate() |>
    httr2::req_url_query(from = last_week) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (length(all_recordings$meetings)) {
    n <- length(r4ds_active_clubs())
    youtube_playlists <- r4ds_youtube_playlists(n)
    slack_channels <- r4ds_slack_channels()

    working_video_dir <- fs::path(
      rappdirs::user_cache_dir("bookclubcron")
    )
    working_video_path <- fs::path(
      working_video_dir,
      "youtube_video_status",
      ext = "rds"
    )

    for (meeting_num in seq_along(all_recordings$meetings)) {
      this_meeting <- all_recordings$meetings[[meeting_num]]

      meeting_date <- lubridate::date(
        lubridate::with_tz(
          lubridate::ymd_hms(
            this_meeting$start_time
          ),
          "America/Chicago"
        )
      )
      meeting_end <- lubridate::round_date(
        meeting_date, unit = "hours"
      ) + lubridate::hours(1)
      if (meeting_end < lubridate::now()) {
        this_meeting$ready_to_delete <- rep(
          FALSE, length(this_meeting$recording_files)
        )

        cohort_id <- this_meeting$topic

        if (cohort_id == "do4ds01-proj01") {
          meeting_date_day <- lubridate::day(meeting_date)
          if (meeting_date_day > 7 && meeting_date_day < 15) {
            cohort_id <- "proj01"
          } else {
            cohort_id <- "do4ds01"
          }
        }

        book_abbrev <- stringr::str_remove(
          cohort_id,
          "\\d+$"
        ) |>
          # Some clubs have a break before the number.
          stringr::str_remove("(_|-)$")
        cohort_number <- stringr::str_extract(
          cohort_id,
          "\\d+$"
        )
        channel_name <- glue::glue("book_club-{book_abbrev}")

        if (cohort_id == "tmwr_es") {
          channel_name <- "book_club-tmwr"
        }

        for (file_num in seq_along(this_meeting$recording_files)) {
          this_file <- this_meeting$recording_files[[file_num]]
          if (this_file$status == "completed") {
            switch(
              this_file$file_type,
              MP4 = {
                # Make sure it's long enough to bother with.
                start_dt <- lubridate::as_datetime(this_file$recording_start)
                end_dt <- lubridate::as_datetime(this_file$recording_end)
                duration <- end_dt - start_dt

                if (duration > lubridate::minutes(10)) {
                  file_path <- withr::local_tempfile(
                    pattern = paste(
                      cohort_id,
                      lubridate::date(
                        lubridate::with_tz(
                          lubridate::ymd_hms(
                            this_meeting$start_time
                          ),
                          "America/Chicago"
                        )
                      ),
                      stringr::str_pad(meeting_num, 2, pad = "0"),
                      stringr::str_pad(file_num, 2, pad = "0"),
                      sep = "_"
                    ),
                    fileext = ".mp4"
                  )

                  httr2::request(this_file$download_url) |>
                    zoomer:::.zoom_req_authenticate() |>
                    httr2::req_perform() |>
                    httr2::resp_body_raw() |>
                    writeBin(con = file_path)

                  # Use previous videos to get details. Note: The first video
                  # needs to be uploaded manually.

                  if (!(cohort_id %in% names(youtube_playlists))) {
                    # TODO: Make this a link to the playlist creator, or consider
                    # doing this automatically.
                    cli::cli_abort("{log_now()} New playlist! Create {cohort_id}.")
                  }

                  playlist_id <- youtube_playlists[[cohort_id]]

                  # Get the most recent video on that playlist. Unfortunately it
                  # returns them in order from earliest so I need to get all then
                  # take the last one.
                  playlist_items <- youtubeR::yt_call_api(
                    endpoint = "playlistItems",
                    query = list(
                      playlist_id = playlist_id,
                      part = "contentDetails",
                      max_results = 50L
                    )
                  )

                  # TODO: A deleted video was on a playlist, and it was hard to
                  # tell that's what had happened! At a minimum we should check
                  # for contentDetails$videoPublishedAt to make sure it's real.

                  if (length(playlist_items$items)) {
                    target_n <- length(playlist_items$items)
                    previous_id <- playlist_items$items[[
                      target_n
                    ]]$contentDetails$videoId

                    # Translate that to details.
                    result <- youtubeR::yt_call_api(
                      endpoint = "videos",
                      query = list(
                        part = "snippet",
                        id = previous_id
                      )
                    )

                    # Use the previous snippet to fill in details. I'll edit
                    # later.
                    snippet <- result$items[[1]]$snippet
                    snippet <- snippet[c(
                      "title",
                      "description",
                      "tags",
                      "categoryId",
                      "defaultLanguage",
                      "defaultAudioLanguage"
                    )]
                    snippet$description <- paste(
                      "EDITTHIS ",
                      snippet$description
                    )
                    # TODO: Use the bookclub spreadsheets to fill in information
                    # about the video.
                  } else {
                    cli::cli_alert_warning(
                      "{log_now()} The {cohort_id} playlist is empty!"
                    )
                    snippet <- list(
                      title = glue::glue("BOOK: Introduction ({cohort_id} 1)"),
                      description = glue::glue(
                        "FACILITATOR kicks off a new book club for BOOK by AUTHORS",
                        "on {meeting_date}, to the R4DS {book_abbrev} Book Club.",
                        "Cohort {cohort_number}",
                        "\n\nRead along at BOOK_URL",
                        "\nJoin the conversation at r4ds.io/join!",
                        .sep = " "
                      ),
                      tags = "rstats"
                    )
                  }

                  # Upload it.
                  new_video_id <- youtubeR::yt_videos_insert(
                    video_path = file_path,
                    snippet = snippet,
                    status = youtubeR::yt_schema_video_status(
                      license = "creativeCommon",
                      privacy_status = "private",
                      self_declared_made_for_kids = FALSE
                    ),
                    recording_date = this_meeting$start_time
                  )

                  # Add it to the playlist. I'm assigning this so the result
                  # doesn't print.
                  playlist_result <- youtubeR::yt_call_api(
                    endpoint = "playlistItems",
                    query = list(part = "snippet"),
                    body = list(
                      snippet = list(
                        playlistId = playlist_id,
                        resourceId = list(
                          kind = "youtube#video",
                          videoId = new_video_id
                        )
                      )
                    )
                  )

                  # TODO: Protect this against racetime in case a different worker
                  # is updating the file?

                  # Log the ID as one to watch in other steps.
                  this_video_status <- tibble::tibble(
                    channel_name = channel_name,
                    cohort_number = cohort_number,
                    video_id = new_video_id,
                    status = "uploaded"
                  )

                  if (fs::file_exists(working_video_path)) {
                    readRDS(working_video_path) |>
                      dplyr::bind_rows(this_video_status) |>
                      saveRDS(working_video_path)
                  } else {
                    fs::dir_create(working_video_dir)
                    saveRDS(this_video_status, working_video_path)
                  }
                }
              },
              CHAT = {
                # Download chats to their folder.
                chat_dir <- fs::path_home(
                  "Dropbox (Personal)",
                  "R", "R4DScommunity", "r4ds_bookclub_meetings", "Chats",
                  cohort_id
                )

                # fs is nice enough to create if it doesn't exist and leave it alone
                # otherwise.
                fs::dir_create(chat_dir)

                chat_path <- fs::path(
                  chat_dir,
                  paste(
                    cohort_id,
                    meeting_date,
                    stringr::str_pad(meeting_num, 2, pad = "0"),
                    stringr::str_pad(file_num, 2, pad = "0"),
                    sep = "_"
                  ),
                  ext = tolower(this_file$file_extension)
                )

                httr2::request(this_file$download_url) |>
                  zoomer:::.zoom_req_authenticate() |>
                  httr2::req_perform() |>
                  httr2::resp_body_raw() |>
                  writeBin(con = chat_path)

                # Read it back in, and upload to the appropriate channel.
                chat_log <- readLines(chat_path, warn = FALSE)

                if (channel_name %in% slack_channels$name) {
                  slackposts::chat_message(
                    channel = slack_channels$id[
                      slack_channels$name == channel_name
                    ],
                    text = glue::glue_collapse(
                      c(
                        glue::glue("The most recent cohort{cohort_number} chat log:"),
                        "```",
                        chat_log,
                        "```"
                      ),
                      sep = "\n"
                    )
                  )
                } else {
                  cli::cli_inform(
                    c(
                      "!" = "{log_now()} Cannot find channel {channel_name}.",
                      ">" = "Is the meeting name weird?"
                    )
                  )
                }
              },
              {
                cli::cli_inform(
                  c(
                    "!" = "{log_now()} File skipped.",
                    "i" = "Meeting had file type {this_file$file_type}."
                  )
                )
              }
            )

            this_meeting$ready_to_delete[[file_num]] <- TRUE
          }
        }

        if (all(this_meeting$ready_to_delete)) {
          # Delete the Zoom meeting announcement for this meeting on this day in
          # Slack.
          if (channel_name %in% slack_channels$name) {
            channel_id <- slack_channels$id[slack_channels$name == channel_name]
            channel_msgs <- slackthreads::conversations(
              channel_id,
              max_results = 100,
              limit = 100
            )

            zoom_reminder_msgs <- purrr::keep(
              channel_msgs,
              \(x) {
                x[["user"]] == "USLACKBOT" &
                  stringr::str_detect(
                    x[["text"]],
                    "Join Zoom Meeting"
                  )
              }
            )

            if (length(zoom_reminder_msgs)) {
              # We'll clean out ALL of the msgs, in case past ones got stuck.
              for (zoom_msg in zoom_reminder_msgs) {
                slackcalls::post_slack(
                  slack_method = "chat.delete",
                  channel = channel_id,
                  ts = zoom_msg$ts
                )
              }
            }
          } else {
            cli::cli_inform(
              c(
                x = "{log_now()} Zoom message not deleted.",
                "!" = "Cannot find channel {channel_name}.",
                i = "Is the meeting name weird?"
              )
            )
          }

          # Delete the recording for this meeting.
          httr2::request("https://api.zoom.us/v2/") |>
            httr2::req_url_path_append("meetings", this_meeting$uuid, "recordings") |>
            httr2::req_method("DELETE") |>
            zoomer:::.zoom_req_authenticate() |>
            httr2::req_perform()
        } else {
          cli::cli_alert_info(
            "{log_now()} {cohort_id} meeting is in progress or processing on Zoom."
          )
        }
      } else {
        cli::cli_alert_info(
          c("{log_now()} Skipping in-progress meeting.")
        )
      }
    }
  } else {
    cli::cli_alert_info(
      "{log_now()} No recordings available."
    )
  }
}
