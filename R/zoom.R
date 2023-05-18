#' Process Zoom recordings
#'
#' Check for Zoom recordings, and post them to YouTube if appropriate.
#'
#' @return I'm not sure yet.
#' @export
process_zoom <- function() {
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

      meeting_start_time <- this_meeting$start_time
      meeting_start_dt <- lubridate::with_tz(
        lubridate::ymd_hms(meeting_start_time),
        "America/Chicago"
      )
      meeting_date <- lubridate::date(meeting_start_dt)
      meeting_end_dt <- lubridate::round_date(meeting_start_dt, unit = "hours") +
        lubridate::hours(1)
      if (meeting_end_dt < lubridate::now()) {
        this_meeting$ready_to_delete <- rep(
          FALSE, length(this_meeting$recording_files)
        )

        # TODO: cohort_info <- .parse_cohort_info(this_meeting$topic,
        # meeting_date)
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
        if (book_abbrev == "proj") {
          channel_name <- "project-club"
        }

        if (book_abbrev == "tmwr_es") {
          channel_name <- "book_club-tmwr"
        }

        # To edit, I need both the video and the chat at the same time.
        file_types <- purrr::map_chr(this_meeting$recording_files, "file_type")
        chat_files <- this_meeting$recording_files[file_types == "CHAT"]
        mp4_files <- this_meeting$recording_files[file_types == "MP4"]

        if (length(chat_files) == 1 && length(mp4_files) == 1) {
          chat_log <- .process_zoom_chat(
            chat_recording_file = chat_files[[1]],
            cohort_id,
            cohort_number,
            meeting_date,
            meeting_num,
            "chat",
            channel_name,
            slack_channels
          )

          start_time <- chat_log |>
            tolower() |>
            stringr::str_subset("start|begin")

          if (length(start_time)) {
            # If "start" showed up more than once, use the first one.
            start_time <- start_time[[1]] |>
              stringr::str_extract("^\\d{2}:\\d{2}:\\d{2}")
          } else {
            start_time <- "00:00:00"
          }
          end_time <- chat_log |>
            tolower() |>
            # Allow for a few variants of "end".
            stringr::str_subset("\\s*end|finish|stop")

          if (length(end_time)) {
            # If they said "end" more than once, just use the last one.
            end_time <- end_time[length(end_time)] |>
              stringr::str_extract("^\\d{2}:\\d{2}:\\d{2}")
          } else {
            end_time <- NULL
          }

          .process_zoom_video(
            mp4_files[[1]],
            cohort_number,
            cohort_id,
            book_abbrev,
            meeting_start_time,
            meeting_date,
            meeting_num,
            "video",
            youtube_playlists,
            channel_name,
            working_video_dir,
            working_video_path,
            start_time,
            end_time
          )
          this_meeting$ready_to_delete <- TRUE
          # TODO: Log that it's edited already.
        } else {
          # For now just fall back to the old way.
          for (file_num in seq_along(this_meeting$recording_files)) {
            this_file <- this_meeting$recording_files[[file_num]]
            if (this_file$status == "completed") {
              switch(
                this_file$file_type,
                MP4 = .process_zoom_video(
                  this_file,
                  cohort_number,
                  cohort_id,
                  book_abbrev,
                  meeting_start_time,
                  meeting_date,
                  meeting_num,
                  file_identifier = stringr::str_pad(file_num, 2, pad = "0"),
                  youtube_playlists,
                  channel_name,
                  working_video_dir,
                  working_video_path
                ),
                CHAT = .process_zoom_chat(
                  this_file,
                  cohort_id,
                  cohort_number,
                  meeting_date,
                  meeting_num,
                  stringr::str_pad(file_num, 2, pad = "0"),
                  channel_name,
                  slack_channels
                ),
                {
                  cli::cli_alert_danger(
                    c(
                      "x" = "{log_now()} File skipped.",
                      "i" = "Meeting had file type {this_file$file_type}."
                    )
                  )
                }
              )

              this_meeting$ready_to_delete[[file_num]] <- TRUE
            }
          }
        }

        if (all(this_meeting$ready_to_delete)) {
          .clean_zoom(this_meeting, channel_name, slack_channels)
        }
      }
    }
  }
}

.process_zoom_chat <- function(chat_recording_file,
                               cohort_id,
                               cohort_number,
                               meeting_date,
                               meeting_num,
                               file_identifier,
                               channel_name,
                               slack_channels) {
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
      file_identifier,
      sep = "_"
    ),
    ext = tolower(chat_recording_file$file_extension)
  )

  httr2::request(chat_recording_file$download_url) |>
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
    cli::cli_alert_warning(
      c(
        "!" = "{log_now()} Cannot find channel {channel_name}.",
        ">" = "Is the meeting name weird?"
      )
    )
  }

  return(chat_log)
}


.process_zoom_video <- function(video_recording_file,
                                cohort_number,
                                cohort_id,
                                book_abbrev,
                                meeting_start_time,
                                meeting_date,
                                meeting_num,
                                file_identifier,
                                youtube_playlists,
                                channel_name,
                                working_video_dir,
                                working_video_path,
                                start_time = NULL,
                                end_time = NULL) {
  # Make sure it's long enough to bother with.
  start_dt <- lubridate::as_datetime(video_recording_file$recording_start)
  end_dt <- lubridate::as_datetime(video_recording_file$recording_end)
  duration <- end_dt - start_dt

  if (duration > lubridate::minutes(10)) {
    file_path <- withr::local_tempfile(
      pattern = paste(
        cohort_id,
        meeting_date,
        stringr::str_pad(meeting_num, 2, pad = "0"),
        file_identifier,
        sep = "_"
      ),
      fileext = ".mp4"
    )

    httr2::request(video_recording_file$download_url) |>
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

    # If we have editing info, edit it.
    if (length(start_time) || length(end_time)) {
      start_time <- start_time %||% "00:00:00"

      input_path <- file_path
      file_path <- withr::local_tempfile(
        pattern = paste(
          cohort_id,
          meeting_date,
          stringr::str_pad(meeting_num, 2, pad = "0"),
          file_identifier,
          sep = "_"
        ),
        fileext = ".mp4"
      )

      if (length(end_time)) {
        ffmpeg_cmd <- glue::glue(
          "ffmpeg -v quiet -i {input_path} -ss {start_time} -to {end_time} ",
          "-c copy {file_path}"
        )
      } else {
        ffmpeg_cmd <- glue::glue(
          "ffmpeg -v quiet -i {input_path} -ss {start_time} -c copy {file_path}"
        )
      }
      system(ffmpeg_cmd, ignore.stdout = TRUE)
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
      recording_date = meeting_start_time
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

    # TODO: Warn if the same club is in the list twice.
    if (fs::file_exists(working_video_path)) {
      readRDS(working_video_path) |>
        dplyr::bind_rows(this_video_status) |>
        saveRDS(working_video_path)
    } else {
      fs::dir_create(working_video_dir)
      saveRDS(this_video_status, working_video_path)
    }
  }
}

.clean_zoom <- function(this_meeting,
                        channel_name,
                        slack_channels) {
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
    cli::cli_alert_danger(
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
}
