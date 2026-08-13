# Get video count + time for 2024.
throw_away <- purrr::quietly(youtubeR::yt_authenticate)()
rm(throw_away)
youtube_playlists <- dslc_youtube_playlists(500L)

# Those are returned in order of when videos were added to them. SO, if we hit a
# list that doesn't have any videos in the specified timeframe, we're done.
vids2024 <- vector(mode = "list", length = length(youtube_playlists))
for (i in seq_along(youtube_playlists)) {
  playlist_id <- youtube_playlists[[i]]
  playlist_items <- youtubeR::yt_call_api(
    endpoint = "playlistItems",
    query = list(
      playlist_id = playlist_id,
      part = "contentDetails",
      max_results = 50L
    )
  )
  video_ids <- playlist_items$items |>
    purrr::keep(\(item) {
      publish_year <- lubridate::ymd_hms(
        item$contentDetails$videoPublishedAt
      ) |>
        lubridate::year()
      item$kind == "youtube#playlistItem" && publish_year == 2024
    }) |>
    purrr::map_chr(c("contentDetails", "videoId"))
  if (length(video_ids)) {
    vids2024[[i]] <- video_ids
  }
}
vids2024 <- unlist(vids2024)

# Get the running time of each *public* video.
running_times <- purrr::map(
  vids2024,
  \(video_id) {
    video_details <- youtubeR::yt_call_api(
      endpoint = "videos",
      query = list(
        part = "contentDetails,status",
        id = video_id
      )
    )
    if (video_details$items[[1]]$status$privacyStatus == "public") {
      duration_string <- video_details$items[[1]]$contentDetails$duration
      duration <- lubridate::hms(duration_string, quiet = TRUE)
      if (is.na(duration)) {
        duration <- lubridate::ms(duration_string, quiet = TRUE)
      }
      if (is.na(duration)) {
        res <- purrr::quietly(lubridate::ms)(paste0(duration_string, "0S"))
        if (length(res$warnings)) {
          cli::cli_abort("Duration string: {duration_string}")
        }
        duration <- res$result
      }
      if (is.na(duration)) {
        cli::cli_abort("Duration string: {duration_string}")
      }
      return(lubridate::as.duration(duration))
    }
  }
)
total_seconds <- sum(unlist(running_times))
total_minutes <- total_seconds / 60
total_hours <- total_seconds / 60 / 60
total_days <- total_seconds / 60 / 60 / 24
