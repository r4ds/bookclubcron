.yt_random_video <- function() {
  playlist_items <- .yt_random_club_playlist_items()
  this_item_id <- sample(playlist_items, 1)[[1]]$contentDetails$videoId
  video_snippet <- .yt_video_snippet(this_item_id)
  return(
    list(
      video_id = this_item_id,
      title = video_snippet$title,
      description = video_snippet$description,
      video_url = paste0("https://youtu.be/", this_item_id),
      tags = unlist(video_snippet$tags)
    )
  )
}

.yt_random_club_playlist_items <- function() {
  playlist_items <- list()
  i <- 1L
  while (!length(playlist_items) && i < 11) {
    i <- i + 1L
    this_playlist <- .yt_random_club_playlist()
    playlist_items <- .yt_playlist_videos(this_playlist)
  }
  if (length(playlist_items)) {
    return(playlist_items)
  }
  cli::cli_abort("No club playlist items found.")
}

.yt_random_club_playlist <- function() {
  youtube_playlists <- list()
  i <- 1L
  while (!length(youtube_playlists) && i < 11) {
    i <- i + 1L
    youtube_playlists <- .yt_club_playlists()
  }
  if (length(youtube_playlists)) {
    return(sample(youtube_playlists, 1))
  }
  cli::cli_abort("No club playlists found.")
}

.yt_club_playlists <- function() {
  youtube_playlists <- dslc_youtube_playlists(500L)

  # Get rid of non-club playlists
  youtube_playlists[stringr::str_detect(names(youtube_playlists), "\\(\\w+\\)")]
}

.yt_playlist_videos <- function(playlist_id) {
  # TODO: Cache this like dslc_youtube_playlists()
  youtubeR::yt_call_api(
    endpoint = "playlistItems",
    query = list(
      playlist_id = playlist_id,
      part = "contentDetails",
      max_results = 50L
    )
  )$items
}

.yt_video_snippet <- function(video_id) {
  youtubeR::yt_call_api(
    endpoint = "videos",
    query = list(
      part = "snippet",
      id = video_id
    )
  )$items[[1]]$snippet
}
