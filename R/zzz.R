.onLoad <- function(lib, pkg) {
  # Memoise functions that shouldn't change within a session.
  .active_clubs <<- memoise::memoise(.active_clubs) # nocov

  # Authenticate so the user can walk away.
  # youtubeR::yt_authenticate()

  # I may not want to do this long-term, since n will eventually matter.
  # .cache_dslc_youtube_playlists(n = 50L)

  # Cache the Slack channels so we can hit them later. This will also make sure
  # we have Slack authentication.
  # .cache_dslc_slack_channels()

  # Ideally we should also do a quick zoom check here.

  the$old_timeout <- options(timeout = 1000)
}

.onUnload <- function(lib) {
  options(the$old_timeout)
}

do_stuff <- function(...) {
  cli::cli_abort("{log_now()} This isn't defined.")
}
