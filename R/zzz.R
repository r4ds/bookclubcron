.onLoad <- function(lib, pkg) {
  # Authenticate so the user can walk away.
  # youtubeR::yt_authenticate()

  # I may not want to do this long-term, since n will eventually matter.
  # .cache_r4ds_youtube_playlists(n = 50L)

  # Cache the Slack channels so we can hit them later. This will also make sure
  # we have Slack authentication.
  # .cache_r4ds_slack_channels()

  # Ideally we should also do a quick zoom check here.

  the$old_timeout <- options(timeout = 1000)
  the$old_digits_secs <- options(digits.secs = 0)
}

.onUnload <- function(lib) {
  options(the$old_timeout)
  options(the$old_digits_secs)
}

do_stuff <- function(...) {
  cli::cli_abort("{log_now()} This isn't defined.")
}
