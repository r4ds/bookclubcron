library(bookclubcron)

# token <- youtubeR::yt_authenticate(force = TRUE)
# keyring::key_set_with_value("youtube-refresh", password = token$refresh_token)
process_zoom()
process_youtube()
