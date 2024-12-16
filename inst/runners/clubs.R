library(bookclubcron)

# yt_token <- youtubeR::yt_authenticate(force = TRUE)
# keyring::key_set_with_value("youtube-refresh", password = yt_token$refresh_token)
# zoom_token <- zoomer::zoom_authenticate()
# keyring::key_set_with_value("zoom-refresh", password = zoom_token$refresh_token)
process_zoom()
process_youtube()
