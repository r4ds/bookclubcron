li_message_randoms <- function(...) {
  msg <- character()
  new_msgs <- .discard_empty(...)
  if (length(new_msgs)) {
    msg <- .social_message_compile(
      # TODO: Create LI-specific function that inserts @ for DSLC.
      .mast_message_new_start(),
      new_msgs,
      msg
    )
  }
  msg_end <- .social_message_end()
  new_msg <- .li_message_archive_start()
  n_randoms <- 5L - length(new_msgs)
  for (i in seq_len(n_randoms)) {
    msg <- .social_message_compile(msg, new_msg)
    new_msg <- .li_random_video_message()
  }
  return(
    glue::as_glue(.social_message_compile(msg, msg_end))
  )
}

.li_message_archive_start <- function() {
  return("From the aRchives:")
}

.li_random_video_message <- function() {
  video <- .yt_random_video()
  return(
    .li_video_message(video)
  )
}

.li_video_message <- function(video) {
  bullets <- .tags_to_bullets(video$tags)
  hashtags <- .tags_to_hashtags(video$tags)
  glue::glue(
    glue::glue_collapse(bullets, sep = " "),
    '"{video$title}" {video$video_url}',
    glue::glue_collapse(hashtags, sep = " "),
    .sep = " "
  )
}
