li_message_randoms <- function(...) {
  msg <- character()
  new_msgs <- .discard_empty(...)
  msg_log <- new_msgs
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
  n_randoms <- max(5L - length(new_msgs), 0L)
  for (i in seq_len(n_randoms)) {
    msg <- .social_message_compile(msg, new_msg)
    new_msg <- .li_random_video_message()
    msg_log <- c(msg_log, new_msg)
  }
  # Tack the first msg onto the end so LinkedIn shows it as the video preview.
  # Hopefully we can fix this when we implement the API.
  last_msg <- paste("For preview:", msg_log[[1]])
  return(
    glue::as_glue(.social_message_compile(msg, msg_end, last_msg))
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
  .glue_special(
    glue::glue_collapse(bullets),
    "{{video$title}} {{video$video_url}}",
    glue::glue_collapse(hashtags, sep = " ")
  )
}
