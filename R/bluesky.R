bsky_message_randoms <- function(...) {
  msg <- character()
  new_msgs <- .discard_empty(...)
  hashtags <- "dataBS"
  if (length(new_msgs)) {
    new_msgs <- .extract_hashtags(new_msgs)
    hashtags <- c(hashtags, attr(new_msgs, "hashtags"))
    msg <- .social_message_compile(
      .bsky_message_new_start(),
      .social_message_minimize(new_msgs),
      msg
    )
  }
  msg_end <- .social_message_end()
  new_msg <- .bsky_random_video_message()
  new_hashtags <- .clean_hashtags(c(hashtags, attr(new_msg, "hashtags")))
  new_msg <- .social_message_compile(
    .bsky_message_archive_start(),
    new_msg
  )
  while (.sum_nchar(msg, new_msg, msg_end, new_hashtags) + 1 + 3 < 300) {
    hashtags <- new_hashtags
    msg <- .social_message_compile(msg, new_msg)
    new_msg <- .bsky_random_video_message()
    new_hashtags <- .clean_hashtags(c(hashtags, attr(new_msg, "hashtags")))
  }
  hashtags <- glue::glue_collapse(.clean_hashtags(hashtags), sep = " ")
  return(
    glue::as_glue(.social_message_compile(msg, msg_end, hashtags))
  )
}

.bsky_message_new_start <- function() {
  return("Recent DSLC club meetings:")
}

.bsky_message_archive_start <- function() {
  return("From the DSLC video aRchives:")
}

.bsky_random_video_message <- function() {
  video <- .yt_random_video()
  return(
    .bsky_video_message(video)
  )
}

.bsky_video_message <- function(video) {
  bullets <- .tags_to_bullets(video$tags)
  title <- .social_message_minimize(video$title)
  msg <- .glue_special(
    glue::glue_collapse(bullets, sep = " "),
    "{{title}} {{video$video_url}}"
  )
  attr(msg, "hashtags") <- .tags_to_hashtags(video$tags)
  return(msg)
}
