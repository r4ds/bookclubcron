mast_message_randoms <- function(...) {
  msg <- character()
  new_msgs <- .discard_empty(...)
  if (length(new_msgs)) {
    msg <- .social_message_compile(
      .mast_message_new_start(),
      .social_message_minimize(new_msgs),
      msg
    )
  }
  msg_end <- .social_message_end()
  potential_msg <- .social_message_compile(
    .mast_message_archive_start(),
    .mast_random_video_message()
  )
  while (.sum_nchar(msg, potential_msg, msg_end) + 1 < 500) {
    msg <- .social_message_compile(msg, potential_msg)
    video <- .yt_random_video()
    potential_msg <- .mast_video_message(video)
  }
  return(
    glue::as_glue(.social_message_compile(msg, msg_end))
  )
}

.mast_message_new_start <- function() {
  return("Recent @DSLC club meetings:")
}

.mast_message_archive_start <- function() {
  "From the @DSLC :rstats:\u200Bchives:"
}

.mast_random_video_message <- function() {
  video <- .yt_random_video()
  return(
    .mast_video_message(video)
  )
}

.mast_video_message <- function(video) {
  icons <- .tags_to_icons(video$tags)
  hashtags <- .tags_to_hashtags(video$tags)
  title <- .social_message_minimize(video$title)
  .glue_special(
    glue::glue_collapse(icons, sep = " "),
    "{{title}} {{video$video_url}}",
    glue::glue_collapse(hashtags, sep = " ")
  )
}

.tags_to_icons <- function(tags) {
  tags <- .clean_tags(tags)
  known_language_tags <- c(
    rstats = "rstats",
    pydata = "python",
    python = "python",
    julia = "julia",
    julialang = "julia",
    js = "javascript",
    javascript = "javascript",
    rustlang = "rust",
    rust = "rust"
  )
  lang_tags <- tags[tolower(tags) %in% names(known_language_tags)]
  tags_for_icons <- unique(known_language_tags[tolower(lang_tags)])
  return(
    paste0(":", tags_for_icons, ":")
  )
}
