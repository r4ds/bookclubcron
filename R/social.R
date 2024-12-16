.social_message_compile <- function(...) {
  msgs <- .discard_empty(...)
  if (length(msgs)) {
    return(
      paste(msgs, collapse = "\n\n")
    )
  }
  return(character())
}

.social_message_end <- function() {
  "Visit https://dslc.video for hours of new #DataScience videos every week!"
}

.social_message_minimize <- function(msgs) {
  msgs <-
    msgs |>
    stringr::str_remove(r"(Chapter \d+)") |>
    stringr::str_remove(r"(Ch \d+\&\d+)") |>
    stringr::str_remove(r"(Ch \d+)") |>
    stringr::str_replace(r"(: \d+)", ":") |>
    stringr::str_remove_all('"') |>
    stringr::str_remove_all(r"(\(\d{4}-\d{2}-\d{2}\))") |>
    stringr::str_remove_all(r"(\([a-z0-9_]+\d{2}( \d+)*\))") |>
    stringr::str_remove_all(r"(\([a-z0-9_]+\d{2} extra\))") |>
    stringr::str_replace("Book Club\\s*:", ":") |>
    stringr::str_replace_all(" :", ":") |>
    stringr::str_replace_all("::", ":") |>
    stringr::str_squish()
  return(msgs)
}

.discard_empty <- function(...) {
  dots <- unlist(list(...))
  return(dots[nchar(dots) > 0])
}

.extract_hashtags <- function(msgs) {
  hashtags <- stringr::str_extract_all(msgs, "#[a-zA-Z0-9]+")
  msgs <- stringr::str_remove_all(msgs, "#[a-zA-Z0-9]+")
  attr(msgs, "hashtags") <- .clean_hashtags(hashtags)
  return(msgs)
}

.clean_tags <- function(tags) {
  tags <- unlist(tags)
  tags <- stringr::str_remove(tags, "#")
  has_space <- stringr::str_detect(tags, " ")
  tags[has_space] <- snakecase::to_lower_camel_case(tags[has_space])
  names(tags) <- tolower(tags)
  tags <- tags[unique(names(tags))]
  if ("rstats" %in% names(tags)) {
    tags[["rstats"]] <- "RStats"
  }
  if ("javascript" %in% names(tags)) {
    tags[["javascript"]] <- "JavaScript"
  }
  if ("databs" %in% names(tags)) {
    tags[["databs"]] <- "dataBS"
  }
  return(unname(tags))
}

.clean_hashtags <- function(hashtags) {
  return(.tags_to_hashtags(.clean_tags(hashtags)))
}

.tags_to_hashtags <- function(tags) {
  return(paste0("#", .clean_tags(tags)))
}

.tags_to_bullets <- function(tags) {
  tags <- .clean_tags(tags)
  known_language_tags <- c(
    rstats = "🔵",
    pydata = "🟢",
    python = "🟢",
    julia = "🟣",
    julialang = "🟣",
    js = "🟡",
    javascript = "🟡",
    rustlang = "🟠",
    rust = "🟠"
  )
  lang_tags <- known_language_tags[names(known_language_tags) %in% tolower(tags)]
  if (length(lang_tags)) {
    return(unique(unname(lang_tags)))
  }
  return("🔴")
}

.sum_nchar <- function(...) {
  dots <- .discard_empty(...)
  return(sum(nchar(dots)) + length(dots) - 1)
}
