#' Cache or Fetch DSLC Slack channels
#'
#' Fetch public and private DSLC Slack channel information.
#'
#' @inheritParams .fetch_dslc_slack_channels
#' @param refresh Get fresh data?
#'
#' @inherit .fetch_dslc_slack_channels return
#' @export
dslc_slack_channels <- function(refresh = FALSE,
                                token = slack_default_token()) {
  if (refresh) {
    .cache_dslc_slack_channels(token)
    return(the$slack_channels)
  }

  return(
    rlang::env_cache(
      the,
      "slack_channels",
      .fetch_dslc_slack_channels(token)
    )
  )
}

#' Cache DSLC Slack channels
#'
#' Set DSLC slack channels in the package `the` environment.
#'
#' @inheritParams .fetch_dslc_slack_channels
#'
#' @return A dataframe with information about DSLC Slack channels, invisibly.
#' @keywords internal
.cache_dslc_slack_channels <- function(token = slack_default_token()) {
  return(
    rlang::env_bind(
      the,
      slack_channels = .fetch_dslc_slack_channels(token)
    )
  )
}

#' Fetch DSLC Slack channels
#'
#' @inheritParams slackteams::get_conversations_list
#'
#' @return A dataframe with information about DSLC Slack channels.
#' @keywords internal
.fetch_dslc_slack_channels <- function(token = slack_default_token()) {
  return(
    dplyr::select(
      slackteams::get_conversations_list(
        type = c("public_channel", "private_channel"),
        exclude_archived = TRUE,
        token = token
      ),
      "id", "name", "is_private", "created", "is_general"
    )
  )
}

#' Fetch a Slack token
#'
#' Fetch the Slack token using the keyring package (if available), or an
#' environment variable with the same name.
#'
#' @param key_name The name of the keyring key or the environment variable.
#'
#' @return The token value as a string, or NULL (invisibly).
#' @export
#'
#' @examples
#' token <- slack_default_token()
#' nchar(token)
slack_default_token <- function(key_name = "SLACK_API_TOKEN") {
  key_value <- .keyring_try(key_name) %||% Sys.getenv(key_name)
  return(invisible(key_value))
}

.keyring_try <- function(key_name, keyring = NULL) {
  tryCatch(
    keyring::key_get(key_name, keyring = keyring),
    error = function(e) NULL
  )
}

#' Set a Slack API token
#'
#' Use the keyring package to store an API key.
#'
#' @param key_name The name of the key.
#' @param keyring The name of a specific keyring, passed on to
#'   `keyring::set_key` if available.
#'
#' @return The key, invisibly.
#' @export
slack_set_token <- function(key_name = "SLACK_API_TOKEN", keyring = NULL) {
  rlang::check_installed("keyring", "to save the API token securely.") # nocov
  keyring::key_set(key_name, prompt = "Slack API token", keyring = keyring) # nocov
  return(invisible(.keyring_try(key_name, keyring = keyring))) # nocov
}

#' Remove Zoom reminders
#'
#' Remove Zoom reminders from Slack channels.
#'
#' @inheritParams dslc_slack_channels
#' @param channel_name The name of the channel from which reminders should be
#'   removed.
#' @param min_age_minutes How old messages need to be (in minutes) to be
#'   cleared.
#' @param max_msgs_to_check How many messages to fetch to check for Zoom
#'   reminders.
#' @param slack_channels A data.frame of Slack channels.
#'
#' @return NULL (invisibly)
#' @keywords internal
remove_slack_reminders <- function(channel_name,
                                   min_age_minutes = 55,
                                   max_msgs_to_check = Inf,
                                   token = slack_default_token(),
                                   slack_channels = dslc_slack_channels(
                                     token = token
                                   )) {
  channel_id <- .slack_channel_name_to_id(channel_name, token, slack_channels)
  old_reminder_messages <- .slack_reminder_messages(
    channel_id,
    min_age_minutes = min_age_minutes,
    max_msgs_to_check = max_msgs_to_check,
    token = token,
    slack_channels = slack_channels
  )
  .delete_slack_messages(old_reminder_messages, channel_id)
}

.slack_reminder_messages <- function(channel_id,
                                     min_age_minutes = NULL,
                                     max_msgs_to_check = Inf,
                                     token = slack_default_token(),
                                     slack_channels = dslc_slack_channels(
                                       token = token
                                     )) {
  channel_messages <- dslc_slack_channel_messages(
    channel_id,
    max_results = max_msgs_to_check,
    token = token,
    slack_channels = slack_channels
  )
  .filter_slack_messages(
    channel_messages,
    user = "USLACKBOT",
    text = "Join Zoom Meeting",
    min_age_minutes = min_age_minutes
  )
}

.slack_channel_name_to_id <- function(channel_name,
                                      token = slack_default_token(),
                                      slack_channels = dslc_slack_channels(
                                        token = token
                                      )) {
  channel_name <- .validate_channel_name(channel_name, token, slack_channels)
  slack_channels$id[slack_channels$name == channel_name]
}

.validate_channel_name <- function(channel_name,
                                   token = slack_default_token(),
                                   slack_channels = dslc_slack_channels(
                                     token = token
                                   )) {
  if (channel_name %in% slack_channels$name) {
    return(channel_name)
  }
  cli::cli_abort(
    "Cannot find channel {channel_name}.",
    class = "bookclubcron-error-channel_name"
  )
}

dslc_slack_channel_messages <- function(channel_id,
                                        max_results = Inf,
                                        token = slack_default_token(),
                                        slack_channels = dslc_slack_channels(
                                          token = token
                                        )) {
  slackthreads::conversations(
    channel_id,
    token = token,
    max_results = max_results,
    limit = min(1000, max_results)
  )
}

.filter_slack_messages <- function(channel_messages,
                                   user = NULL,
                                   text = NULL,
                                   min_age_minutes = NULL) {
  purrr::keep(
    channel_messages,
    \(x) {
      (is.null(user) || x[["user"]] %fin% user) &&
        (is.null(text) || stringr::str_detect(x[["text"]], text)) &&
        (is.null(min_age_minutes) || .ts_is_older(x[["ts"]], min_age_minutes))
    }
  )
}

.ts_is_older <- function(ts, min_age_minutes) {
  .ts_age(ts) > lubridate::minutes(min_age_minutes)
}

.ts_age <- function(ts) {
  lubridate::now() - .ts_as_datetime(ts)
}

.ts_as_datetime <- function(ts) {
  lubridate::as_datetime(as.numeric(ts))
}

.delete_slack_message <- function(channel_id,
                                  timestamp,
                                  token = slack_default_token()) {
  slackcalls::post_slack(
    slack_method = "chat.delete",
    channel = channel_id,
    ts = timestamp,
    token = token
  )
}

.delete_slack_messages <- function(messages,
                                   channel_id,
                                   token = slack_default_token()) {
  for (msg in messages) {
    .delete_slack_message(channel_id, msg$ts, token = token)
  }
}

dslc_book_club_channels <- function(token = slack_default_token(),
                                    slack_channels = dslc_slack_channels(
                                      token = token
                                    )) {
  slack_channels |>
    dplyr::filter(stringr::str_starts(name, "book_club-")) |>
    dplyr::pull(.data$name)
}

#' Remove Book Club Zoom reminders
#'
#' Clear Zoom reminders from Book Club Slack channels.
#'
#' @inheritParams remove_slack_reminders
#'
#' @return NULL (invisibly)
#' @export
remove_all_club_reminders <- function(min_age_minutes = 55,
                                      token = slack_default_token(),
                                      slack_channels = dslc_slack_channels(
                                        token = token
                                      )) {
  club_channels <- dslc_book_club_channels(
    token = token,
    slack_channels = slack_channels
  )
  for (channel_name in club_channels) {
    remove_slack_reminders(
      channel_name,
      min_age_minutes = min_age_minutes,
      token = token,
      slack_channels = slack_channels
    )
  }
}
