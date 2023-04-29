#' Cache or Fetch R4DS Slack channels
#'
#' Fetch public and private R4DS Slack channel information.
#'
#' @param refresh Get fresh data?
#'
#' @inherit .fetch_r4ds_slack_channels return
#' @export
r4ds_slack_channels <- function(refresh = FALSE) {
  if (refresh) {
    .cache_r4ds_slack_channels()
    return(the$slack_channels)
  }

  return(
    rlang::env_cache(
      the,
      "slack_channels",
      .fetch_r4ds_slack_channels()
    )
  )
}

#' Cache R4DS Slack channels
#'
#' Set R4DS slack channels in the package `the` environment.
#'
#' @return A dataframe with information about R4DS Slack channels, invisibly.
#' @keywords internal
.cache_r4ds_slack_channels <- function() {
  return(
    rlang::env_bind(
      the,
      slack_channels = .fetch_r4ds_slack_channels()
    )
  )
}

#' Fetch R4DS Slack channels
#'
#' @return A dataframe with information about R4DS Slack channels.
#' @keywords internal
.fetch_r4ds_slack_channels <- function() {
  return(
    dplyr::select(
      slackteams::get_conversations_list(
        type = c("public_channel", "private_channel"),
        exclude_archived = TRUE
      ),
      "id", "name", "is_private", "created", "is_general"
    )
  )
}
