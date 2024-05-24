#' Cache or Fetch DSLC Slack channels
#'
#' Fetch public and private DSLC Slack channel information.
#'
#' @param refresh Get fresh data?
#'
#' @inherit .fetch_dslc_slack_channels return
#' @export
dslc_slack_channels <- function(refresh = FALSE) {
  if (refresh) {
    .cache_dslc_slack_channels()
    return(the$slack_channels)
  }

  return(
    rlang::env_cache(
      the,
      "slack_channels",
      .fetch_dslc_slack_channels()
    )
  )
}

#' Cache DSLC Slack channels
#'
#' Set DSLC slack channels in the package `the` environment.
#'
#' @return A dataframe with information about DSLC Slack channels, invisibly.
#' @keywords internal
.cache_dslc_slack_channels <- function() {
  return(
    rlang::env_bind(
      the,
      slack_channels = .fetch_dslc_slack_channels()
    )
  )
}

#' Fetch DSLC Slack channels
#'
#' @return A dataframe with information about DSLC Slack channels.
#' @keywords internal
.fetch_dslc_slack_channels <- function() {
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
