% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slack.R
\name{slack_set_token}
\alias{slack_set_token}
\title{Set a Slack API token}
\usage{
slack_set_token(key_name = "SLACK_API_TOKEN", keyring = NULL)
}
\arguments{
\item{key_name}{The name of the key.}

\item{keyring}{The name of a specific keyring, passed on to
\code{keyring::set_key} if available.}
}
\value{
The key, invisibly.
}
\description{
Use the keyring package to store an API key.
}
