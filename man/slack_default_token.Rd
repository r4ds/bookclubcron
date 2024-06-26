% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slack.R
\name{slack_default_token}
\alias{slack_default_token}
\title{Fetch a Slack token}
\usage{
slack_default_token(key_name = "SLACK_API_TOKEN")
}
\arguments{
\item{key_name}{The name of the keyring key or the environment variable.}
}
\value{
The token value as a string, or NULL (invisibly).
}
\description{
Fetch the Slack token using the keyring package (if available), or an
environment variable with the same name.
}
\examples{
token <- slack_default_token()
nchar(token)
}
