# Cache or Fetch DSLC Slack channels

Fetch public and private DSLC Slack channel information.

## Usage

``` r
dslc_slack_channels(refresh = FALSE, token = slack_default_token())
```

## Arguments

- refresh:

  Get fresh data?

- token:

  character, api token issued by slack

## Value

A dataframe with information about DSLC Slack channels.
