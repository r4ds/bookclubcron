# Remove Book Club Zoom reminders

Clear Zoom reminders from Book Club Slack channels.

## Usage

``` r
remove_all_club_reminders(
  min_age_minutes = 55,
  token = slack_default_token(),
  slack_channels = dslc_slack_channels(token = token)
)
```

## Arguments

- min_age_minutes:

  How old messages need to be (in minutes) to be cleared.

- token:

  character, api token issued by slack

- slack_channels:

  A data.frame of Slack channels.

## Value

NULL (invisibly)
