# Remove Zoom reminders

Remove Zoom reminders from Slack channels.

## Usage

``` r
remove_slack_reminders(
  channel_name,
  min_age_minutes = 55,
  max_msgs_to_check = Inf,
  token = slack_default_token(),
  slack_channels = dslc_slack_channels(token = token)
)
```

## Arguments

- channel_name:

  The name of the channel from which reminders should be removed.

- min_age_minutes:

  How old messages need to be (in minutes) to be cleared.

- max_msgs_to_check:

  How many messages to fetch to check for Zoom reminders.

- token:

  character, api token issued by slack

- slack_channels:

  A data.frame of Slack channels.

## Value

NULL (invisibly)
