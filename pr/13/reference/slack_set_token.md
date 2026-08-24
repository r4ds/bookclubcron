# Set a Slack API token

Use the keyring package to store an API key.

## Usage

``` r
slack_set_token(key_name = "SLACK_API_TOKEN", keyring = NULL)
```

## Arguments

- key_name:

  The name of the key.

- keyring:

  The name of a specific keyring, passed on to `keyring::set_key` if
  available.

## Value

The key, invisibly.
