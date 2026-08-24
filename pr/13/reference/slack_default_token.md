# Fetch a Slack token

Fetch the Slack token using the keyring package (if available), or an
environment variable with the same name.

## Usage

``` r
slack_default_token(key_name = "SLACK_API_TOKEN")
```

## Arguments

- key_name:

  The name of the keyring key or the environment variable.

## Value

The token value as a string, or NULL (invisibly).

## Examples

``` r
token <- slack_default_token()
#> Warning: Selecting ‘env’ backend. Secrets are stored in environment variables
nchar(token)
#> [1] 0
```
