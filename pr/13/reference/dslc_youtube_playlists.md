# Cache or Fetch DSLC YouTube Playlists

Fetch DSLC YouTube playlist information.

## Usage

``` r
dslc_youtube_playlists(n = 50L, refresh = FALSE)
```

## Arguments

- n:

  How many playlists do we need? This should ideally be equal to the
  number of active clubs.

- refresh:

  Get fresh data?

## Value

A character vector of playlist IDs, with titles as names.
