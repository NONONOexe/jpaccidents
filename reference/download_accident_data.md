# Download the traffic accident data

`download_accident_data()` downloads the file of traffic accident data
provided the [National Police
Agency](https://www.npa.go.jp/publications/statistics/koutsuu/opendata/index_opendata.html).

## Usage

``` r
download_accident_data(type, download_dir = getwd(), years = 2023)
```

## Arguments

- type:

  A character string specifying the type of data to download: one of
  "main", "supp", or "highway".

- download_dir:

  A character string specifying the directory to save the downloaded
  files (default: [`getwd()`](https://rdrr.io/r/base/getwd.html)).

- years:

  An integer or vector of integers specifying the years of the data to
  download. Available years are 2019 to 2023 (default: `2023`).

## Value

A character vector of file paths for the downloaded files (invisibly).

## Details

The traffic accident data are divided into the following types:

- `"main"` : Basic data on traffic accidents (honhyo).

- `"supp"` : Additional data related to the `"main"` data (hojuhyo).

- `"highway"` : Data on traffic accidents on highways (kosokuhyo).

Use the `type` argument to specify which data type to download.

## Examples

``` r
if (FALSE) { # \dontrun{
# Download the main data for 2023
download_accident_data("main", "download-dir-path", 2023)

# Download the supplementary data for 2023
download_accident_data("supp", "download-dir-path", 2023)

# Download the highway data for 2019 to 2023
download_accident_data("highway", "download-dir-path", 2019:2023)
} # }
```
