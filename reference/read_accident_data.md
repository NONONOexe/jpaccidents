# Read accident data from CSV files

`read_accident_data()` reads accident data from multiple CSV files,
processes each file, and combines the data into three categories:

- `accident_info`: Basic accident information including date, location,
  and conditions

- `person_info` : Information about individuals involved in the
  accidents

- `highway_info` : Information specific to accidents that occurred on
  highways/expressways

## Usage

``` r
read_accident_data(file_paths)
```

## Arguments

- file_paths:

  A character vector of file paths to the CSV files.

## Value

A list containing three data frames (`accident_info`, `person_info`, and
`highway_info`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Read main and supplementary data for 2023
read_accident_data(c("example/honhyo_2023.csv", "example/hojuhyo_2023.csv"))
} # }
```
