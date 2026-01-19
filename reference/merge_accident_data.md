# Merge accident data

`merge_accident_data()` merges multiple `accident_data` objects into a
single `accident_data` object.

## Usage

``` r
merge_accident_data(...)

# S3 method for class 'accident_data'
merge_accident_data(...)

# S3 method for class 'list'
merge_accident_data(data_list, ...)
```

## Arguments

- ...:

  `accident_data` objects to merge.

- data_list:

  A list of `accident_data` objects to merge.

## Value

A merged `accident_data` object.
