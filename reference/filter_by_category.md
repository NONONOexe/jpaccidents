# Filter accident data by category

`filter_by_category()` filters accident data based on the specified
category and its values.

## Usage

``` r
filter_by_category(data, category, category_name)
```

## Arguments

- data:

  A `accident_data` object.

- category:

  A vector specifying the category values to filter by.

- category_name:

  A string specifying the name of the category column.

## Value

A filtered `accident_data` object.
