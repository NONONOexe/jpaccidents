# Filter accident data by collision patterns

`filter_collision_pattern()` filters accident data based on primary and
secondary impact conditions for each party involved in the accident.

## Usage

``` r
filter_collision_pattern(
  data,
  primary_impacts,
  secondary_impacts = NA_character_
)
```

## Arguments

- data:

  A list containing accident data.

- primary_impacts:

  A vector specifying the initial collision points (primary impact) for
  each involved party.

- secondary_impacts:

  A vector specifying the subsequent collision points (secondary impact)
  for each involved party (default: `NA_character_`).

## Value

A list containing filtered accident data.

## Details

The values of `primary_impacts` and `secondary_impacts` are following:

- `"no_impact"` : No impact

- `"front"` : Front of the vehicle

- `"right"` : Right side of the vehicle

- `"rear"` : Rear of the vehicle

- `"left"` : Left side of the vehicle

- `"front_right"`: Front right of the vehicle

- `"rear_right"` : Rear right of the vehicle

- `"rear_left"` : Rear left of the vehicle

- `"front_left"` : Front left of the vehicle The order of values in
  `primary_impacts` and `secondary_impacts` corresponds to the party
  order (`party_order`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Filter accident data by cases indicating head-on collisions
head_on_collisions <- filter_collision_pattern(data, c("front", "front"))

# Filter accident data by cases indicating rear-end collisions
rear_end_collisions <- filter_collision_pattern(data, c("rear", "front"))
} # }
```
