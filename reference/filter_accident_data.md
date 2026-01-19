# Filter accident data by categories

These functions filter accident data based on the specified categories.

## Usage

``` r
filter_accident_type(data, accident_type)

filter_road_shape(data, road_shape)
```

## Arguments

- data:

  A `accident_data` object.

- accident_type:

  A vector of specifying the accident type to filter. Must be one of the
  following values:

  - `"vehicle_to_pedestrian"`: Vehicle-to-pedestrian collision

  - `"vehicle_to_vehicle"` : Vehicle-to-vehicle collision

  - `"single_vehicle"` : Single-vehicle accident

  - `"train"` : Train accident

- road_shape:

  A vector of specifying the road type to filter. Must be one of the
  following values:

  - `"intersection_roundabout"` : Roundabout intersection

  - `"intersection_other"` : Regular intersection

  - `"near_intersection_roundabout"`: Area near a roundabout

  - `"near_intersection_other"` : Area near an intersection

  - `"tunnel"` : Tunnel

  - `"bridge"` : Bridge

  - `"curve"` : Curve or bend in the road

  - `"single_road"` : Straight single road

  - `"railroad_crossing_type1"` : Type 1 railroad crossing

  - `"railroad_crossing_type3"` : Type 3 railroad crossing

  - `"railroad_crossing_type4"` : Type 4 railroad crossing

  - `"general_traffic_area"` : General traffic area

## Value

A filtered `accident_data` object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract accidents where vehicle collided with pedestrian
vehicle_to_pedestrian_collisions <- data %>%
  filter_accident_type("vehicle_to_pedestrian")

# Extract accidents involving vehicle-to-vehicle collisions
vehicle_to_pedestrian_collisions <- data %>%
  filter_accident_type("vehicle_to_pedestrian")

# Extract accidents that occured on curved roads
curved_road_accidents <- data %>%
  filter_road_shape("curve")
} # }
```
