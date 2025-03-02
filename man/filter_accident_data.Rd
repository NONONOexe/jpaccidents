% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter-accident-data.R
\name{filter_accident_data}
\alias{filter_accident_data}
\alias{filter_accident_type}
\alias{filter_road_shape}
\title{Filter accident data by categories}
\usage{
filter_accident_type(data, accident_type)

filter_road_shape(data, road_shape)
}
\arguments{
\item{data}{A \code{accident_data} object.}

\item{accident_type}{A vector of specifying the accident type to
filter. Must be one of the following values:
\itemize{
\item \code{"vehicle_to_pedestrian"}: Vehicle-to-pedestrian collision
\item \code{"vehicle_to_vehicle"}   : Vehicle-to-vehicle collision
\item \code{"single_vehicle"}       : Single-vehicle accident
\item \code{"train"}                : Train accident
}}

\item{road_shape}{A vector of specifying the road type to filter.
Must be one of the following values:
\itemize{
\item \code{"intersection_roundabout"}     : Roundabout intersection
\item \code{"intersection_other"}          : Regular intersection
\item \code{"near_intersection_roundabout"}: Area near a roundabout
\item \code{"near_intersection_other"}     : Area near an intersection
\item \code{"tunnel"}                      : Tunnel
\item \code{"bridge"}                      : Bridge
\item \code{"curve"}                       : Curve or bend in the road
\item \code{"single_road"}                 : Straight single road
\item \code{"railroad_crossing_type1"}     : Type 1 railroad crossing
\item \code{"railroad_crossing_type3"}     : Type 3 railroad crossing
\item \code{"railroad_crossing_type4"}     : Type 4 railroad crossing
\item \code{"general_traffic_area"}        : General traffic area
}}
}
\value{
A filtered \code{accident_data} object.
}
\description{
These functions filter accident data based on the specified categories.
}
\examples{
\dontrun{
# Extract accidents where vehicle collided with pedestrian
vehicle_to_pedestrian_collisions <- data \%>\%
  filter_accident_type("vehicle_to_pedestrian")

# Extract accidents involving vehicle-to-vehicle collisions
vehicle_to_pedestrian_collisions <- data \%>\%
  filter_accident_type("vehicle_to_pedestrian")

# Extract accidents that occured on curved roads
curved_road_accidents <- data \%>\%
  filter_road_shape("curve")
}
}
