# How `Moose_travel()` Calculates Travel Distance and Time

## Overview

`Moose_travel()` adds road travel distance and estimated travel time to a data frame. It sends each valid origin-destination coordinate pair to an OSRM-compatible routing server and appends two columns:

- `travel_distance_meters`: route distance in metres
- `travel_time_seconds`: estimated route duration in seconds

The function calculates a route along the road network. It does **not** calculate straight-line (great-circle or Euclidean) distance.

## Required input

The input must be a data frame or tibble with four coordinate columns:

1. origin latitude
2. origin longitude
3. destination latitude
4. destination longitude

Example data:

```r
trips <- data.frame(
  start_lat = c(53.5461, 51.0447),
  start_lon = c(-113.4938, -114.0719),
  end_lat   = c(53.5444, 51.0486),
  end_lon   = c(-113.4909, -114.0708)
)
```

Coordinates must use decimal degrees in the WGS 84 longitude/latitude system normally used by GPS and online maps. Latitude must be between -90 and 90, and longitude must be between -180 and 180.

## Calculation process

For each valid origin-destination pair, `Moose_travel()` performs the following operations.

### 1. Validate the coordinates

The function converts the selected columns to numeric values and checks their geographic ranges. Missing, non-numeric, infinite, or out-of-range coordinates are not sent to the routing server. Their distance and duration results are returned as `NA`.

### 2. Remove duplicate requests

Coordinate pairs are normalized to eight decimal places. When several rows contain the same origin and destination, the route is requested only once. The resulting distance and duration are then copied back to every matching row.

This reduces network traffic and makes large data sets with repeated trips faster to process.

### 3. Send an OSRM route request

The function constructs an OSRM Route Service request in longitude-latitude order:

```text
/route/v1/{profile}/{start_longitude},{start_latitude};{end_longitude},{end_latitude}
```

For example:

```text
/route/v1/driving/-113.4938,53.5461;-113.4909,53.5444
```

Only summary values are requested; detailed geometry, navigation steps, and alternative routes are disabled.

### 4. Snap coordinates to the road network

OSRM matches, or "snaps," each coordinate to a nearby routable road segment. The route therefore begins and ends at the matched road locations, which may differ slightly from the exact input coordinates.

This behavior is important for coordinates located inside buildings, parking lots, parks, or other places that are not directly represented as routable road segments.

### 5. Find a route through the road graph

OSRM represents the map as a graph:

- intersections and relevant road positions are graph nodes;
- road segments are graph edges;
- one-way restrictions, turn restrictions, access rules, and road classifications affect which edges may be used;
- the selected routing profile assigns costs and expected speeds to the edges.

With `profile = "driving"`, OSRM searches for a car-routable path using the server's driving profile. The result is generally a fastest or lowest-cost route under that profile, rather than simply the geometrically shortest path.

### 6. Return distance and duration

OSRM returns two route summary values:

- `distance`: the total length of the selected road route, in metres;
- `duration`: the estimated time required to travel the route, in seconds.

`Moose_travel()` places these values in the output data set without changing the original rows or columns.

## What the time estimate means

Travel time is a model-based estimate derived from the speeds and penalties configured in the OSRM routing profile. Depending on the server and map profile, it may account for factors such as:

- road class and expected speed;
- speed limits represented in OpenStreetMap;
- intersections and turn penalties;
- traffic signals;
- access and turn restrictions;
- ferry or special-road penalties.

The standard self-hosted OSRM car profile does not automatically include current traffic congestion, weather, construction delays, incidents, parking time, or time spent walking between a building and the snapped road location. The result should therefore be interpreted as an estimated routing time, not a guaranteed arrival time.

## Basic usage

```r
library(MooseR)

result <- Moose_travel(
  dataset = trips,
  start_latitude = "start_lat",
  start_longitude = "start_lon",
  end_latitude = "end_lat",
  end_longitude = "end_lon"
)

result
```

## Using an authenticated private server

Store credentials in environment variables rather than embedding them directly in a script:

```r
Sys.setenv(
  MOOSER_OSRM_USER = "your_username",
  MOOSER_OSRM_PASSWORD = "your_password"
)
```

Then pass the environment variables to `Moose_travel()`:

```r
result <- Moose_travel(
  dataset = trips,
  start_latitude = "start_lat",
  start_longitude = "start_lon",
  end_latitude = "end_lat",
  end_longitude = "end_lon",
  server = "https://your-osrm-server.example.com",
  username = Sys.getenv("MOOSER_OSRM_USER"),
  password = Sys.getenv("MOOSER_OSRM_PASSWORD")
)
```

The credentials are sent using HTTP Basic Authentication. When credentials are
provided, `Moose_travel()` requires an `https://` server so that credentials and
route requests are encrypted in transit. It rejects credentials over plain HTTP,
rejects usernames or passwords embedded in the server URL, and does not follow
HTTP redirects. Authentication values are redacted from printed HTTP request
objects.

OSRM coordinates are part of the request URL. Exact origins and destinations can
therefore appear in logs kept by the OSRM server, Cloudflare, or another authorized
reverse proxy. Review and restrict access-log retention before routing sensitive
or health-related coordinates.

## Error handling

The `on_error` argument controls routing-service failures:

```r
on_error = "warn"  # return NA and issue a warning
on_error = "na"    # return NA without a warning
on_error = "stop"  # stop at the first failed request
```

Invalid or missing coordinates always produce `NA`, regardless of this setting.

## Important limitations

- Results depend on the completeness and accuracy of the map loaded by the OSRM server.
- A server containing only Alberta map data cannot reliably route destinations outside its coverage area.
- Input coordinates may be snapped to an unintended nearby road, particularly around divided highways, service roads, bridges, or private roads.
- The driving profile may not support `walking` or `cycling`; available profiles depend on how the server was prepared.
- Route estimates can change when OpenStreetMap data, routing profiles, or OSRM versions are updated.
- Each unique route normally requires a network request, so large data sets may take time to process.
- The public OSRM demonstration server is intended for light use. Large or production workloads should use an OSRM server that you operate or are authorized to use.

## Interpretation example

If the output contains:

```text
travel_distance_meters = 12500
travel_time_seconds    = 1020
```

the selected road route is 12.5 kilometres long, and its modeled travel time is 1,020 seconds, or 17 minutes:

```r
12500 / 1000  # kilometres
1020 / 60     # minutes
```

These values describe the route selected by the server's routing model. They are not straight-line distance and should not be interpreted as live-traffic measurements unless the selected OSRM-compatible service explicitly provides traffic-aware routing.
