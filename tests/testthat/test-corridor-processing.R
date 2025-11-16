library(testthat)
library(data.table)
library(sf)

source(testthat::test_path("..", "..", "R", "corridor_processing.R"))

test_that("near-parallel centerlines aggregate into shared corridor", {
  route_trips <- data.table(
    unique_route_id = c("r1", "r2"),
    agency = "test_agency",
    trips_am_dir0 = c(4, 4),
    trips_am_dir1 = c(0, 0),
    trips_pm_dir0 = c(4, 4),
    trips_pm_dir1 = c(0, 0)
  )

  all_trips <- data.table(
    unique_route_id = c("r1", "r2"),
    unique_trip_id = c("t1", "t2"),
    unique_shape_id = c("s1", "s2"),
    direction_id = 0L,
    agency = "test_agency"
  )

  all_shapes <- data.table(
    unique_shape_id = c("s1", "s1", "s2", "s2"),
    agency = "test_agency",
    shape_pt_lat = c(41.0, 41.001, 41.0, 41.001),
    shape_pt_lon = c(-87.0, -87.0, -86.9999, -86.9999),
    shape_pt_sequence = c(1L, 2L, 1L, 2L)
  )

  result <- identify_overlapping_segments(
    route_trips = route_trips,
    all_trips = all_trips,
    all_shapes = all_shapes,
    max_interval_minutes = 15,
    segment_length_meters = 200
  )

  expect_s3_class(result$qualifying_shapes, "sf")
  expect_gt(nrow(result$qualifying_shapes), 0)
  expect_equal(unique(result$qualification_summary$num_routes), 2)
  expect_false(any(st_is_empty(result$qualifying_shapes)))
})
