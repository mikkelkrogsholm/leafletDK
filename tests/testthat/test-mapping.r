context("Mapping functions")

# Test 1

test_join_map_data <- leafletDK:::join_map_data(value = "INDHOLD", id = "OMRÅDE", subplot = NULL,
              mapdata = leafletDK::folk1,
              leafletDK::municipal)

test_that("join_map_data creates a class SpatialPolygonsDataFrame", {
  expect_match(class(test_join_map_data), "SpatialPolygonsDataFrame")
})

## Test 2

test_join_map_data_zip <- suppressMessages(leafletDK:::join_map_data_zip(value = "indhold", id = "zip",
                                                        subplot = NULL, mapdata = leafletDK::ejen4,
                                                        leafletDK::zip))

test_that("join_map_data_zip creates a class SpatialPolygonsDataFrame", {
  expect_match(class(test_join_map_data_zip), "SpatialPolygonsDataFrame")
})

## Test 3

leafletmap <- leafletDK:::map_it(test_join_map_data, map = FALSE, legend = FALSE,
                                 pal = "YlOrRd", logcol = FALSE, legendtitle = "")

test_that("map_it creates a leaflet map ", {
  expect_match(is(leafletmap), "leaflet")
})


