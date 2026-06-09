test_that("st_set_point returns sf POINT", {
  result <- st_set_point(lon = 145.5, lat = -14.7)
  expect_s3_class(result, "sf")
  expect_equal(as.character(sf::st_geometry_type(result)), "POINT")
})
test_that("seascape_color_pal returns named character vector", {
  result <- seascape_color_pal()
  expect_type(result, "character")
  expect_true(!is.null(names(result)))
})
