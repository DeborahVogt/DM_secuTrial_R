context("secuTrial-testing")

load.tables(data.dir=system.file("extdata", "s_export_CSV-xls_DEM00_20180912-125720.zip", package = "secuTrial"))

test_that("Bone mineral density dataset (non rectangular) has the correct dimensions", {
  expect_equal(dim(bmd),c(501, 25))
})



