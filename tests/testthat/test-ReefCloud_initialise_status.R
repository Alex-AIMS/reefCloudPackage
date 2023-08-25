test_that("ReefCloud_initialise_status works", {
  reefCloudPackage::ReefCloud_initialise_status()

  testthat::expect_true(exists("STATUS"))
})
