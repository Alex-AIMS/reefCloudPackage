test_that("startMatter works", {
  args = c("--bucket=C:/Users/N11003324/OneDrive - Queensland University of Technology/Documents/ReefCloud project/aus_data/AUS",
           "--domain=site","--debug=true", "--runStage=1", "--refresh_data=true")

  reefCloudPackage::startMatter(args)

  #global variables created by initialise_status
  testthat::expect_true(exists("STATUS"))

  #global variables created by parseCLA
  testthat::expect_true(exists("REFRESH_DATA"))
  testthat::expect_true(exists("DEBUG_MODE"))
  testthat::expect_true(exists("runstage"))
  testthat::expect_true(exists("runStage"))
  testthat::expect_true(exists("AWS_PATH"))
  testthat::expect_true(exists("DATA_FROM"))
  testthat::expect_true(exists("DOMAIN_CATEGORY"))
  testthat::expect_true(exists("AWS_OUTPUT_PATH"))
  testthat::expect_true(exists("DOMAIN_NAME"))
  testthat::expect_true(exists("GENERATE_REPORT"))
  testthat::expect_true(exists("MODEL_TYPE"))
  testthat::expect_true(exists("BY_TIER"))

  #global variables created by genereteSettings
  testthat::expect_true(exists("DATA_PATH"))
  testthat::expect_true(exists("INPUT_DATA"))
  testthat::expect_true(exists("RDATA_FILE"))
  testthat::expect_true(exists("FILENAME"))
  testthat::expect_true(exists("INPUT_FORMAT"))
  testthat::expect_true(exists("CSV_FILE"))
  testthat::expect_true(exists("TIER_DATA"))
})
