context("Test Datapack model paramaters")

test_that("Can test different combinations of model param passing", {

  # missing both model_path and model data
  d <- list()
  d$keychain$model_data_path <- NULL
  testthat::expect_error(
  packDataPack(d, model_data = NULL),
  "You have provided neither a model path nor model data to packTool, Please provide at least one!"
  )

  # have both model_path and model data
   d <- list()
   d$keychain$model_data_path <- test_sheet("COP23_model_data_random.rds")
   model_data <- data.frame()
   testthat::expect_error(
     packDataPack(d, model_data = model_data),
     "You have provided both a model path and model data to packTool. Please provide only one!"
   )

})
