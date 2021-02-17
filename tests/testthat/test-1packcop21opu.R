context("pack a cop 21 data pack")
httptest::with_mock_api({
test_that("pack a cop 21 data pack", {
  
  datimutils::loginToDATIM(secrets,
                           d2_session_name = "d2_session")
  
model_data <- readRDS(model_data_path)
# pack a cop 21 data pack
for (i in 1:NROW(pick)) {
  
  print(paste0(i," of ",NROW(pick)))
  print("try")
  suppressWarnings(datapackr::packDataPack(model_data = model_data,
                          datapack_name = pick[[i,1]],
                          country_uids = pick[[i,2]][[1]],
                          template_path = NULL,
                          cop_year = 2021,
                          output_folder = output_folder,
                          d2_session = d2_session,
                          results_archive = FALSE))
}

## don't forget I need to open and save the file
testthat::expect_equal(1,1)

cleanup()

})
})