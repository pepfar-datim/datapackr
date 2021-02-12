test_that("pack a cop 20 OPU datapack", {
  
  datimutils::loginToDATIM(secrets,
                           d2_session_name = "d2_session")
  
for (i in 1:NROW(pick)) {
  print(paste0(i," of ",NROW(pick), ": ", pick[[i,1]]))
  
  datapackr::packOPUDataPack(datapack_name = pick[[i,1]],
                             country_uids = unlist(pick[[i,"country_uids"]]),
                             template_path = NULL,
                             cop_year = 2020,
                             output_folder = output_folder,
                             d2_session = d2_session,
                             results_archive = FALSE)
}


testthat::expect_equal(1,1)

cleanup()

})