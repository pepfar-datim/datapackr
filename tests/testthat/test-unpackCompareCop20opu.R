test_that("unpack a cop 20 data pack and compare", {
# unpack a cop 20 data pack
  
  datimutils::loginToDATIM(secrets,
                           d2_session_name = "d2_session")
  
d <- datapackr::unPackTool("~/datapackr_test_files/Testing/OPU/OPU Data Pack_Eswatini_20201116165741_CDC_USAID_with dedup.xlsx"
                           ,d2_session = d2_session
)


foo <- datapackr::compareData_OpuDatapackVsDatim(d 
                                                 , d2_session = d2_session
)

testthat::expect_equal(1,1)

cleanup()
})