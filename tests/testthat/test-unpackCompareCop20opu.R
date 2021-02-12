test_that("unpack a cop 20 data pack and compare", {
# unpack a cop 20 data pack
d <- datapackr::unPackTool("~/datapackr_test_files/Testing/OPU/OPU Data Pack_Eswatini_20201116165741_CDC_USAID_with dedup.xlsx"
                           ,d2_session = d2_session
)


foo <- datapackr::compareData_OpuDatapackVsDatim(d 
                                                 , d2_session = d2_session
)

cleanup()
})