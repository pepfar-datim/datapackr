# context("Test Spectrum data generation")
# 
# with_mock_api({
#   test_that("We can generate play Spectrum data", {
# 
#     
#     test_dataset <- create_play_spectrum_output(country_uids = "qllxzIjjurr",
#                                                 cop_year = 2021,
#                                                 d2_session = training)
# 
#     expect_named(
#       test_dataset,
#       c(
#         "psnu",
#         "psnu_uid",
#         "area_id",
#         "indicator_code",
#         "dataelementuid",
#         "age",
#         "age_uid",
#         "sex",
#         "sex_uid",
#         "calendar_quarter",
#         "value",
#         "age_sex_rse",
#         "district_rse"
#       )
#     )
# 
#     types <- c(rep("character", 10), "double", "integer", "integer")
#     expect_identical(unname(unlist(lapply(test_dataset, typeof))), types)
#     expect_true(all(sapply(test_dataset$psnu_uid, is_uidish)))
#     expect_true(all(sapply(test_dataset$dataelementuid, is_uidish)))
#     uid_or_na <- function(x) {
#       is_uidish(x) | is.na(x)
#     }
#     expect_true(all(sapply(test_dataset$age_uid, uid_or_na)))
#     expect_true(all(sapply(test_dataset$sex_uid, uid_or_na)))
#     expect_true(all(is.numeric(test_dataset$value)))
#     expect_true(all(mapply(all.equal, test_dataset$age_sex_rse, as.integer(test_dataset$age_sex_rse))))
#     expect_true(all(mapply(all.equal, test_dataset$district_rse, as.integer(test_dataset$district_rse))))
#   })
# })
