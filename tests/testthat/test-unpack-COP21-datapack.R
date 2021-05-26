context("can-unpack-COP21-datapack")

d_data_targets_names<-c("PSNU","psnuid","sheet_name","indicator_code","Age","Sex","KeyPop","value")
d_data_tests_types<-c("tbl_df","tbl","data.frame")

with_mock_api({
test_that("Can unpack all data pack sheets", {
  datimutils::loginToDATIM(config_path = test_config("test-config.json"))
  expect_true(exists("d2_default_session"))
  d <- datapackr:::createKeychainInfo(submission_path = test_sheet('COP21_DP_random_no_psnuxim.xlsx'),
                                      tool = "Data Pack",
                                      country_uids = NULL,
                                      cop_year = NULL)
  
  d <- unPackSheets(d)
  expect_true(!is.null(d$data$targets))
  expect_setequal(names(d$data$targets),d_data_targets_names)
  expect_true((NROW(d$data$targets)>0))
  expect_setequal(class(d$data$targets),c("tbl_df","tbl","data.frame"))
  expect_identical(unname(sapply(d$data$targets, typeof) ),c(rep("character",7),"double"))
  #Expect there to be test information
  #The test_name attribute should not be null
  expect_true(!is.null(d$tests))
  expect_true(all(unlist(lapply(d$tests,function(x) (setequal(class(x),d_data_tests_types))))))
  all(unlist(lapply(d$tests, function(x) !is.null(attr(x,"test_name")))))
  
  #Should throw an error if the tool is an unknown type
  d$info$tool<-"FooPack"
  expect_error(d <- unPackSheets(d))
} ) })