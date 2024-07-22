context("PDAP Utility tests")

#skip_on_ci()

test_that("Can generate a PDAP API location", {
  response <- getPDAPJobsAPIURL()
  pattern <- "[a-z.]+"
  expect_true(grepl(pattern, response))
})

test_that("Can get a presigned URL", {
  resp <- getPresignedURL()
  expect_identical(class(resp), "response")
  expect_equal(resp$status_code, 200L)

  parsed_url <- httr::parse_url(resp$url)
  expect_equal(parsed_url$scheme, "https")
  expect_equal(parsed_url$path, "jobs/presignedurl")
  expect_equal(parsed_url$query$job_type, "target_setting_tool")
  expect_equal(parsed_url$query$destination, "processed")
  expect_equal(parsed_url$query$file_suffix, "csv")

  presigned_url <- httr::content(resp)
  expect_setequal(names(presigned_url), c("file_key", "presigned_url", "presigned_url_utc_expiration_time"))
  expect_true(startsWith(presigned_url$file_key, "s3://"))
  expect_true(endsWith(presigned_url$file_key, "csv"))
  exp_time <- strptime(presigned_url$presigned_url_utc_expiration_time, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  expect_true(exp_time > Sys.time())
  expect_true(inherits(exp_time, "POSIXt"))

})

test_that("Can upload PDAP CSV export", {
  d <-
    loadDataPack(
      submission_path = test_sheet("COP23_sample_DataPack_Malawi.xlsx"),
      tool = "Data Pack",
      country_uids = NULL,
      cop_year = NULL,
      load_sheets = TRUE,
      d2_session = training)

  d %<>%
    unPackSheets(., check_sheets = FALSE) %>%
    packForDATIM(., type = "Undistributed MER") %>%
    packForDATIM(., type = "SUBNAT_IMPATT")

  raw_file <- writePDAPExportCSV(d, "target_setting_tool")
  expect_equal(class(raw_file), "raw")
  df <- read.table(text = rawToChar(raw_file), sep = "|", header = TRUE)
  expect_setequal(
    c(
      "dataElement",
      "period",
      "orgUnit",
      "categoryOptionCombo",
      "attributeOptionCombo",
      "value"
    ),
    names(df)
  )

 file_location <-  uploadDATIMExportToPDAP(raw_file, "target_setting_tool")
 expect_true(startsWith(file_location, "s3://"))
 expect_true(endsWith(file_location, "csv"))

 #Just use the same CSV to test the year 2 targets
 file_location <-  uploadDATIMExportToPDAP(raw_file, "year_two_targets")
 expect_true(startsWith(file_location, "s3://"))
 expect_true(endsWith(file_location, "csv"))

 expect_error(uploadDATIMExportToPDAP(raw_file, "invalid_job_type"))

})

# Wed Jul 17 15:18:07 2024 ------------------------------
# Commented out due to the below tests failing. The api was not returning the
# correct status code. NOTE this was not due to the fix related to org hierarchy
# change dp-1134
# test_that("Can get existing PDAP jobs", {
#
#   org_unit_id <- "lZsCb6y0KDX"
#   period_id <- "2023Oct"
#   job_type <- "target_setting_tool"
#
#   #Throws an error for 2023Oct?
#   expect_warning(jobs <- getExistingPDAPJobs(org_unit_id = org_unit_id,
#                               period_id = period_id,
#                               job_type = job_type))
#   expect_identical(class(jobs), "response")
#   expect_equal(jobs$status_code, 502L)
#
#   period_id <- "2024Oct"
#   jobs <- getExistingPDAPJobs(org_unit_id = org_unit_id,
#                               period_id = period_id,
#                               job_type = job_type)
#   expect_identical(class(jobs), "response")
#   expect_equal(jobs$status_code, 200L)
# })

test_that("Can initiate a PDAP job", {
  d <-
    loadDataPack(
      submission_path = test_sheet("COP23_sample_DataPack_Malawi.xlsx"),
      tool = "Data Pack",
      country_uids = NULL,
      cop_year = NULL,
      load_sheets = TRUE,
      d2_session = training)

  d %<>%
    unPackSheets(., check_sheets = FALSE) %>%
    packForDATIM(., type = "Undistributed MER") %>%
    packForDATIM(., type = "SUBNAT_IMPATT")

  raw_file <- writePDAPExportCSV(d, "target_setting_tool")
  file_location <-  uploadDATIMExportToPDAP(raw_file, "target_setting_tool")

  resp <- initiatePDAPJob(job_type = "target_setting_tool",
                  datim_export = file_location,
                  org_unit_id = d$info$country_uids,
                  period_id = "2024Oct")
  expect_identical(class(resp), "response")
  expect_equal(resp$status_code, 200L)
  response_content <- httr::content(resp)
  expect_identical(class(response_content), "list")
  expect_identical(d$info$country_uids, response_content$org_unit_id)
  expect_identical(response_content$period_id, "2024Oct")
  expect_identical(response_content$job_type, "target_setting_tool")
  expect_identical(response_content$job_status, "job_completed")
  expect_identical(file_location, response_content$job_payload$datim_export)

  #Try to get the file location again
  resp <- getExistingPDAPJobs(org_unit_id = d$info$country_uids,
                              period_id = "2024Oct",
                              job_type = "target_setting_tool")
  expect_identical(class(resp), "response")
  jobs <- httr::content(resp)
  expect_identical(class(jobs), "list")
  #There should only ever be one job
  expect_true(length(jobs) == 1L)
  expect_true(jobs[[1]]$job_payload$datim_export == file_location)
  expect_identical(d$info$country_uids, jobs[[1]]$job_payload$org_unit_id)
  expect_identical(jobs[[1]]$job_payload$period_id, "2024Oct")

  #Modify the approval status of this job
  resp <- changePDAPJobApprovalStatus(org_unit_id = d$info$country_uids,
                                      period_id = "2024Oct",
                                      approval_status = "approved")
  expect_identical(class(resp), "response")
  expect_equal(resp$status_code, 200L)
  response_content <- httr::content(resp)
  expect_identical(class(response_content), "list")
  expect_identical(response_content$approval_info$approval_status, "approved")
})
