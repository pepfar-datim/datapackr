context("PDAP Utility tests")

skip_on_ci()

test_that("Can generate a PDAP API location", {
  response <- getPDAPJobsAPIURL()
  pattern <- "[a-z.]+"
  expect_true(grepl(pattern, response))
})

test_that("Can get a presigned URL", {
  resp <- getPresignedURL()
  expect_identical("response", class(resp))
  expect_equal(200L, resp$status_code)

  parsed_url <- httr::parse_url(resp$url)
  expect_equal("https", parsed_url$scheme)
  expect_equal("jobs/presignedurl", parsed_url$path)
  expect_equal("target_setting_tool", parsed_url$query$job_type)
  expect_equal("processed", parsed_url$query$destination)
  expect_equal("csv", parsed_url$query$file_suffix)

  presigned_url <- httr::content(resp)
  expect_setequal(c("file_key", "presigned_url", "presigned_url_utc_expiration_time"), names(presigned_url))
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
  expect_equal("raw", class(raw_file))
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

test_that("Can get existing PDAP jobs", {

  org_unit_id <- "lZsCb6y0KDX"
  period_id <- "2023Oct"
  job_type <- "target_setting_tool"

  #Throws an error for 2023Oct?
  expect_warning(jobs <- getExistingPDAPJobs(org_unit_id = org_unit_id,
                              period_id = period_id,
                              job_type = job_type))
  expect_identical("response", class(jobs))
  expect_equal(502L, jobs$status_code)

  period_id <- "2024Oct"
  jobs <- getExistingPDAPJobs(org_unit_id = org_unit_id,
                              period_id = period_id,
                              job_type = job_type)
  expect_identical("response", class(jobs))
  expect_equal(200L, jobs$status_code)
})


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
  expect_identical("response", class(resp))
  expect_equal(200L, resp$status_code)
  response_content <- httr::content(resp)
  expect_identical("list", class(response_content))
  expect_identical(d$info$country_uids, response_content$org_unit_id)
  expect_identical("2024Oct", response_content$period_id)
  expect_identical("target_setting_tool", response_content$job_type)
  expect_identical("job_completed", response_content$job_status)
  expect_identical(file_location, response_content$job_payload$datim_export)

  #Try to get the file location again
  resp <- getExistingPDAPJobs(org_unit_id = d$info$country_uids,
                              period_id = "2024Oct",
                              job_type = "target_setting_tool")
  expect_identical("response", class(resp))
  jobs <- httr::content(resp)
  expect_identical("list", class(jobs))
  expect_true(length(jobs) > 0)
  expect_true(jobs[[1]]$job_payload$datim_export == file_location)
  expect_identical(d$info$country_uids, jobs[[1]]$job_payload$org_unit_id)
  expect_identical("2024Oct", jobs[[1]]$job_payload$period_id)
})
