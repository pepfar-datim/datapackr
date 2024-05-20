

#' Title
#' @description Generic function to interact with the PDAP Jobs API. Based on origina
#' from Patrick Linton https://gist.github.com/pashri/c889ebb79c18ca77312490217b534da4
#' @param url URL of the PDAP Jobs API
#' @param verb HTTP verb to use
#' @param query Any query parameters which should be added to the request
#' @param body The body of the request. Be sure to convert to the proper format
#' as needed. If converting to JSON, use toJSON() from the jsonlite package.
#' If uploading a file, use the raw binary data.
#' @param headers Any additional headers which should be supplied, such as "application/csv".
#'
#' @return Returns the raw response from the API
#'

aws.executeapi <- function(url, verb, query = NULL, body = "", headers = NULL) {


  if (!(verb %in% c("GET", "HEAD", "OPTIONS", "POST", "PUT", "PATCH", "DELETE"))) {
    stop("Invalid verb")
  }

  parsed_url <- httr::parse_url(url)
  datetime <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")

  request_body <- body

  canonical_headers <- list(
    Host = parsed_url$hostname
  )
  ## Create the signature

  auth <- aws.signature::signature_v4_auth(
    datetime = datetime,
    service = "execute-api",
    action = paste0("/", parsed_url$path),
    verb = verb,
    canonical_headers = canonical_headers,
    query_args = query,
    request_body = if (verb %in% c("GET", "HEAD", "OPTIONS")) "" else request_body,
    algorithm = "AWS4-HMAC-SHA256",
  )

  auth_headers <- list(
    Authorization = paste0(
      "AWS4-HMAC-SHA256 Credential=", auth$Credential,
      ", SignedHeaders=", auth$SignedHeaders,
      ", Signature=", auth$Signature
    ),
    Date = datetime
  )

  httr::VERB(
    verb = verb,
    url = url,
    body = request_body,
    query = query,
    do.call(httr::add_headers, c(auth_headers, headers))
  )

}

aws.executeapi.delete <- purrr::partial(aws.executeapi, verb = "DELETE")
aws.executeapi.get <- purrr::partial(aws.executeapi, verb = "GET")
aws.executeapi.head <- purrr::partial(aws.executeapi, verb = "HEAD")
aws.executeapi.options <- purrr::partial(aws.executeapi, verb = "OPTIONS")
aws.executeapi.patch <- purrr::partial(aws.executeapi, verb = "PATCH")
aws.executeapi.post <- purrr::partial(aws.executeapi, verb = "POST")
aws.executeapi.put <- purrr::partial(aws.executeapi, verb = "PUT")

#' Title
#' @description
#' Utility function to get the PDAP API URL from the AWS SSM Parameter Store.
#'
#' @param job Name of the job, i.e. PDAPAPIDomainName
#'
#' @return Returns the PDAP API URL
#'
getPDAPJobsAPIURL <- function(job = "PDAPAPIDomainName") {

  creds <- aws.signature::locate_credentials()
  assertthat::assert_that(!is.null(creds), msg = "No AWS credentials found")
  assertthat::assert_that(!is.null(creds$key), msg = "No AWS key found")
  assertthat::assert_that(!is.null(creds$secret), msg = "No AWS secret found")
  assertthat::assert_that(!is.null(creds$region), msg = "No AWS region found")

  ssm_client <- paws::ssm()
  #This will throw an error the the creds do not work.
  ssm_response <- ssm_client$get_parameter(Name = job)
  return(ssm_response$Parameter$Value)
}


#' @title Upload DATIM Export to PDAP
#' Title
#'
#' @param job Name of the job, i.e. PDAPAPIDomainName
#' @param endpoint Path of the endpoint, i.e. /jobs/presignedurl
#' @param query List of query parameters
#' @param service Name of the service, i.e. execute-api
#'
#' @return Returns the presigned URL list (file_key, presigned_url, expiration time)
#'
getPresignedURL <- function(job = "PDAPAPIDomainName",
                            endpoint = "/jobs/presignedurl",
                            job_type = "target_setting_tool",
                            destination = "processed",
                            file_suffix = "csv") {

  url_pdap_jobs_api <- getPDAPJobsAPIURL(job)

  query <- list(job_type = job_type,
                destination = destination,
                file_suffix = file_suffix)

  response <- aws.executeapi.get(
    url = paste0("https://", url_pdap_jobs_api, endpoint),
    query = query
  )

  if (response$status_code != 200L) {
    stop("Error getting presigned url")
  }

  return(response)

}



#' Title
#' @export
#' @description
#' Given a DataPack object, this function will extract the necessary data
#' and format it for the PDAP API. The data will be written to a CSV file
#' and returned as raw binary data. The job type will determine the format
#' of the data. Currently, only target_setting_tool and year_two_targets
#' are supported.
#'
#' @inheritParams datapackr_params
#' @param job_type The type of job to upload the data to. Currently only
#' target_setting_tool or year_two_targets are supported. An invalid job
#' type will throw an error.
#'
#' @return Returns the raw binary data of the CSV file
writePDAPExportCSV <- function(d, job_type) {

  if (job_type == "target_setting_tool") {
    datim_export <- createPAWExport(d)
  } else if (job_type == "year_two_targets") {
    datim_export <- d$datim$year2 %>%
      dplyr::mutate(value = as.character(round(value)))
  } else {
    stop("Invalid job type")
  }

  tmp <- tempfile()
  #Need better error checking here if we cannot write the file.
  write.table(
    datim_export,
    file = tmp,
    quote = FALSE,
    sep = "|",
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )

  # Load the file as a raw binary
  read_file <- file(tmp, "rb")
  raw_file <- readBin(read_file, "raw", n = file.size(tmp))
  close(read_file)
  unlink(tmp)

  return(raw_file)
}
#' @title Upload DATIM Export to PDAP
#'
#' @description Extracts current COP year data from the d object,
#' formats it, and uploads the data to PDAP.
#'
#' @inheritParams datapackr_params
#' @param job_type The type of job to upload the data to. Currently only
#' target_setting_tool or year_two_targets are supported.
#'
#' @return Returns the S3 file location of the uploaded file if successful,
#' otherwise NULL
#' @export
#'
uploadDATIMExportToPDAP <- function(raw_file, job_type, content_type = "text/csv") {

  # List of parameters for the DataPack PDAP DATIM Exports
  job <- "PDAPAPIDomainName"
  endpoint <- "/jobs/presignedurl"
  destination <- "processed"
  file_suffix <- "csv"

  query <- list(job_type = job_type,
                destination = destination,
                file_suffix = file_suffix)

  #Get the presigned URL
  presigned_url_response <- getPresignedURL(job = "PDAPAPIDomainName",
                                                   endpoint = endpoint,
                                                   job_type = job_type,
                                                   destination = destination,
                                                   file_suffix = file_suffix)

  if (presigned_url_response$status_code != 200L) {
    warning("Error getting presigned URL")
    return(NULL)
  }

  presigned_url_data <- httr::content(presigned_url_response)
  # Upload the file
  response <- httr::PUT(
    url = presigned_url_data$presigned_url,
    body = raw_file,
    httr::add_headers("Content-Type" = content_type)
  )

  if (response$status_code != 200L) {
    warning("Error uploading file")
  }

  #Return the actual file location here if successful
  return(presigned_url_data$file_key)

}

#' Title
#'
#' @param org_unit_id UID of the organization unit
#' @param period_id ISO8601 formatted period ID
#' @param job_type Type of job to get, i.e. 'target_setting_tool'
#'
#' @return Returns the response from the API. Should be a list of jobs if
#' successful, otherwise, an empty list. If an error occurs (e.g. 5xx), the
#' response will also be returned.
#' @export
#'
getExistingPDAPJobs <- function(org_unit_id, period_id, job_type) {

  url_pdap_jobs_api <- getPDAPJobsAPIURL(job = "PDAPAPIDomainName")
  endpoint <- "/jobs"

  query <- list(job_type = job_type,
                org_unit_id = org_unit_id,
                period_id = period_id)

  response <- aws.executeapi.get(
    url = paste0("https://", url_pdap_jobs_api, endpoint),
    query = query
  )

  if (response$status_code != 200L) {
    warning("Error getting existing jobs")
  }

  return(response)
}

#' Title
#'
#' @param org_unit_id UID of the organization unit
#' @param period_id ISO8601 formatted period ID
#' @param job_type Type of job to delete, i.e. 'target_setting_tool'
#'
#' @return TRUE if all jobs are deleted, otherwise, FALSE.
#' @export
#'
deleteExistingPDAPJobs <- function(org_unit_id, period_id, job_type) {

  url_pdap_jobs_api <- getPDAPJobsAPIURL(job = "PDAPAPIDomainName")
  response <- getExistingPDAPJobs(org_unit_id, period_id, job_type)

  if (response$status_code != 200L) {
    warning("Error getting existing jobs")
    return(FALSE)
  }

  existing_jobs <- httr::content(response)
  success <- TRUE

  if (length(existing_jobs) > 0) {
   for (i in seq_along(existing_jobs)) {
     response <- aws.executeapi.delete(
       url = paste0("https://", url_pdap_jobs_api, "/jobs/", existing_jobs[[i]]$job_id)
     )
     if (response$status_code != 200L) {
       success <- FALSE
       warning("Error deleting existing jobs")
     }
   }
  }

  return(success)
}

#' Title
#'
#' @param job_type Type of the job, i.e. target_setting_tool
#' @param datim_export Location of the S3 file
#' @param org_unit_id UID of the organisation unit
#' @param period_id ISO8601 formatted period ID
#'
#' @return Returns the result of the job initiation
#' @export
#'
initiatePDAPJob <- function(job_type, datim_export, org_unit_id, period_id) {

  if (!inherits(datim_export, "character")) {
    stop("Invalid S3 file object type.")
  }

  s3_regex <- "^s3://[a-zA-Z0-9/._-]+\\.csv$"
  if (!grepl(s3_regex, datim_export)) {
    stop("Invalid S3 file location")
  }

  url_pdap_jobs_api <- getPDAPJobsAPIURL(job = "PDAPAPIDomainName")

  query <- list(job_type = job_type,
                datim_export = datim_export,
                org_unit_id = org_unit_id,
                period_id = period_id)

  #Any existing jobs need to be removed
  jobs_deleted <- deleteExistingPDAPJobs(org_unit_id, period_id, job_type)
  if (!jobs_deleted) {
    warning("Error deleting existing jobs")
  }

  endpoint <- "/jobs"
  #Initiate the job
  response <- aws.executeapi.post(
    url = paste0("https://", url_pdap_jobs_api, endpoint),
    body = jsonlite::toJSON(query, auto_unbox = TRUE),
    headers = list("Content-Type" = "application/json")
  )

  if (response$status_code != 200L) {
    warning("Error initiating job")
  }

  return(response)

}

#' Title
#'
#' @param org_unit_id UID of the organisation unit
#' @param period_id ISO8601 formatted period ID
#' @param approval_status One of 'submitted', 'cancelled', 'approved', 'rejected'
#'
#' @return Returns the response from the API when changing the approval status
#' @export
#'
changePDAPJobApprovalStatus <- function(org_unit_id, period_id, approval_status) {

  if (!(approval_status %in% c("submitted", "cancelled", "approved", "rejected"))) {
    stop("Invalid approval status")
  }

  url_pdap_jobs_api <- getPDAPJobsAPIURL(job = "PDAPAPIDomainName")

  #Get the job
  jobs <- getExistingPDAPJobs(org_unit_id = org_unit_id,
                             period_id = period_id,
                             job_type = "target_setting_tool") %>% httr::content()
  if (length(jobs) == 0) {
    warning("No job found")
    return(NULL)
  }

  endpoint <- paste0("/jobs/", jobs[[1]]$job_id)

  pl <- list(approval_info = list(approval_status = approval_status))

  response <- aws.executeapi.patch(
    url = paste0("https://", url_pdap_jobs_api, endpoint),
    body = jsonlite::toJSON(pl, auto_unbox = TRUE),
    headers = list("Content-Type" = "application/json")
  )

  if (response$status_code != 200L) {
    warning("Error changing job status")
  }

  return(response)

}


#' Title
#' @description
#' Given a job type, organization unit ID, and period ID, this function will
#' return the S3 file location of the existing job. If no job is found, NULL
#' will be returned. If more than one job is found, a warning will be issued
#' and the first job will be used. If an error occurs, NULL will be returned.
#'
#' @param job_type Type of job to get, i.e. 'target_setting_tool'
#' @param org_unit_id UID of the organization unit
#' @param period_id ISO8601 formatted period ID
#'
#' @return Returns the S3 file location of the existing job, otherwise NULL
#' @export
#'
getExistingFileS3Location <- function(job_type, org_unit_id, period_id) {

  url_pdap_jobs_api <- getPDAPJobsAPIURL(job = "PDAPAPIDomainName")
  endpoint <- "/jobs"
  service <- "execute-api"

  query <- list(job_type = job_type,
                org_unit_id = org_unit_id,
                period_id = period_id)

  response <- getExistingPDAPJobs(org_unit_id, period_id, job_type)

  if (response$status_code != 200L) {
    warning("Error getting existing jobs")
    return(NULL)
  } else {
    existing_jobs <- httr::content(response)
  }

  if (length(existing_jobs) == 0) {
    return(NULL)
  }

  if (length(existing_jobs) > 1) {
    warning("More than one job found. Using the first one.")
  }

  existing_file_s3_location <- existing_jobs[[1]]$job_payload$datim_export

  return(existing_file_s3_location)

}
