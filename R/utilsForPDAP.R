

#' @title Upload DATIM Export to PDAP
#' Title
#'
#' @param job Name of the job, i.e. PDAPAPIDomainName
#' @param endpoint Path of the endpoint, i.e. /jobs/presignedurl
#' @param job_type Type of job, i.e. target_setting_tool
#' @param destination Destination of the job, i.e. processed
#' @param file_suffix File suffix, i.e. csv
#' @param service Name of the service to execute, i.e. execute-api
#' @param payload Data to be uploaded
#' @param job_paramaters List of job parameters
#'
#' @return Returns the presigned URL list (file_key, presigned_url, expiration time)
#' @export
#'
awsJob <-
  function(job,
           endpoint,
           job_type,
           destination,
           file_suffix,
           service,
           payload,
           job_paramaters) {

    creds <- aws.signature::locate_credentials()

    ssm_client <- paws::ssm()
    ssm_response <- ssm_client$get_parameter(Name = job)
    datetime <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")

    # Create the signature
    auth <- aws.signature::signature_v4_auth(
      datetime = datetime,
      region = creds$region,
      service = service,
      verb = "GET",
      action = endpoint,
      query_args = list(
        job_type = job_type,
        destination = destination,
        file_suffix = file_suffix
      ),
      canonical_headers = list(Host = ssm_response$Parameter$Value),
      request_body = ""
    )

    url <-
      paste0("https://", ssm_response$Parameter$Value, endpoint)
    # retreive presigned url
    response <- httr::GET(
      url = url,
      query = list(
        job_type = job_type,
        destination = destination,
        file_suffix = file_suffix
      ),
      httr::add_headers(Authorization = auth$SignatureHeader,
                        Date = datetime)
    )

    presigned_url_data <- httr::content(response)

    if (response$status_code != 200) {
      stop("Error getting presigned url")
    }

    tmp <- tempfile()
    #Need better error checking here if we cannot write the file.
    write.table(
      payload,
      file = tmp,
      quote = FALSE,
      sep = "|",
      row.names = FALSE,
      na = "",
      fileEncoding = "UTF-8"
    )

    response <- httr::PUT(url = presigned_url_data$presigned_url,
                          body = list(x = httr::upload_file(tmp)))

    if (response$status_code != 200) {
      stop("Error uploading file")
    }

    #Return for further processing if needed
    return(presigned_url_data)

  }


uploadDATIMExportToPDAP <- function(d) {
  payload <- createPAWExport(d)

  job <- "PDAPAPIDomainName"
  endpoint <- "/jobs/presignedurl"
  job_type <- "target_setting_tool"
  destination <- "processed"
  file_suffix <- "csv"
  service <- "execute-api"

  #TODO...may need more here but leave for now.
  job_paramaters <- list(job_type = job_type,
                         destination = destination,
                         file_suffix = file_suffix)

  job_result <- awsJob(
    job = job,
    endpoint = endpoint,
    jobType = job_type,
    destination = destination,
    file_suffix = file_suffix,
    service = service,
    payload = payload,
    job_paramaters = job_paramaters
  )

  return(job_result)

}
