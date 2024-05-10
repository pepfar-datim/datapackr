#' @title Upload DATIM Export to PDAP
#' Title
#'
#' @param job Name of the job, i.e. PDAPAPIDomainName
#' @param endpoint Path of the endpoint, i.e. /jobs/presignedurl
#' @param query List of query parameters
#' @param service Name of the service, i.e. execute-api
#'
#' @return Returns the presigned URL list (file_key, presigned_url, expiration time)
#' @export
#'
getPresignedURL <-
  function(job,
           endpoint,
           query,
           service) {

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
      query_args = query,
      canonical_headers = list(Host = ssm_response$Parameter$Value),
      request_body = "",
      algorithm = "AWS4-HMAC-SHA256"
    )

    url <- paste0("https://", ssm_response$Parameter$Value, endpoint)

    # retreive presigned url
    response <- httr::GET(
      url = url,
      query = query,
      httr::add_headers(Authorization = auth$SignatureHeader,
                        Date = datetime)
    )

    presigned_url_data <- httr::content(response)

    if (response$status_code != 200) {
      stop("Error getting presigned url")
    }

  }


#' @title Upload DATIM Export to PDAP
#'
#' @description Extracts current COP year data from the d object,
#' formats it, and uploads the data to PDAP.
#'
#' @inheritParams datapackr_params
#'
#' @return Returns the response from the upload
#' @export
#'
uploadDATIMExportToPDAP <- function(d) {
  # Create the DATIM export
  datim_export <- createPAWExport(d)
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

  # List of paramaters for the DataPack PDAP DATIM Exports
  job <- "PDAPAPIDomainName"
  endpoint <- "/jobs/presignedurl"
  job_type <- "target_setting_tool"
  destination <- "processed"
  file_suffix <- "csv"
  service <- "execute-api"

  query <- list(job_type = job_type,
                         destination = destination,
                         file_suffix = file_suffix)

  presigned_url_data <- getPresignedURL(
    job = job,
    endpoint = endpoint,
    query = query,
    service = service
  )

  # Upload the file
  response <- httr::PUT(
    url = presigned_url_data$presigned_url,
    body = raw_file,
    httr::add_headers("Content-Type" = "text/csv")
  )

  if (response$status_code != 200) {
    warning("Error uploading file")
  }

  #Just return the raw response if we need to do anything else
  return(response)

}
