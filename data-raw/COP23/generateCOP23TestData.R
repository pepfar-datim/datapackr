require(datapackr)
require(magrittr)
require(dplyr)

set.seed(5883959)

getSampleDataCodeList <- function() {
  datasets_to_pull <- tibble::tribble(
    ~dataset_uid, ~dataset_name, ~FY, ~targets_results, ~datastream, ~org_unit,
    "dA9C5bL44NX", "FY24 MER Targets", 2024, "targets", "mer", "psnu",
    "cihuwjoY5xP", "FY24 MER DOD Targets", 2024, "targets", "mer", "_mil",
    "vpDd67HlZcT", "FY24 DREAMS Targets", 2024, "targets", "dreams", "dsnu",
    "kWKJQYP1uT7", "FY24 IMPATT", 2024, "targets", "impatt", "psnu",
    "CxMsvlKepvE", "FY23 IMPATT", 2023, "targets", "impatt", "psnu",
    "bKSmkDP5YTc", "FY24 SUBNAT Targets", 2024, "targets", "subnat", "psnu",
    "J4tdiDEi08O", "FY23 SUBNAT Targets", 2023, "targets", "subnat", "psnu",
    "IXiORiVFqIv", "FY22 SUBNAT Results", 2022, "results", "subnat", "psnu")

  ds <- data.frame()

    lapply(
      datasets_to_pull$dataset_uid,
      function(x) {
        cl <- datimutils::getSqlView(sql_view_uid = "DotdxKrNZxG",
                                     variable_keys = "dataSets",
                                     variable_values = x) %>%
          dplyr::mutate(dataset_uid = x)
        ds <- rbind(ds, cl)
      }) %>%
    do.call(rbind, .) %>%
    dplyr::left_join(
      dplyr::select(datasets_to_pull, -org_unit),
      by = c("dataset_uid" = "dataset_uid"))
}

generateSampleCOP23Data <- function(x, fullCodeList) {
  country_uids <- COP21_datapacks_countries %>%
    dplyr::filter(datapack_name == x$ou) %>%
    dplyr::pull(country_uids) %>%
    unlist(.)

  psnus <- valid_OrgUnits %>%
    dplyr::filter(country_uid %in% country_uids,
                  is.na(DREAMS)) %>%
    dplyr::select(orgUnit = uid)



  #A few mechanisms
  mechs <- getMechanismView(country_uids = country_uids, cop_year = "2022") %>%
    dplyr::filter(mechanism_code %in% x$mechs) %>%
    dplyr::select(attributeOptionCombo)

  des_cocs_mer <- fullCodeList %>%
    dplyr::filter(datastream == "mer",
                  targets_results == "targets") %>%
    dplyr::select(dataelementuid, categoryoptioncombouid) %>%
    dplyr::distinct() %>%
    dplyr::full_join(mechs, by = character())

  #Subnat, DREAMS, Impatt
  des_cocs_default <-  fullCodeList %>%
    dplyr::filter(datastream %in% c("subnat", "dreams", "impatt"))  %>%
    dplyr::select(dataelementuid, categoryoptioncombouid) %>%
    dplyr::distinct() %>%
    dplyr::mutate(attributeOptionCombo = default_catOptCombo())

  ratio_des <- fullCodeList %>%
    dplyr::filter(datastream == "impatt") %>%
    dplyr::filter(grepl("_PREV", dataelement)) %>%
    dplyr::pull(dataelementuid) %>%
    unique()

  des_cocs_numeric <- rbind(des_cocs_mer, des_cocs_default) %>%
   dplyr::filter(!(dataelementuid %in% percentage_des))

  des_cocs_ratio <- des_cocs_default %>%
    dplyr::filter(dataelementuid %in% percentage_des)

  #Mock a d object
  d <- list()
  d$info$cop_year <- 2023
  d$info$country_uids <- country_uids
  d$info$tool <- "Target Setting Tool"
  d$info$operating_unit$ou <- x$ou
  d$info$datapack_name <- x$ou
  d$info$operating_unit$ou_id <- d$info$country_uids
  d$info$has_error <- FALSE
  d$info$sane_name <- getSaneName(x$ou)
  d$info$approval_status <- "UNAPPROVED"
  d$info$source_user <- "littlebobbytables"



  d$datim$cop23_sample_data <- psnus %>%
    dplyr::full_join(des_cocs_numeric, by = character()) %>%
    dplyr::arrange(orgUnit, dataelementuid, categoryoptioncombouid, attributeOptionCombo) %>%
    dplyr::mutate(period = "2023Oct", value = sample(0:200, dplyr::n(), replace = TRUE)) %>%
    dplyr::select(dataElement = dataelementuid,
                  period,
                  orgUnit,
                  categoryOptionCombo = categoryoptioncombouid,
                  attributeOptionCombo,
                  value)

  #Generate ratio values
  d$datim$cop23_prev_data <- psnus %>%
    dplyr::full_join(des_cocs_ratio, by = character()) %>%
    dplyr::arrange(orgUnit, dataelementuid, categoryoptioncombouid, attributeOptionCombo) %>%
    dplyr::mutate(period = "2023Oct", value = runif(dplyr::n())) %>%
    dplyr::select(dataElement = dataelementuid,
                  period,
                  orgUnit,
                  categoryOptionCombo = categoryoptioncombouid,
                  attributeOptionCombo,
                  value)



  if (length(d$info$country_uids) == 1) {

    d$datim$cop23_year2_sample_data  <- d$datim$cop23_sample_data %>%
      dplyr::filter(attributeOptionCombo != datapackr::default_catOptCombo()) %>% #Remove IMPATT data
      dplyr::group_by(dataElement, categoryOptionCombo) %>%
      dplyr::summarise(value = sum(round(value * 1.25)), .groups = "drop") %>%
      dplyr::mutate(
        orgUnit = d$info$country_uids,
        period = "2024Oct",
        attributeOptionCombo = datapackr::default_catOptCombo()
      ) %>%
      dplyr::select(dataElement,
                    period,
                    orgUnit,
                    categoryOptionCombo,
                    attributeOptionCombo,
                    value)
  } else {
        d$datim$cop23_year2_sample_data <- NULL
  }


  sendTimeStampLogToS3(d)
  sendDATIMExportToS3(d)
  if (!is.null(d$datim$cop23_sample_data)) {
    sendYear2ExportToS3(d)
  }


}

createS3BucketTags <- function(d) {
  d$info$country_uids <- paste(d$info$country_uids, sep = "", collapse = "_")
  tags <- c("tool", "country_uids", "cop_year", "has_error", "sane_name", "approval_status", "source_user")
  object_tags <- d$info[names(d$info) %in% tags]
  object_tags <- URLencode(paste(names(object_tags), object_tags, sep = "=", collapse = "&"))

  return(object_tags)
}

sendTimeStampLogToS3 <- function(d) {

  #Write an archived copy of the file
  s3 <- paws::s3()
  object_tags <- createS3BucketTags(d)

  object_name <-
    paste0("processed/",
           gsub("^20", "cop", d$info$cop_year),
           ifelse(d$info$cop_year == 2021, "_opu", ""),
           "/",
           d$info$sane_name,
           ".csv")

  timestamp_info <- list(
    ou = d$info$operating_unit$ou,
    ou_id = d$info$operating_unit$ou_id,
    country_name = d$info$datapack_name,
    country_uids = paste(d$info$country_uids, sep = "", collapse = ", "),
    upload_timestamp = strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%d %H:%M:%S"),
    filename = object_name
  )

  tmp <- tempfile()
  write.table(
    as.data.frame(timestamp_info),
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

  object_name <-
    paste0(
      "upload_timestamp/",
      gsub("^20", "cop", d$info$cop_year),
      ifelse(d$info$cop_year == 2021, "_opu", ""),
      "/",
      d$info$sane_name,
      ".csv"
    )

  r <- tryCatch({
    foo <- s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                         Body = raw_file,
                         Key = object_name,
                         Tagging = object_tags,
                         ContentType = "text/csv")
    message("Timestamp log sent to S3", name = "datapack")
    TRUE
  },
  error = function(err) {
    message("Timestamp log could not be saved to S3", name = "datapack")
    FALSE
  })
  unlink(tmp)
  return(r)
}

sendDATIMExportToS3 <- function(d) {
  #Write the flatpacked output
  tmp <- tempfile()

  datim_export <- d$datim$cop23_sample_data %>%
    mutate(across(everything(), as.character))

  #Need better error checking here.
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

  object_tags <- createS3BucketTags(d)

  object_name <- paste0("datim_export/", gsub("^20", "cop", d$info$cop_year),
                        ifelse(d$info$cop_year == 2021, "_opu", ""),
                        "/", d$info$sane_name, ".csv")

  s3 <- paws::s3()

  r <- tryCatch({
    foo <- s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                         Body = raw_file,
                         Key = object_name,
                         Tagging = object_tags,
                         ContentType = "text/csv")
    message("DATIM Export sent to S3", name = "datapack")
    TRUE
  },
  error = function(err) {
    message("DATIM Export could not be sent to S3", name = "datapack")
    message(err, name = "datapack")
    FALSE
  })

  unlink(tmp)

  return(r)
}

sendYear2ExportToS3 <- function(d, custom_object_name = NULL) {

  if (!is.null(d$datim$cop23_year2_sample_data)) {
    #Write the flatpacked output
    tmp <- tempfile()

    datim_export <- d$datim$cop23_year2_sample_data  %>%
      dplyr::bind_rows(d$datim$cop23_prev_data) %>%
      mutate(across(everything(), as.character))

    #Need better error checking here.
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

    object_tags <- createS3BucketTags(d)

    object_name <- paste0("datim_export/", gsub("^20", "cop", d$info$cop_year),
                          ifelse(d$info$cop_year == 2021, "_opu", ""),
                          "/", d$info$sane_name, "_Y2.csv")

    s3 <- paws::s3()

    r <- tryCatch({
      foo <- s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                           Body = raw_file,
                           Key = object_name,
                           Tagging = object_tags,
                           ContentType = "text/csv")
      message("DATIM Export sent to S3", name = "datapack")
      TRUE
    },
    error = function(err) {
      message("DATIM Export could not be sent to S3", name = "datapack")
      message(err, name = "datapack")
      FALSE
    })

    unlink(tmp)

    return(r)
  }

}


ous <- list(
  list(
    ou =  "Angola",
    mechs = c("81002", "18437")
  ),
  list(
    ou = "Caribbean Region",
    mechs = c("100081", "100129", "12567")
  ),
  list(ou = "Cameroon",
       mechs = c("85436", "81586")
  )
)

secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

fullCodeList <- getSampleDataCodeList()
lapply(ous, function(x) generateSampleCOP23Data(x, fullCodeList = fullCodeList))
