#' @export
#' @title packForDATIM
#'
#' @description
#' Flexible function that allows packaging of a variety of datapackr outputs as
#' DATIM import files.
#'
#' @inheritParams datapackr_params
#' @param type Type of dataset to prep for DATIM. Choose from \code{PSNUxIM},
#' \code{SUBNAT_IMPATT}, \code{OPU PSNUxIM}, or \code{Undistributed MER}.
#'
#' @return d object
packForDATIM <- function(d, type = NULL) {

  # Call DATIM map ----
  datim_map <- datapackr::getMapDataPack_DATIM_DEs_COCs(d$info$cop_year)
  #TODO: Fix this in getMapDataPack_DATIM_DEs_COCs
  if (type == "OPU PSNUxIM" && d$info$cop_year == 2022) {
    datim_map <- cop22_map_adorn_import_file
  }

  # Check params ----
  approved_types <- c("PSNUxIM", "SUBNAT_IMPATT",
                      "OPU PSNUxIM", "Undistributed MER")

  type <- type %||% c("")

  if (!type %in% approved_types) {
    stop("Specify type: 'PSNUxIM', 'SUBNAT_IMPATT', 'OPU PSNUxIM', 'Undistributed MER'")
  }

  # Pick correct dataset ----
  data <- switch(type,
                 PSNUxIM = d$data$SNUxIM,
                 SUBNAT_IMPATT = d$data$SUBNAT_IMPATT,
                 `OPU PSNUxIM` = d$data$SNUxIM,
                 `Undistributed MER` = d$data$MER)

  if (is.null(data) || NROW(data) == 0) {
    stop(
      paste0(
        "For type '", type, "', expected to see ",
        switch(type,
               PSNUxIM = "PSNUxIM data",
               SUBNAT_IMPATT = "SUBNAT & IMPATT data",
               `OPU PSNUxIM` = "PSNUxIM data",
               `Undistributed MER` = "data from the main tabs of your Data Pack"),
        ". However, this appears to be missing."))
  }

  # Munge ----
  expected_col_names <- c("PSNU", "indicator_code", "Age", "Sex", "KeyPop",
                          "psnuid", "mech_code", "support_type", "value")

  if (type == "PSNUxIM") { ## PSNUxIM ----
    # Combine PSNUxIM distributed data with undistributed AGYW_PREV
    agyw_data <- d$data$MER %>%
      dplyr::filter(stringr::str_detect(indicator_code, "^AGYW_PREV")) %>%
      dplyr::mutate(
        support_type = "No Support Type",
        mech_code = datapackr::default_catOptCombo()) %>%
      dplyr::select(expected_col_names)

    data %<>%
      dplyr::bind_rows(agyw_data)
  }

  if (type == "Undistributed MER") { ## Undistributed MER ----
    data %<>%
      dplyr::mutate(
        support_type = dplyr::case_when(
          stringr::str_detect(indicator_code, "AGYW_PREV") ~ "No Support Type",
          TRUE ~ "DSD"),
        mech_code = default_catOptCombo())
  }

  if (type == "SUBNAT_IMPATT") { ## SUBNAT_IMPATT ----
    data %<>%
      dplyr::mutate(
        mech_code = default_catOptCombo(),
        support_type = "Sub-National") %>%
    # PATCH: Drop TX_CURR_SUBNAT.R for now
      dplyr::filter(indicator_code != "TX_CURR_SUBNAT.R")
  }

  data %<>%
    dplyr::select(expected_col_names)

  data %<>%
  # Map to dataElement & categoryOptionCombo ----
    dplyr::left_join(datim_map,
                     by = c("indicator_code" = "indicator_code",
                            "Age" = "valid_ages.name",
                            "Sex" = "valid_sexes.name",
                            "KeyPop" = "valid_kps.name",
                            "support_type" = "support_type")) %>%
  # Round value as needed ----
    dplyr::mutate(
      value =
        dplyr::case_when(
          value_type == "integer" ~ datapackr::round_trunc(value),
          TRUE ~ value)) %>%
  # Form into DATIM import file ----
    dplyr::select(dataElement = dataelementuid,
                  period,
                  orgUnit = psnuid,
                  categoryOptionCombo = categoryoptioncombouid,
                  attributeOptionCombo = mech_code,
                  value) %>%
  # Aggregate across 50+ age bands ----
    dplyr::group_by(dplyr::across(c(-value))) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
  # Drop any rows with NA in any col to prevent breakage in iHub ----
    tidyr::drop_na()

  # TEST: Blank Rows; Error ----
  # blank_rows <- data %>%
  #   dplyr::filter_all(dplyr::any_vars(is.na(.)))
  #
  # if (NROW(blank_rows) > 0) {
  #   d$tests$blank_rows_datim_subnat_impatt <- blank_rows
  #   attr(d$tests$blank_rows_datim_subnat_impatt, "test_name") <- "SUBNAT/IMPATT data with blanks"
  #   warning_msg <-
  #     paste0(
  #       "ERROR! In tab SUBNATT/IMPATT. DATIM Export has blank rows. Contact support.")
  #   d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
  #   d$info$has_error <- TRUE
  # }

  # Prioritizations ----
  if (type == "SUBNAT_IMPATT") {
    d$datim$prioritizations <- data %>%
      dplyr::filter(
        dataElement %in%
          datim_map$dataelementuid[which(datim_map$indicator_code == "IMPATT.PRIORITY_SNU.T")])
  }

  # nolint start
  switch(type,
         PSNUxIM = {d$datim$MER <- data},
         SUBNAT_IMPATT = {d$datim$subnat_impatt <- data},
         `OPU PSNUxIM` = {d$datim$OPU <- data},
         `Undistributed MER` = {d$data$UndistributedMER <- data})
  # nolint end


  return(d)
}
