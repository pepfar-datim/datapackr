prepareTargetsData <- function(d, append = TRUE) {
  # # If does exist, extract missing combos ####
  # if (d$info$has_psnuxim) {
  #   d$data$missingCombos <- d$data$MER %>%
  #     # TODO: Create this here rather than upstream
  #     dplyr::anti_join(d$data$PSNUxIM_combos)

  #   d$info$missing_psnuxim_combos <- (NROW(d$data$missingCombos) > 0)
  # }

  # TODO: Move this into packPSNUxIM to allow that function to exit early if all good
  # Proceed IFF no PSNU x IM tab exists, or exists but with missing combos ####
  if (d$info$has_psnuxim && !d$info$missing_psnuxim_combos) {
    interactive_warning("No new information available to write to PSNUxIM tab.")
    return(d)
  }

  #One of two things can happen here.
  #1) We are only appending new rows to the existing PSNUxIM
  #2) We already have allocated data, but are missing some combos
  #which requires that we write a completely new PSNUxIM tab
  #3) We want to keep existing allocation percentages,
  #But update the targets values which exist. We have to write a
  #New PSNU

  has_non_equal_targets <- NROW(d$tests$non_equal_targets) > 0

  if ((d$info$has_psnuxim && d$info$missing_psnuxim_combos) || has_non_equal_targets) {

    p <- d
    p$data$MER <- p$data$missingCombos
    p <- packForDATIM(p, type = "Undistributed MER")

    if (append == TRUE) {
      targets_data <- p$datim$UndistributedMER
    } else {

      if (!has_non_equal_targets) {
        dp_data <- dplyr::bind_rows(p$datim$UndistributedMER, d$datim$OPU) %>%
          dplyr::filter(attributeOptionCombo %in% c("00000", "00001"))
        } else {
          print("Using existing model data")
        psnuxim_model <- extractDataPackModel(d)

      #Get the original targets
          targets_data <-  d$datim$UndistributedMER %>%
          dplyr::select(-attributeOptionCombo) %>%
          dplyr::left_join(psnuxim_model, by = c("dataElement", "period", "orgUnit", "categoryOptionCombo")) %>%
          dplyr::mutate(percent = dplyr::case_when(is.na(percent) ~ 1,
                                                   TRUE ~ as.numeric(percent)),
                        attributeOptionCombo = dplyr::case_when(is.na(attributeOptionCombo) ~ default_catOptCombo(),
                                                                TRUE ~ attributeOptionCombo),
                        #Very likely this is going to cause problems if we round here....
                        value = value * percent) %>%
         dplyr::select(dataElement, period, orgUnit, categoryOptionCombo, attributeOptionCombo, value)
      }
    }
  } else {
    targets_data <- d$datim$UndistributedMER
  }


  #TODO: Do we really need to mirror the data for a PSNUxIM tab?
  # #Mirror the data in TA as well
  # dsd_ta_map <- getMapDataPack_DATIM_DEs_COCs(cop_year = d$info$cop_year,
  #                                             datasource = d$info$tool)
  #
  #
  # dsd_ta_map <- dsd_ta_map %>%
  #   dplyr::select(indicator_code,
  #                 dataelementuid,
  #                 support_type,
  #                 numerator_denominator,
  #                 disagg_type)  %>%
  #   dplyr::filter(support_type %in% c("DSD", "TA")) %>%
  #   dplyr::distinct() %>%
  #   tidyr::pivot_wider(names_from = "support_type", values_from = "dataelementuid") %>%
  #   dplyr::select(DSD, TA)
  #
  # ta_targets_data <- dplyr::inner_join(targets_data, dsd_ta_map, by = c("dataElement" = "DSD")) %>%
  #   dplyr::select(-dataElement) %>%
  #   dplyr::rename(dataElement = TA)
  #
  # targets_data <- dplyr::bind_rows(targets_data, ta_targets_data)

  targets_data


}

#' @export
#' @title Write PSNUxIM Tab
#'
#' @description Checks a Data Pack for need of new or appended PSNUxIM data, then
#' writes this into the Data Pack supplied. unPackTool must be run as prerequisite.
#'
#' @param append If TRUE append rows to the existing DataPack otherwise,
#' output a Missing PSNUxIM targets workbook.
#' @param snuxim_model_data_path Export from DATIM needed to allocate data
#' across mechanisms in the PSNUxIM tab
#' @inheritParams datapackr_params
#' @return d
#'
writePSNUxIM <- function(d,
                         snuxim_model_data_path = NULL,
                         output_folder = NULL,
                         d2_session = dynGet("d2_default_session",
                                             inherits = TRUE),
                         append = TRUE) {

  stopifnot(
    "Cannot update PSNUxIM tab without model data." = !is.null(snuxim_model_data_path),
    "Packing SNU x IM tabs is not supported for the requested COP year."
      = d$info$cop_year %in% supportedCOPYears(d$info$tool)
  )

  if (is.null(output_folder)) {
    interactive_warning("If no output_folder is provided, new Data Packs will not be written.")
  }

  d$keychain$snuxim_model_data_path <- snuxim_model_data_path
  d$keychain$output_folder <- output_folder

  # Start running log of all warning and information messages ####
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE

  # We normally cannot process PSNUxIM tabs with threaded comments
  # However, if we are not appending to the existing data pack, we
  # should be able to proceed.
  if (d$info$has_comments_issue && append) {
    warning_msg <-
      paste0(
        "ERROR! Cannot update PSNUxIM information in a Data Pack with Threaded
        Comments. Please remove these and resubmit. For more information about
        the difference between Threaded Comments and Notes, see:
        https://support.office.com/en-us/article/the-difference-between-threaded-comments-and-notes-75a51eec-4092-42ab-abf8-7669077b7be3")  # nolint

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    d$info$has_error <- TRUE

    #TODO: Replace this with a centralized method
    if (NROW(d$info$messages) > 0 && interactive()) {
      options(warning.length = 8170)
      cat(crayon::red(d$info$messages$message))
    }

    return(d)
  }

  # Check whether to write anything into SNU x IM tab and write if needed ####
  if (d$info$cop_year == 2022) {
  # Prepare data to distribute ####
    d$info$has_psnuxim <- !(NROW(d$data$SNUxIM) == 1 & is.na(d$data$SNUxIM$PSNU[1]))

    targets_data <- prepareTargetsData(d)

    # Prepare d$tool$wb ####
    # If append is true, add the missing PSNUxIM combos to the existing
    # workbook, otherwise, use a template.
    if (append == TRUE) {
      if (is.null(d$tool$wb)) {
        d$tool$wb <- openxlsx::loadWorkbook(d$keychain$submission_path)
      }
      openxlsx::removeFilter(d$tool$wb, names(d$tool$wb))
    } else {

      template_file <- system.file("extdata", "COP22_Data_Pack_Template.xlsx", package = "datapackr")
      wb <- openxlsx::loadWorkbook(template_file)
      openxlsx::activeSheet(wb) <- "PSNUxIM"
      sheets <- openxlsx::getSheetNames(template_file)
      sheets_to_keep <- which(sheets %in% c("Home", "PSNUxIM"))
      sheets_to_delete <- seq_along(sheets)[!(seq_along(sheets) %in% sheets_to_keep)]
      for (i in seq_along(sheets_to_delete)) {
            openxlsx::sheetVisibility(wb)[sheets_to_delete[i]] <- "veryHidden"
      }

      #These hard coded values are maybe present in the schema???
      openxlsx::writeData(wb, "Home", "Missing PSNUxIM Targets", startCol = 2, startRow = 10)
      openxlsx::writeData(wb, "Home", d$info$datapack_name, startCol = 2, startRow = 20)
      openxlsx::writeData(wb, "Home", d$info$country_uids, startCol = 2, startRow = 25)
      d$tool$wb <- wb

    }

    # Prepare d$data$snuxim_model_data ####
    smd <- readRDS(d$keychain$snuxim_model_data_path)
    d$data$snuxim_model_data <- smd[d$info$country_uids] %>%
      dplyr::bind_rows()
    rm(smd)

    dp_datim_map <- getMapDataPack_DATIM_DEs_COCs(cop_year = d$info$cop_year)

    d$data$snuxim_model_data %<>%
    ## Address issues with PMTCT_EID ####
      dplyr::mutate_at(
        c("age_option_name", "age_option_uid"),
        ~dplyr::case_when(indicator_code %in% c("PMTCT_EID.N.2.T", "PMTCT_EID.N.12.T")
                          ~ NA_character_,
                          TRUE ~ .)) %>%
    ## Convert to import file format ####
      dplyr::left_join(
        dp_datim_map,
        by = c("indicator_code" = "indicator_code",
               "age_option_uid" = "valid_ages.id",
               "sex_option_uid" = "valid_sexes.id",
               "kp_option_uid" = "valid_kps.id",
               "type" = "support_type")) %>%
      dplyr::select(dataElement = dataelementuid,
                    period,
                    orgUnit = psnu_uid,
                    categoryOptionCombo = categoryoptioncombouid,
                    attributeOptionCombo = mechanism_uid,
                    value) %>%
    ## Aggregate across 50+ age bands ####
      dplyr::group_by(dplyr::across(c(-value))) %>%
      dplyr::summarise(value = sum(value), .groups = "drop")


    org_units <-  getValidOrgUnits(d$info$cop_year) %>%
      dplyr::filter(country_uid %in% d$info$country_uids) %>%
      add_dp_label(orgunits = ., cop_year = d$info$cop_year) %>%
      dplyr::arrange(dp_label) %>%
      ## Remove DSNUs
      dplyr::filter(!is.na(org_type)) %>%
      dplyr::select(dp_label, orgUnit = uid)

    r <- packPSNUxIM(wb = d$tool$wb,
                     data = targets_data,
                     snuxim_model_data = d$data$snuxim_model_data,
                     org_units = org_units,
                     cop_year = d$info$cop_year,
                     tool = d$info$tool,
                     schema = d$info$schema,
                     d2_session = d2_session)

    d$tool$wb <- r$wb
    d$info$messages <- appendMessage(d$info$messages, r$info$messages$message, r$info$messages$level)
    d$info$newSNUxIM <- TRUE

  }

  if (d$info$cop_year == 2023) {

    d$info$has_psnuxim <- !is.null(d$data$SNUxIM)

    dp_datim_map <- getMapDataPack_DATIM_DEs_COCs(cop_year = d$info$cop_year)

    if (!d$info$has_psnuxim || !append) {
      template_file <- system.file("extdata", "COP23_PSNUxIM_Template.xlsx", package = "datapackr")
      wb <- openxlsx::loadWorkbook(template_file)
    } else {
      wb <- openxlsx::loadWorkbook(d$keychain$psnuxim_file_path)
    }

    openxlsx::activeSheet(wb) <- "PSNUxIM"

    #This is if we are dealing with no existing PSNUxIM data
    if (!d$info$has_psnuxim) {
      smd <- readRDS(d$keychain$snuxim_model_data_path)
      d$data$snuxim_model_data <- smd[d$info$country_uids] %>%
        dplyr::bind_rows()
      rm(smd)

      d$data$snuxim_model_data %<>%
        ## Address issues with PMTCT_EID ####
      dplyr::mutate_at(
        c("age_option_name", "age_option_uid"),
        ~dplyr::case_when(indicator_code %in% c("PMTCT_EID.N.2.T", "PMTCT_EID.N.12.T")
                          ~ NA_character_,
                          TRUE ~ .)) %>%
        ## Convert to import file format ####
      dplyr::left_join(
        dp_datim_map,
        by = c("indicator_code" = "indicator_code",
               "age_option_uid" = "valid_ages.id",
               "sex_option_uid" = "valid_sexes.id",
               "kp_option_uid" = "valid_kps.id",
               "type" = "support_type")) %>%
        dplyr::select(dataElement = dataelementuid,
                      period,
                      orgUnit = psnu_uid,
                      categoryOptionCombo = categoryoptioncombouid,
                      attributeOptionCombo = mechanism_uid,
                      value) %>%
        ## Aggregate across 50+ age bands ####
      dplyr::group_by(dplyr::across(c(-value))) %>%
        dplyr::summarise(value = sum(value), .groups = "drop")

    } else {
      d$data$snuxim_model_data <- prepareTargetsData(d, append)
    }


    org_units <-  getValidOrgUnits(d$info$cop_year) %>%
      dplyr::filter(country_uid %in% d$info$country_uids) %>%
      add_dp_label(orgunits = ., cop_year = d$info$cop_year) %>%
      dplyr::arrange(dp_label) %>%
      ## Remove DSNUs
      dplyr::filter(!is.na(org_type)) %>%
      dplyr::select(dp_label, orgUnit = uid)

    schema <- cop23_psnuxim_schema
    tool <- "PSNUxIM"

    r <- packPSNUxIM(wb = wb,
                     data = targets_data,
                     snuxim_model_data = d$data$snuxim_model_data,
                     org_units = org_units,
                     cop_year = d$info$cop_year,
                     tool = tool,
                     schema = schema,
                     d2_session = d2_session)

    if (d$info$cop_year == 2023) {

      country_uids <-  getValidOrgUnits(d$info$cop_year) %>%
        dplyr::filter(uid %in% org_units$orgUnit) %>%
        dplyr::pull(country_uid) %>%
        unique()

      r$wb <- writeHomeTab(wb = r$wb,
                           datapack_name = d$info$datapack_name,
                           country_uids = country_uids,
                           cop_year = d$info$cop_year,
                           tool = tool)
    }

    d$tool$wb <- r$wb
    d$info$messages <- appendMessage(d$info$messages, r$info$messages$message, r$info$messages$level)
    d$info$newSNUxIM <- TRUE

  }

  # If new information added to SNU x IM tab, reexport Data Pack for user ####
  if (d$info$newSNUxIM && !is.null(output_folder)) {
    interactive_print("Removing troublesome NAs that may have been added inadvertently...")
    d <- strip_wb_NAs(d)

    tool <- switch(as.character(d$info$cop_year),
                   "2022" = "OPU Data Pack",
                   "2023" = "PSNUxIM",
                   stop("We do not seem to have a tool for that year"))

    interactive_print("Exporting your new Data Pack...")
    d$info$output_file <- exportPackr(
      data = d$tool$wb,
      output_folder = d$keychain$output_folder,
      tool = tool,
      datapack_name = d$info$datapack_name)

  }

  # If warnings, show all grouped by issue ####
  printMessages(d$info$messages)

  return(d)

}
