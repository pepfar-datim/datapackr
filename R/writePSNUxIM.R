#' @export
#' @importFrom magrittr %>% %<>%
#' @title writePSNUxIM(d)
#'
#' @description Checks a Data Pack for need of new or appended PSNUxIM data, then
#' writes this into the Data Pack supplied. unPackTool must be run as prerequisite.
#'
#' @param d Datapackr object
#' @param snuxim_model_data_path Filepath where SNU x IM distribution model is stored.
#' @param output_folder Local folder where you would like your Data Pack to be
#' saved upon export.
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return d
#'
writePSNUxIM <- function(d,
                        snuxim_model_data_path = NULL,
                        output_folder = NULL,
                        d2_session = dynGet("d2_default_session",
                                            inherits = TRUE)) {

  if (is.null(output_folder)) {
    interactive_warning("If no output_folder is provided, new Data Packs will not be written.")
  }

  if (is.null(snuxim_model_data_path)) {
    stop("Cannot update PSNUxIM tab without model data.")
  }

  d$keychain$snuxim_model_data_path <- snuxim_model_data_path
  d$keychain$output_folder <- output_folder

  # Start running log of all warning and information messages ####
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE

  if (d$info$has_comments_issue) {
    warning_msg <-
      paste0(
        "ERROR! Cannot update PSNUxIM information in a Data Pack with Threaded
        Comments. Please remove these and resubmit. For more information about
        the difference between Threaded Comments and Notes, see:
        https://support.office.com/en-us/article/the-difference-between-threaded-comments-and-notes-75a51eec-4092-42ab-abf8-7669077b7be3")  # nolint

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    d$info$has_error <- TRUE

    #TODO: Replace this with a centralized method
    if (NROW(d$info$messages) > 0 & interactive()) {
      options(warning.length = 8170)
      cat(crayon::red(d$info$messages$message))
    }

    return(d)
  }

  # Check whether to write anything into SNU x IM tab and write if needed ####
  if (d$info$cop_year == 2021) {
    d <- packSNUxIM(d, d2_session = d2_session)
  } else if (d$info$cop_year == 2022) {
  # Prepare data to distribute ####
    d$info$has_psnuxim <- !(NROW(d$data$SNUxIM) == 1 & is.na(d$data$SNUxIM$PSNU[1]))

    # # If does exist, extract missing combos ####
    # if (d$info$has_psnuxim) {
    #   d$data$missingCombos <- d$data$MER %>%
    #     # TODO: Create this here rather than upstream
    #     dplyr::anti_join(d$data$PSNUxIM_combos)

    #   d$info$missing_psnuxim_combos <- (NROW(d$data$missingCombos) > 0)
    # }

    # TODO: Move this into packPSNUxIM to allow that function to exit early if all good
    # Proceed IFF no PSNU x IM tab exists, or exists but with missing combos ####
    if (d$info$has_psnuxim & !d$info$missing_psnuxim_combos) {
      interactive_warning("No new information available to write to PSNUxIM tab.")
      return(d)
    }

    # Prepare targets to distribute ####
    if (d$info$has_psnuxim & d$info$missing_psnuxim_combos) {
      targets_data <- packForDATIM_UndistributedMER(data = d$data$missingCombos,
                                                    cop_year = d$info$cop_year)
    } else {
      targets_data <- d$data$UndistributedMER
    }

    # Prepare d$tool$wb ####
    d$tool$wb <- openxlsx::loadWorkbook(d$keychain$submission_path)

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
        ~dplyr::case_when(indicator_code %in% c("PMTCT_EID.N.2.T","PMTCT_EID.N.12.T")
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
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()

    ## Filter model data to match targets_data ####
    d$data$snuxim_model_data %<>%
      dplyr::right_join(
        targets_data %>% dplyr::select(-value, -attributeOptionCombo) %>% dplyr::distinct(),
        by = c("dataElement" = "dataElement",
               "period" = "period",
               "orgUnit" = "orgUnit",
               "categoryOptionCombo" = "categoryOptionCombo"))

    r <- packPSNUxIM(wb = d$tool$wb,
                     data = targets_data,
                     snuxim_model_data = d$data$snuxim_model_data,
                     cop_year = d$info$cop_year,
                     tool = d$info$tool,
                     schema = d$info$schema,
                     d2_session = d2_session)

    d$tool$wb <- r$wb
    d$info$messages <- appendMessage(d$info$messages, r$message, r$level)
    d$info$newSNUxIM <- TRUE

  } else {
    stop(paste0("Packing SNU x IM tabs is not supported for COP ", d$info$cop_year, " Data Packs."))
  }

  # If new information added to SNU x IM tab, reexport Data Pack for user ####
  if (d$info$newSNUxIM & !is.null(output_folder)) {
    interactive_print("Removing troublesome NAs that may have been added inadvertently...")
    d <- strip_wb_NAs(d)

    interactive_print("Exporting your new Data Pack...")
    exportPackr(
      data = d$tool$wb,
      output_path = d$keychain$output_folder,
      tool = "Data Pack",
      datapack_name = d$info$datapack_name)

  }

  # If warnings, show all grouped by issue ####
  printMessages(d$info$messages)

  return(d)

}
