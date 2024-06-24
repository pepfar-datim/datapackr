prepareTargetsData <- function(d, append = TRUE) {

  if (d$info$has_psnuxim) {
    has_non_equal_targets <- NROW(d$tests$non_equal_targets) > 0

    if (d$info$missing_psnuxim_combos || has_non_equal_targets) {
      p <- d
      p$data$MER <- p$data$missingCombos
      p <- packForDATIM(p, type = "Undistributed MER")
      targets_data <- p$datim$UndistributedMER

      #Only the missing rows
      if (append) {
        return(targets_data)
      }

      #In this case, we need a full refresh
      #From the existing model
      if (!append && has_non_equal_targets) {
        psnuxim_model <- extractDataPackModel(d)
        #Get the original targets
        targets_data <-  d$datim$UndistributedMER %>%
          dplyr::select(-attributeOptionCombo) %>%
          dplyr::left_join(
            psnuxim_model,
            by = c(
              "dataElement",
              "period",
              "orgUnit",
              "categoryOptionCombo"
            )
          ) %>%
          dplyr::mutate(
            percent = dplyr::case_when(is.na(percent) ~ 1,
                                       TRUE ~ as.numeric(percent)),
            attributeOptionCombo = dplyr::case_when(
              is.na(attributeOptionCombo) ~ default_catOptCombo(),
              TRUE ~ attributeOptionCombo
            ),
            #Very likely this is going to cause problems if we round here....
            value = value * percent
          ) %>%
          dplyr::select(
            dataElement,
            period,
            orgUnit,
            categoryOptionCombo,
            attributeOptionCombo,
            value
          )
        return(targets_data)
      }

      if (!append && !has_non_equal_targets) {

        targets_data <- d$datim$OPU %>%
          dplyr::filter(!(attributeOptionCombo %in% c("000000", "00001"))) %>%
          dplyr::bind_rows(targets_data)
        }

    }
    } else {
      return(d$datim$UndistributedMER)
    }

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
#' @param use_template If TRUE, use a template to create the object, otherwise
#' use the existing workbook object which is part of the main d object.
#' @inheritParams datapackr_params
#' @return d
#'
writePSNUxIM <- function(d,
                         snuxim_model_data_path = NULL,
                         output_folder = NULL,
                         d2_session = dynGet("d2_default_session",
                                             inherits = TRUE),
                         append = TRUE,
                         use_template = FALSE) {

  d$keychain$snuxim_model_data_path <- snuxim_model_data_path %||% d$keychain$snuxim_model_data_path
  d$keychain$output_folder <- output_folder %||% d$keychain$output_folder

  stopifnot(
    "Cannot update PSNUxIM tab without model data." = !is.null(snuxim_model_data_path),
    "Packing PSNUxIM tabs is not supported for the requested COP year."
      = d$info$cop_year %in% supportedCOPYears(d$info$tool)
  )

  if (!d$info$needs_psnuxim && d$info$has_psnuxim) {
     interactive_warning("It does not appear that you need a new PSNUxIM tab.")
    return(d)
  }

  if (is.null(output_folder)) {
    interactive_warning("If no output_folder is provided, new Data Packs will not be written.")
  }

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

    if (NROW(d$info$messages) > 0 && interactive()) {
      options(warning.length = 8170)
      cat(crayon::red(d$info$messages$message))
    }

    return(d)
  }

    d$info$has_psnuxim <- !is.null(d$data$SNUxIM)

    dp_datim_map <- getMapDataPack_DATIM_DEs_COCs(cop_year = d$info$cop_year)
    targets_data <- prepareTargetsData(d, append)
    template_file <- system.file("extdata",
                                 paste0("COP",
                                        d$info$cop_year %% 100,
                                        "_PSNUxIM_Template.xlsx"
                                        ),
                                 package = "datapackr")

    if (!d$info$has_psnuxim) {
      print("Loading initial PSNUxIM Template")
      wb <- openxlsx::loadWorkbook(template_file)
    } else {
      if (use_template) {
        wb <- openxlsx::loadWorkbook(template_file)
      } else {
        wb <- openxlsx::loadWorkbook(d$keychain$psnuxim_file_path)
      }
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
      d$data$snuxim_model_data <- targets_data
    }


    org_units <-  getValidOrgUnits(d$info$cop_year) %>%
      dplyr::filter(country_uid %in% d$info$country_uids) %>%
      add_dp_label(orgunits = ., cop_year = d$info$cop_year) %>%
      dplyr::arrange(dp_label) %>%
      ## Remove DSNUs
      dplyr::filter(!is.na(org_type)) %>%
      dplyr::select(dp_label, orgUnit = uid)

    schema <- pick_schema(d$info$cop_year, "PSNUxIM")

    tool <- "PSNUxIM"

    r <- packPSNUxIM(wb = wb,
                     data = targets_data,
                     snuxim_model_data = d$data$snuxim_model_data,
                     org_units = org_units,
                     cop_year = d$info$cop_year,
                     tool = tool,
                     schema = schema,
                     d2_session = d2_session)

      country_uids <-  getValidOrgUnits(d$info$cop_year) %>%
        dplyr::filter(uid %in% org_units$orgUnit) %>%
        dplyr::pull(country_uid) %>%
        unique()

      r$wb <- writeHomeTab(wb = r$wb,
                           datapack_name = d$info$datapack_name,
                           country_uids = country_uids,
                           cop_year = d$info$cop_year,
                           tool = tool)

    d$tool$wb <- r$wb
    d$info$messages <- appendMessage(d$info$messages, r$info$messages$message, r$info$messages$level)
    d$info$newSNUxIM <- TRUE


  # If new information added to SNU x IM tab, reexport Data Pack for user ####
  if (d$info$newSNUxIM && !is.null(output_folder)) {
    interactive_print("Removing troublesome NAs that may have been added inadvertently...")
    d <- strip_wb_NAs(d)

    tool <- switch(as.character(d$info$cop_year),
                   "2023" = "PSNUxIM",
                   "2024" = "PSNUxIM",
                   "2025" = "PSNUxIM",
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
