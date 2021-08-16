#' @export
#' @importFrom magrittr %>% %<>%
#' @title Pack an OPU Data Pack
#'
#' @description
#' Takes an OPU Data Pack template, combines it with data pulled from DATIM
#' API, and produces an OPU Data Pack ready for distribution.
#'
#' @inheritParams datapackr_params
#'
#' @return Exports an OPU Data Pack to Excel within \code{output_folder}.
#'
packOPUDataPack <- function(snuxim_model_data = NULL,
                           datapack_name,
                           country_uids,
                           template_path = NULL,
                           cop_year = getCurrentCOPYear(),
                           output_folder,
                           results_archive = TRUE,
                           d2_session = dynGet("d2_default_session",
                                               inherits = TRUE)) {
  
  # Create data sidecar ####
  d <- list(
    keychain = list(
      template_path = template_path,
      output_folder = output_folder
    ),
    info = list(
      datapack_name = datapack_name,
      country_uids = country_uids,
      tool = "OPU Data Pack",
      cop_year =  cop_year
    ),
    data = list(
      snuxim_model_data = snuxim_model_data
    )
  )
  
  # Start running log of all warning and information messages
  d$info$warning_msg <- NULL
  d$info$has_error <- FALSE
  
  if (!d$info$cop_year %in% c(2020, 2021)) {
    stop("Sorry! We're only set up to run this for COP20 or COP21 OPUs.")
  }
  
  # Check if provided model data is empty ####
  if (!is.null(d$data$snuxim_model_data)) {
    
    empty_snuxim_model_data <- d$data$snuxim_model_data %>%
      dplyr::filter(rowSums(is.na(.)) != ncol(.))
    
    if (NROW(empty_snuxim_model_data) == 0) {
      warning("Provided SNUxIM model data seems empty. Attempting to retrieve data from DATIM instead.")
      d$data$snuxim_model_data = NULL
    }
  }
  
  # If empty or unprovided, pull model data from DATIM ####
  if (is.null(d$data$snuxim_model_data)) {
    d$data$snuxim_model_data <- getOPUDataFromDATIM(cop_year = d$info$cop_year,
                                             country_uids = d$info$country_uids,
                                             d2_session = d2_session)
    if (NROW(d$data$snuxim_model_data) == 0) {
      stop("SNUxIM Model data pull seems to have returned no data from DATIM. Please check with DATIM.")
    }
  }
  
  # Prepare totals data for allocation ####
  if (d$info$cop_year == 2021) {
    d$data$UndistributedMER <- d$data$snuxim_model_data %>%
        dplyr::mutate(attributeOptionCombo = default_catOptCombo()) %>%
        dplyr::group_by(dplyr::across(c(-value))) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(value != 0)
  }
  
  # Open schema ####
  d$info$schema <- pick_schema(cop_year, "OPU Data Pack")
 
  # Open template ####
    # Grab correct schema
  d$keychain$template_path <- pick_template_path(cop_year, "OPU Data Pack")
    
    # Test template against schema ####
  compareTemplateToSchema(template_path = d$keychain$template_path,
                          cop_year = d$info$cop_year,
                          tool = d$info$tool,
                          d2_session = d2_session)
    
    # Place Workbook into play ####
    d$tool$wb <- datapackr::createWorkbook(datapack_name = d$info$datapack_name,
                                           country_uids = d$info$country_uids,
                                           template_path = d$keychain$template_path,
                                           cop_year = d$info$cop_year,
                                           tool = d$info$tool,
                                           d2_session = d2_session)
    
    # Get PSNU List####
    d$data$PSNUs <- datapackr::valid_PSNUs %>%
      dplyr::filter(country_uid %in% d$info$country_uids) %>%
      add_dp_psnu(.) %>%
      dplyr::arrange(dp_psnu) %>%
      dplyr::select(PSNU = dp_psnu, psnu_uid)
    
    # Write PSNUxIM tab ####
    if (d$info$cop_year == 2020) {
      d <- packSNUxIM_OPU(d)
    } else {
      r <- packPSNUxIM(wb = d$tool$wb,
                          data = d$data$UndistributedMER,
                          snuxim_model_data = d$data$snuxim_model_data,
                          cop_year = d$info$cop_year,
                          tool = d$info$tool,
                          schema = d$info$schema,
                          d2_session = d2_session)
      
      d$tool$wb <- r$wb
      d$info$warning_msg %<>% append(r$messages)
    }
    
    # Save & Export Workbook
    print("Saving...")
    exportPackr(data = d$tool$wb,
                output_path = d$keychain$output_folder,
                tool = d$info$tool,
                datapack_name = d$info$datapack_name)
    
    # Save & Export Archive
    if (results_archive) {
      print("Archiving...")
      exportPackr(data = d,
                  output_path = d$keychain$output_folder,
                  tool = "Results Archive",
                  datapack_name = d$info$datapack_name)
    }
    
    # If warnings, show all grouped by sheet and issue
    if (!is.null(d$info$warning_msg) & interactive()) {
      options(warning.length = 8170)
      
      messages <-
        paste(
          paste(
            seq_along(d$info$warning_msg),
            ": " , d$info$warning_msg
            #stringr::str_squish(gsub("\n", "", d$info$warning_msg))
          ),
          sep = "",
          collapse = "\r\n")
      
      key = paste0(
        "*********************\r\n",
        "KEY:\r\n",
        "- WARNING!: Problematic, but doesn't stop us from processing your tool. May waive with approval from PPM and DUIT.\r\n",
        "- ERROR!: You MUST address these issues and resubmit your tool.\r\n",
        "*********************\r\n\r\n")
      
      cat(crayon::red(crayon::bold("VALIDATION ISSUES: \r\n\r\n")))
      cat(crayon::red(key))
      cat(crayon::red(messages))
    }
    
}
