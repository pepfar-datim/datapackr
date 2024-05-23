#' @export
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
packOPUDataPack <- function(d,
                            undistributed_mer_data = NULL,
                            expand_formulas = FALSE,
                            d2_session = dynGet("d2_default_session",
                                                inherits = TRUE)) {

  #Case 1: We supply a model
  # Check if provided model data is empty ####
  if (!is.null(d$keychain$snuxim_model_data_path)) {

    smd <- readRDS(d$keychain$snuxim_model_data_path)
    d$data$snuxim_model_data <- smd[d$info$country_uids] %>%
      dplyr::bind_rows()
    rm(smd)

    empty_snuxim_model_data <- d$data$snuxim_model_data %>%
      dplyr::filter(rowSums(is.na(.)) != ncol(.))

    if (NROW(empty_snuxim_model_data) == 0) {
      warning("Provided SNUxIM model data seems empty. Attempting to retrieve data from DATIM instead.")
      d$data$snuxim_model_data <- NULL
    }
  }

  #Case 2: We get the model from DATIM
  # If empty or unprovided, pull model data from DATIM ####
  if (is.null(d$keychain$snuxim_model_data)) {
    d$data$snuxim_model_data <- getOPUDataFromDATIM(cop_year = d$info$cop_year,
                                                    country_uids = d$info$country_uids,
                                                    d2_session = d2_session)
    if (NROW(d$data$snuxim_model_data) == 0) {
      stop("SNUxIM Model data pull seems to have returned no data from DATIM. Please check with DATIM.")
    }
  }

  #Case 3: We have existing distributed data from the PSNUxIM tab and need to repack it.


  # Prepare totals data for allocation ####
  if (!is.null(undistributed_mer_data)) {
    d$datim$UndistributedMER <- undistributed_mer_data
  } else {
    d$datim$UndistributedMER <- d$data$snuxim_model_data %>%
      dplyr::mutate(attributeOptionCombo = default_catOptCombo(),
                    value = as.numeric(value)) %>%
      dplyr::group_by(dplyr::across(c(-value))) %>%
      #TODO: Are these not numeric here?
      dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(value != 0)
  }

  org_units <- getValidOrgUnits(d$info$cop_year) %>% # Load in valid_PSNUs list from package
    dplyr::filter(country_uid %in% d$info$country_uids) %>%
    add_dp_label(orgunits = ., cop_year = d$info$cop_year) %>%
    dplyr::arrange(dp_label) %>%
    ## Remove DSNUs
    dplyr::filter(!is.na(org_type)) %>%
    dplyr::select(dp_label, orgUnit = uid)

  assertthat::assert_that(NROW(org_units) > 0)

  # Write PSNUxIM tab ####
  r <- packPSNUxIM(wb = d$tool$wb,
                   data = d$datim$UndistributedMER,
                   snuxim_model_data = d$data$snuxim_model_data,
                   org_units = org_units,
                   cop_year = d$info$cop_year,
                   tool = d$info$tool,
                   schema = d$info$schema,
                   expand_formulas = expand_formulas,
                   d2_session = d2_session)

  d$tool$wb <- r$wb
  d$info$messages <- appendMessage(d$info$messages, r$info$messages$message, r$info$messages$level)

  # Return d object ####
  d
}
