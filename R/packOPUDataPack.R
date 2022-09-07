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
packOPUDataPack <- function(d, undistributed_mer_data = NULL,
                            d2_session = dynGet("d2_default_session",
                                                inherits = TRUE)) {

  # Check if provided model data is empty ####
  if (!is.null(d$info$snuxim_model_data_path)) {

    d$data$snuxim_model_data <- readRDS(d$info$snuxim_model_data_path)

    empty_snuxim_model_data <- d$data$snuxim_model_data %>%
      dplyr::filter(rowSums(is.na(.)) != ncol(.))
    # TODO: Consider replacing this with something more straightforward like:
    # all(is.na(d$data$snuxim_model_data))

    if (NROW(empty_snuxim_model_data) == 0) {
      warning("Provided SNUxIM model data seems empty. Attempting to retrieve data from DATIM instead.")
      d$data$snuxim_model_data <- NULL
    }
  }

  # If empty or unprovided, pull model data from DATIM ####
  if (is.null(d$info$snuxim_model_data)) {
    d$data$snuxim_model_data <- getOPUDataFromDATIM(cop_year = d$info$cop_year,
                                                    country_uids = d$info$country_uids,
                                                    d2_session = d2_session)
    if (NROW(d$data$snuxim_model_data) == 0) {
      stop("SNUxIM Model data pull seems to have returned no data from DATIM. Please check with DATIM.")
    }
  }

  # Prepare totals data for allocation ####
  if (!is.null(undistributed_mer_data)) {
    d$data$UndistributedMER <- undistributed_mer_data
  } else {
    d$data$UndistributedMER <- d$data$snuxim_model_data %>%
      dplyr::mutate(attributeOptionCombo = default_catOptCombo()) %>%
      dplyr::group_by(dplyr::across(c(-value))) %>%
      dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(value != 0)
  }

  # Write PSNUxIM tab ####
  r <- packPSNUxIM(wb = d$tool$wb,
                   data = d$data$UndistributedMER,
                   snuxim_model_data = d$data$snuxim_model_data,
                   cop_year = d$info$cop_year,
                   tool = d$info$tool,
                   schema = d$info$schema,
                   d2_session = d2_session)

  d$tool$wb <- r$wb
  d$info$messages <- appendMessage(d$info$messages, r$info$messages$message, r$info$messages$level)

  # Return d object ####
  d
}
