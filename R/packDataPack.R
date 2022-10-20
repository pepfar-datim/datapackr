#' @export
#' @title Pack a Data Pack
#'
#' @description
#' Takes a Data Pack template, combines it with data pulled from DATIM API, and
#' produces a Data Pack ready for distribution.
#'
#' @inheritParams datapackr_params
#'
#' @return Exports a Data Pack to Excel within \code{output_folder}.
#'
packDataPack <- function(d,
                         d2_session = dynGet("d2_default_session",
                                             inherits = TRUE)) {

  # Checks and reads in Data Pack Model File ####
  stopifnot(
    "Model data file could not be read!" = canReadFile(d$keychain$model_data_path),
    "Model data is not correct file type! File must have .rds extension." =
      tools::file_ext(d$keychain$model_data_path) == "rds"
  )

  d$data$model_data <- readRDS(d$keychain$model_data_path)

  # Write Main Sheets ####
  d$tool$wb <- packDataPackSheets(wb = d$tool$wb,
                                  country_uids = d$info$country_uids,
                                  ou_level = "Prioritization",
                                  org_units = d$data$PSNUs,
                                  model_data = d$data$model_data,
                                  schema = d$info$schema,
                                  sheets = NULL,
                                  cop_year = d$info$cop_year)

  # Hide unneeded sheets ####
  sheets_to_hide <- which(stringr::str_detect(names(d$tool$wb), "PSNUxIM"))
  openxlsx::sheetVisibility(d$tool$wb)[sheets_to_hide] <- "hidden"

  # Add Styles ####
  interactive_print("Cleaning up Styles...")

  #TODO: See if new openxlsx release addresses this issue
  spectrumStyle1 <- openxlsx::createStyle(fgFill = "#9CBEBD")
  spectrumStyle2 <- openxlsx::createStyle(fgFill = "#FFEB84")

  openxlsx::addStyle(d$tool$wb,
    sheet = "Spectrum",
    spectrumStyle1,
    cols = 1:3, rows = 1:40, gridExpand = TRUE, stack = TRUE)

  openxlsx::addStyle(d$tool$wb,
    sheet = "Spectrum",
    spectrumStyle2, cols = 2, rows = 2, gridExpand = TRUE, stack = TRUE)

  # Add validations ####
  interactive_print("Adding Validations...")
  #TODO: Adding validations prevents use of openxlsx to add SNU x IM tab

  # Return d object ####
  d
}
