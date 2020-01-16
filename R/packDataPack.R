#' @export
#' @importFrom magrittr %>% %<>%
#' @title Pack a Data Pack
#'
#' @description
#' Takes a Data Pack template, combines it with data pulled from DATIM API, and
#' produces a Data Pack ready for distribution.
#'
#' @param model_data Data from DATIM needed to pack into Data Pack
#' @param datapack_name Name you would like associated with this Data Pack.
#' (Example: "Western Hemisphere", or "Caribbean Region", or "Kenya".)
#' @param country_uids Unique IDs for countries to include in the Data Pack.
#' For full list of these IDs, see \code{datapackr::dataPackMap}.
#' @param template_path Local filepath to Data Pack template Excel (XLSX) file.
#' This file MUST NOT have any data validation formats present. If left
#' \code{NA}, will prompt for file selection via window.
#' @param  cop_year Specifies COP year for dating as well as selection of
#' templates.
#' @param output_folder Local folder where you would like your Data Pack to be
#' saved upon export. If left as \code{NA}, will output to
#' \code{Working Directory}.
#' @param results_archive If TRUE, will export compiled results of all tests and
#' processes to output_folder.
#'
#' @return Exports a Data Pack to Excel within \code{output_folder}.
#'
#TODO Remove use of getwd()!!
packDataPack <- function(model_data,
                         datapack_name,
                         country_uids,
                         template_path = NULL,
                         cop_year = getCurrentCOPYear(),
                         output_folder = getwd(),
                         results_archive = TRUE) {

  #TODO: Combine with packSiteTool? Or merge both into packTool?
  print(datapack_name)
  print(country_uids)

  # Create data train for use across remainder of program
  d <- list(
    keychain = list(
      template_path = template_path,
      output_folder = output_folder
    ),
    info = list(
      datapack_name = datapack_name,
      country_uids = country_uids,
      type = "Data Pack",
      cop_year =  cop_year
    ),
    data = list(
      model_data = model_data
    )
  )

  # Open schema ####
  if (cop_year == 2020) {
   d$info$schema <-  datapackr::cop20_data_pack_schema
  } else {d$info$schema <- datapackr::data_pack_schema}

  # Open template ####
  # Grab correct schema
  if (is.null(d$keychain$template_path)) {
    d$keychain$template_path <- system.file("extdata",
                                 "COP20_Data_Pack_Template_vFINAL.xlsx",
                                 package = "datapackr",
                                 mustWork = TRUE)
  }

  d$keychain$template_path <- handshakeFile(path = d$keychain$template_path,
                                             tool = "Data Pack Template")

  # Test template against schema ####
  print("Checking template against schema and DATIM...")
  schema <-
    unPackSchema_datapack(
      filepath = d$keychain$template,
      skip = skip_tabs(tool = "Data Pack Template", cop_year = cop_year),
      cop_year = cop_year)

  if (!identical(d$info$schema, schema)) {
    stop("Ruh roh. Template provided does not match archived schema.")
  }

  # Place Workbook into play ####
  d$tool$wb <- openxlsx::loadWorkbook(d$keychain$template_path)

  # Set global numeric format ####
  options("openxlsx.numFmt" = "#,##0")

  # Write Home Sheet info ####
  d$tool$wb <- writeHomeTab(wb = d$tool$wb,
                            datapack_name = d$info$datapack_name,
                            country_uids = d$info$country_uids,
                            cop_year = cop_year,
                            type = "Data Pack")

  # Get PSNU List####
  d$data$PSNUs <- datapackr::valid_PSNUs %>%
    dplyr::filter(country_uid %in% country_uids) %>%
    add_dp_psnu(.) %>%
    dplyr::arrange(dp_psnu) %>%
    dplyr::select(PSNU = dp_psnu, psnu_uid)
  # TODO: Separate PSNUs as parameter for this function, allowing you to include
  # a list of whatever org units you want. Sites, PSNUs, Countries, whatever.

  # TODO: AFTER regionalization is deployed to DATIM, add lastUpdated lookup to
  # check whether PSNUs have been updated at all since valid_PSNUs was last run.

  # Write Main Sheets ####
  d$tool$wb <- packDataPackSheets(wb = d$tool$wb,
                                  country_uids = d$info$country_uids,
                                  ou_level = "Prioritization",
                                  org_units = d$data$PSNUs,
                                  model_data = d$data$model_data,
                                  schema = d$info$schema,
                                  sheets = NULL,
                                  cop_year = d$info$cop_year)

  # Write SNU x IM tab ####
  # print("Writing SNU x IM tab. This can sometimes take a few minutes...")
  # TODO: Move this to separate function for use in shiny app
  psnu_sheet_num <- grep("PSNUxIM",names(d$tool$wb))
  openxlsx::sheetVisibility(d$tool$wb)[psnu_sheet_num] <- "hidden"

  # Add Styles ####
  print("Cleaning up Styles...")
  ## TODO: Address this in Data Pack?
  #   ## Add styles to Summary tab
  # summaryStyle = openxlsx::createStyle(fgFill = "#404040")
  # openxlsx::addStyle(d$tool$wb, sheet = "Summary", summaryStyle, cols = 1:2, rows = 1:62, gridExpand = TRUE, stack = TRUE)

    ## Add styles to Spectrum tab
  #TODO: See if new openxlsx release addresses this issue
  spectrumStyle1 = openxlsx::createStyle(fgFill = "#9CBEBD")
  spectrumStyle2 = openxlsx::createStyle(fgFill = "#FFEB84")
  openxlsx::addStyle(d$tool$wb, sheet = "Spectrum", spectrumStyle1, cols = 1:3, rows = 1:40, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(d$tool$wb, sheet = "Spectrum", spectrumStyle2, cols = 2, rows = 2, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(d$tool$wb, sheet = "Spectrum IDs", spectrumStyle1, cols = 1:3, rows = 1:40, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(d$tool$wb, sheet = "Spectrum IDs", spectrumStyle2, cols = 2, rows = 2, gridExpand = TRUE, stack = TRUE)

  # Add validations
  print("Adding Validations...")
  #TODO: Adding validations prevents use of openxlsx to add SNU x IM tab


  # Save & Export Workbook
  print("Saving...")
  exportPackr(data = d$tool$wb,
              output_path = d$keychain$output_folder,
              type = d$info$type,
              datapack_name = d$info$datapack_name)

  # Save & Export Archive
  if (results_archive) {
    exportPackr(data = d,
                output_path = d$keychain$output_folder,
                type = "Results Archive",
                datapack_name = d$info$datapack_name)
  }
}
