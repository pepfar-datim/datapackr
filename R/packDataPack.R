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
packDataPack <- function(model_data,
                         datapack_name,
                         country_uids,
                         template_path = NULL,
                         cop_year = getCurrentCOPYear(),
                         output_folder = NULL,
                         results_archive = TRUE,
                         d2_session = dynGet("d2_default_session",
                                             inherits = TRUE)) {

  interactive_print(datapack_name)
  interactive_print(country_uids)

  if (is.null(output_folder) || file.access(output_folder, 2) != 0) {
    stop("Cannot write to output_folder")
  }

  # Create data train for use across remainder of program
  d <- list(
    keychain = list(
      template_path = template_path,
      output_folder = output_folder
    ),
    info = list(
      datapack_name = datapack_name,
      sane_name = getSaneName(datapack_name),
      country_uids = country_uids,
      tool = "Data Pack",
      cop_year =  cop_year,
      source_user = d2_session$me$userCredentials$username,
      operating_unit = getOUFromCountryUIDs(country_uids)
    ),
    data = list(
      model_data = model_data
    )
  )

  # Open schema ####
  d$info$schema <- pick_schema(cop_year,"Data Pack")

  # Open template ####
  # Grab correct schema
  if (is.null(d$keychain$template_path)) {
    if (cop_year == 2021) {
      d$info$template_filename <- "COP21_Data_Pack_Template.xlsx"
    } else if (cop_year == 2022) {
      d$info$template_filename <- "COP22_Data_Pack_Template.xlsx"
    }

    d$keychain$template_path <- system.file("extdata",
                                 d$info$template_filename,
                                 package = "datapackr",
                                 mustWork = TRUE)
  }

  d$keychain$template_path <- handshakeFile(path = d$keychain$template_path,
                                             tool = "Data Pack Template")

  # Test template against schema ####
  interactive_print("Checking template against schema and DATIM...")
  schema <-
    unPackSchema_datapack(
      template_path = d$keychain$template,
      skip = skip_tabs(tool = "Data Pack Template", cop_year = cop_year),
      cop_year = cop_year)

  if (!identical(d$info$schema, schema)) {
    stop("Template provided does not match specified schema.")
  }

  # Place Workbook into play ####
  if (is.null(d$tool$wb)) {
    d$tool$wb <- openxlsx::loadWorkbook(d$keychain$template_path)
  }

  # Set global numeric format ####
  options("openxlsx.numFmt" = "#,##0")

  # Write Home Sheet info ####
  d$tool$wb <- writeHomeTab(wb = d$tool$wb,
                            datapack_name = d$info$datapack_name,
                            country_uids = d$info$country_uids,
                            cop_year = cop_year,
                            tool = "Data Pack")

  # Get PSNU List####
  d$data$PSNUs <- datapackr::valid_PSNUs %>%
    dplyr::filter(country_uid %in% country_uids) %>%
    add_dp_psnu(.) %>%
    dplyr::arrange(dp_psnu) %>%
    ## Remove DSNUs
    dplyr::filter(!is.na(psnu_type)) %>%
    dplyr::select(PSNU = dp_psnu, psnu_uid, snu1)

  # TODO: Separate PSNUs as parameter for this function, allowing you to include
  # a list of whatever org units you want. Sites, PSNUs, Countries, whatever.

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
  ## TODO: Address this in Data Pack?
  #   ## Add styles to Summary tab
  # summaryStyle = openxlsx::createStyle(fgFill = "#404040")
  # openxlsx::addStyle(d$tool$wb, sheet = "Summary",
  # summaryStyle, cols = 1:2, rows = 1:62, gridExpand = TRUE, stack = TRUE)

    ## Add styles to Spectrum tab ####
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

  # Add validations
  interactive_print("Adding Validations...")
  #TODO: Adding validations prevents use of openxlsx to add SNU x IM tab


  # Save & Export Workbook
  interactive_print("Saving...")
  exportPackr(data = d$tool$wb,
              output_folder = d$keychain$output_folder,
              tool = d$info$tool,
              datapack_name = d$info$datapack_name)

  # Save & Export Archive
  if (results_archive) {
    exportPackr(data = d,
                output_folder = d$keychain$output_folder,
                tool = "Results Archive",
                datapack_name = d$info$datapack_name)
  }
}
