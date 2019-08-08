#' @export
#' @importFrom magrittr %>% %<>%
#' @title Pack a Data Pack
#' 
#' @description 
#' Takes a Data Pack template, combines it with data pulled from DATIM API, and
#' produces a Data Pack ready for distribution.
#'
#' @param datapack_name Name you would like associated with this Data Pack.
#' (Example: "Western Hemisphere", or "Caribbean Region", or "Kenya".)
#' @param country_uids Unique IDs for countries to include in the Data Pack.
#' For full list of these IDs, see \code{datapackr::dataPackMap}.
#' @param template_path Local filepath to Data Pack template Excel (XLSX) file.
#' This file MUST NOT have any data validation formats present. If left
#' \code{NA}, will prompt for file selection via window.
#' @param output_folder Local folder where you would like your Data Pack to be
#' saved upon export. If left as \code{NA}, will output to
#' \code{Working Directory}.
#' 
#' @return Exports a Data Pack to Excel within \code{output_folder}.
#'

packDataPack <- function(datapack_name,
                         country_uids,
                         template_path = NA,
                         model_data,
                         output_folder = getwd()) {
  
  #TODO: Combine with packSiteTool? Or merge both into packTool?
  
  # Create data train for use across remainder of program
  d <- list(
    keychain = list(
      template_path = template_path,
      output_folder = output_folder
    ),
    info = list(
      datapack_name = datapack_name,
      country_uids = country_uids,
      type = "Data Pack"
    ),
    data = list(
      model_data = model_data
    )
  )
  
  # Open schema ####
  d$info$schema <- datapackr::data_pack_schema
  
  # Open template ####
  d$keychain$template_path <- handshakeFile(path = d$keychain$template_path,
                                             tool = "Data Pack Template")
  
  # Test template against schema ####
  schema <- unPackSchema_datapack(filepath = d$keychain$template,
                        skip = skip_tabs("Data Pack Template"))
  
  if (!identical(d$info$schema, schema)) {
    stop("Ruh roh. Template provided does not match archived schema.")
  }
  
  # Place Workbook into play
  d$tool$wb <- openxlsx::loadWorkbook(d$keychain$template_path)
  
  # Write Home Sheet info ####
  d$tool$wb <- writeHomeTab(wb = d$tool$wb,
                            datapack_name = d$info$datapack_name,
                            country_uids = d$info$country_uids,
                            type = "Data Pack")
  
  # Get PSNU List####
  d$data$PSNUs <- getPSNUs(country_uids = d$info$country_uids,
                           include_mil = TRUE) %>%
    dplyr::arrange(dp_psnu)
  
  # Prepare data
  if (!all(d$info$country_uids %in% names(d$data$model_data))) {
    missing <- d$info$country_uids[!d$info$country_uids %in% names(d$data$model_data)]
    stop(
      paste0(
        "Model data file does not have data for the following country_uids: \r\n\t- ",
        paste(missing, collapse = "\r\n\t- ")
        )
      )
  }
  
  sj <- d$data$model_data %>%
    rlist::list.match(paste(d$info$country_uids, collapse = "|"))
  
  # Write Main Sheets ####
  
  
  
  
  # Write SNU x IM tab ####
  
  
  # Add Styles ####
  
  
  # Add validations
  
  
  # Save & Export Workbook
  exportPackr(data = d$tool$wb,
              output_path = d$keychain$output_folder,
              type = d$info$type,
              datapack_name = d$info$datapack_name)
  
  # Save & Export Archive
  exportPackr(data = d,
              output_path = d$keychain$output_folder,
              type = "Results Archive",
              datapack_name = d$info$datapack_name)
  
}
