#' @export
#' @importFrom magrittr %>% %<>%
#' @title Pack a Data Pack
#' 
#' @description 
#' Takes a Data Pack template, combines it with data pulled from DATIM API, and
#' produces a Data Pack ready for distribution.
#'
#' @param datapack_uid Unique ID to assign Data Pack. Usually the same as
#' Operating Unit ID (level 3 ID). For full list of these IDs, see
#' \code{datapackr::dataPackMap}.
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

packDataPack <- function(datapack_uid = NA, #TODO: Do we need datapack_uid anymore?
                         country_uids = NA,
                         template_path = NA,
                         output_folder = getwd()) {
  
  
  # Create data train for use across remainder of program
  d <- list(
    keychain = list(
      template_path = template_path,
      output_folder = output_folder
    ),
    info = list(
      datapack_uid = datapack_uid,
      country_uids = country_uids
    )
  )
  
  # Open template ####
  d$keychain$template_path <- handshakeFile(path = d$keychain$template_path,
                                             tool = "Data Pack Template")
  
  wb <- openxlsx::loadWorkbook(d$keychain$template_path)
  
  # Derive schema from template####
  
  
  # Write Home Sheet info ####
  wb <- writeHomeTab(wb = wb,
                     datapack_uid = d$info$datapack_uid,
                     type = "Data Pack")
  
  # Get PSNU List####
  d$data$PSNUs <- getPSNUs(country_uids = d$info$country_uids,
                           include_mil = TRUE)
  
  # Write Main Sheets ####
  sheets <- readxl::excel_sheets(d$keychain$template_path)
  sheets_to_loop <- sheets[which(!stringr::str_detect(sheets, "Home|Quotes|Summary|Spectrum|SNU x IM|Visualizations|Validations"))]
  
  
  
  # Write SNU x IM tab ####
  
  
  # Add Styles ####
  
  
  # Add validations
  
  
  # Save & Export
  
  
}