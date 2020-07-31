#' @export
#' @importFrom magrittr %>% %<>%
#' @title Pack a COP20 OPU Data Pack
#'
#' @description
#' Takes a COP20 OPU Data Pack template, combines it with data pulled from DATIM
#' API, and produces a COP20 OPU Data Pack ready for distribution.
#'
#' @param model_data Data from DATIM needed to pack into Data Pack
#' @param datapack_name Name you would like associated with this Data Pack.
#' (Example: "Western Hemisphere", or "Caribbean Region", or "Kenya".)
#' @param country_uids Unique IDs for countries to include in the Data Pack.
#' For full list of these IDs, see \code{datapackr::dataPackMap}.
#' @param template_path Local filepath to Data Pack template Excel (XLSX) file.
#' This file MUST NOT have any data validation formats present. If left
#' \code{NULL}, will prompt for file selection via window.
#' @param  cop_year Specifies COP year for dating as well as selection of
#' templates.
#' @param output_folder Local folder where you would like your Data Pack to be
#' saved upon export. If left as \code{NULL}, will output to
#' \code{Working Directory}.
#' @param results_archive If TRUE, will export compiled results of all tests and
#' processes to output_folder.
#'
#' @return Exports a COP20 OPU Data Pack to Excel within \code{output_folder}.
#'
packOPUDataPack <- function(model_data,
                           datapack_name,
                           country_uids,
                           template_path = NULL,
                           cop_year = getCurrentCOPYear(),
                           output_folder,
                           results_archive = TRUE) {
  
  if (cop_year != 2020) {
    stop("Sorry! We're only set up to run this for COP20 OPUs for right now. Check back later.")
  }
  
  # Create data train for use across remainder of program
  d <- list(
    keychain = list(
      template_path = template_path,
      output_folder = output_folder
    ),
    info = list(
      datapack_name = datapack_name,
      country_uids = country_uids,
      type = "OPU Data Pack",
      cop_year =  cop_year
    ),
    data = list(
      model_data = model_data
    )
  )
  
  # Open schema ####
    #TODO: Update and store schema gh849
    #d$info$schema <-  datapackr::cop20_data_pack_schema
 
  # Open template ####
    # Grab correct schema
    if (is.null(d$keychain$template_path)) {
      d$keychain$template_path <- system.file("extdata",
                                              "COP20_OPU_Data_Pack_Template.xlsx",
                                              package = "datapackr",
                                              mustWork = TRUE)
    }
    
    d$keychain$template_path <- handshakeFile(path = d$keychain$template_path,
                                              tool = "OPU Data Pack Template") 
    
    # Test template against schema ####
    print("Checking template against schema and DATIM...")
    schema <-
      unPackSchema_datapack(
        filepath = d$keychain$template,
        skip = skip_tabs(tool = "OPU Data Pack Template", cop_year = cop_year),
        cop_year = cop_year)
    
    #TODO: Update skip_tabs function gh851
    
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
                              type = "OPU Data Pack")
    
    # Get PSNU List####
    d$data$PSNUs <- datapackr::valid_PSNUs %>%
      dplyr::filter(country_uid %in% country_uids) %>%
      add_dp_psnu(.) %>%
      dplyr::arrange(dp_psnu) %>%
      dplyr::select(PSNU = dp_psnu, psnu_uid)
    
    # Write PSNUxIM tab ####
    #TODO: Create function to write the new PSNUxIM tab
    
    
    # Save & Export Workbook
    print("Saving...")
    exportPackr(data = d$tool$wb,
                output_path = d$keychain$output_folder,
                type = d$info$type,
                datapack_name = d$info$datapack_name)
    #TODO: Update exportPackr gh853
    
    # Save & Export Archive
    if (results_archive) {
      exportPackr(data = d,
                  output_path = d$keychain$output_folder,
                  type = "Results Archive",
                  datapack_name = d$info$datapack_name)
    }
    
}
