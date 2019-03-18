#' @export
#' @title Frame a mechanism mapping tool.
#' 
#' @description 
#' Creates shell of a mechanism mapping tool from scratch and prepares it for
#' addition of other features and data.
#' 
#' @param datapack_uid A unique ID specifying the PEPFAR Operating Unit 
#' the Site Tool belongs to.
#' 
#' @return OpenXLSX workbook object.
#' 
frameMechMap <- function(datapack_uid) {
  # Create Workbook ####
    wb <- openxlsx::createWorkbook(
      creator = "PRIME Information Systems Team"
    )
  
  # Modify Base Font ####
    openxlsx::modifyBaseFont(wb, fontName = "Calibri", fontSize = 11)
    
  # Add Home tab ####
    wb <- writeHomeTab(wb = wb,
                        data_pack_uid = datapack_uid,
                        type = "Mechanism Map")
    
  # Add other tabs ####
    sheet_names <- c("Old Mechs", "New Mechs", "Validations", "Mechanism Map")
    visible <- c(TRUE, TRUE, FALSE, TRUE)
    
    mapply(function(x, y) openxlsx::addWorksheet(wb, sheetName = x, visible = y),
           sheet_names,
           visible)
    
  # Write title into Mechanism Map sheet ####
    openxlsx::writeData(wb = wb,
                        sheet = "Mechanism Map",
                        x = "Mechanism Map",
                        xy = c(1,1),
                        colNames = FALSE)
    
  # Write New Mechanism title ####
    openxlsx::writeData(wb = wb,
                        sheet = "Mechanism Map",
                        x = "New Mechanisms",
                        xy = c(6,2),
                        colNames = FALSE)
    
  # Write other headers into Mech Map sheet ####
    headers = tibble::tribble(
      ~A,~B,~C,~D,~E,
      "Old Mechanism", "PSNU", "Indicator", "Type", "Distribution Check")
    openxlsx::writeData(wb = wb,
                        sheet = "Mechanism Map",
                        x = headers,
                        xy = c(1,3),
                        colNames = FALSE)
    
  # Add dist check to Mech Map ####
    checkFx <- paste0(
      '=IF(A', 4:200, '<>"",SUM(F', 4:200, ':AD', 4:200, '),"")')
    
    openxlsx::writeFormula(
      wb, sheet = "Mechanism Map",
      x = checkFx,
      xy = c(5, 4)
    )
    
    
  # Add Styles ####
    ## Title
    openxlsx::addStyle(wb, sheet = "Mechanism Map",
                       style = datapackr::styleGuide$data$title,
                       rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)
    ## Labels
    mechColHeaders <- openxlsx::createStyle(halign = "center",
                                            valign = "center",
                                            textRotation = 90,
                                            fgFill = "#9CBEBD",
                                            textDecoration = "bold")
    
    openxlsx::addStyle(wb, sheet = "Mechanism Map", style = mechColHeaders,
                       rows = 3, cols = 6:30, gridExpand = TRUE, stack = TRUE)
    
    ## Row Headers
    openxlsx::addStyle(wb, sheet = "Mechanism Map",
                       style = datapackr::styleGuide$data$rowHeader,
                       rows = 3, cols = 1:5,
                       gridExpand = TRUE, stack = TRUE)
    
    openxlsx::setRowHeights(wb = wb,
                            sheet = "Mechanism Map",
                            rows = 3,
                            heights = 140)
    openxlsx::setColWidths(wb = wb, 
                           sheet = "Mechanism Map",
                           cols = 1:30,
                           widths = c(33,33,33,"auto","auto",rep(5,25)))
    
    ## New Mech header
    openxlsx::addStyle(wb, "Mechanism Map",
                       style = openxlsx::createStyle(textDecoration = "bold"),
                       rows = 2, cols = 6, stack = TRUE)
    
    ## Percentage Styles
    openxlsx::addStyle(wb, "Mechanism Map",
                       style = openxlsx::createStyle(numFmt = "0%"),
                       rows = 4:200, cols = 5:30,
                       gridExpand = TRUE, stack = TRUE)
    
  # Freeze Panes ####
    openxlsx::freezePane(wb = wb, sheet = "Mechanism Map",
                         firstActiveRow = 4, firstActiveCol = 6)
    
  return(wb)

}


#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom lubridate mdy
#' 
#' @title Create Mechanism Mapping tool
#' 
#' @description 
#' Produces an Excel tool to allow PEPFAR Country Teams to map shifting
#' Implementing Mechanism service patterns to allow automated distribution of
#' site-level data between distinct mechanisms.
#' 
#' @param datapack_name Usually the same as Operating Unit, except in rare
#'   cases for some PEPFAR Regional Operating Units. Call
#'   \code{datapackr::dataPackMap} to see syntax for Regional Programs.
#' @param FY Numeric value of COP Fiscal Year. (For COP19/FY20 targets, enter
#' \code{2019}.)
#' @param output_path A local path directing to the folder where you would like
#' outputs to be saved. If not supplied, will output to working directory.
#' @param includeFACTS Logical. If \code{TRUE}, \code{packMechMap} will corrob-
#' orate FY2019 DATIM mechanisms against FACTS Info Mechanisms and add any
#' missing mechanisms. Default is \code{FALSE}.
#' @param FACTSMechs_path A local file path directing to the extract of FY2019
#' FACTS Info Mechanisms. Default is \code{NA}.
#' 
#' @return Excel workbook containing features necessary to allow various mechanism
#' mapping scenarios, which can be fed back into the Data Pack site distribution
#' process.
#' 
packMechMap <- function(datapack_name,
                        FY,
                        output_path = NA,
                        includeFACTS = FALSE,
                        FACTSMechs_path = NA) {

  # Log into DATIM ####
  #    If user has not already logged in using DATIM credentials, request
  #    credentials via console and getPass.
    #datapackr::loginToDATIM(getOption("secrets"))

  datapack_uid <- datapackr::dataPackMap %>%
    dplyr::filter(data_pack_name == datapack_name) %>%
    dplyr::select(model_uid) %>%
    dplyr::distinct() %>%
    dplyr::pull(model_uid)
  
  # Frame Workbook ####
    wb <- frameMechMap(datapack_uid)
  
  # Write PSNU list ####
    PSNUs <- getPSNUs(datapack_uid) %>%
      dplyr::select(PSNU = DataPackSiteID)
    
    openxlsx::writeDataTable(
      wb = wb,
      sheet = "Validations",
      x = PSNUs,
      xy = c(1,1),
      colNames = TRUE,
      tableName = "psnus",
      tableStyle = "none",
      withFilter = TRUE
    )
  
  # Write Mech Lists ####
    country_uids <- datapackr::dataPackMap %>%
      dplyr::filter(model_uid == datapack_uid) %>%
      dplyr::pull(country_uid)
    oldMechList <- getMechList(country_uids,
                              COP_FY = (FY-1))
    newMechList <- getMechList(country_uids,
                              COP_FY = FY)
    
    ## PATCH for Missing FACTS Mechs
    if (includeFACTS) {
      if (is.na(FACTSMechs_path)) {
        stop("Please supply file path to FACTS Mechanism Extract!!")
      }
      
      country_names <- datapackr::dataPackMap %>%
        dplyr::filter(data_pack_name == datapack_name) %>%
        dplyr::pull(country_name) %>%
        unique()
      
      FACTSMechs <- readr::read_csv(file = FACTSMechs_path,
                                    na = "NULL",
                                    col_types = readr::cols(.default = "c")) %>%
        dplyr::select(code = HQID, name = IM,
                      start_date = StartDate, end_date = EndDate,
                      organisation_unit_name = Country,
                      partner = PrimePartner, agency = Agency) %>%
        dplyr::mutate(start_date = lubridate::mdy(start_date),
                      end_date = lubridate::mdy(end_date),
                      name = paste0(code, " - ", name),
                      id = NA_character_,
                      organisation_unit_id = NA_character_) %>%
        dplyr::filter(!stringr::str_detect(agency,"State")
                      & !organisation_unit_name %in% c("Asia Region",
                                        "West-Central Africa Region",
                                        "Western Hemisphere Region")
                      ) %>%
        dplyr::select(code, name, id, start_date, end_date,
                      organisation_unit_name, organisation_unit_id,
                      partner, agency) %>%
        dplyr::filter(organisation_unit_name == datapack_name,
                      !code %in% (newMechList %>%
                                    dplyr::pull(code) %>%
                                    unique()))
    
      newMechList %<>%
        dplyr::bind_rows(FACTSMechs)
    }
    
    openxlsx::writeDataTable(
      wb = wb, sheet = "Old Mechs",
      x = data.frame(oldMechList),
      xy = c(1,1),
      colNames = TRUE, tableName = "old_mechs", tableStyle = "none"
    )
    openxlsx::writeDataTable(
      wb = wb, sheet = "New Mechs",
      x = data.frame(newMechList),
      xy = c(1,1),
      colNames = TRUE, tableName = "new_mechs", tableStyle = "none"
    )
  
  # Write valid indicators ####
    indicators <- datapackr::site_tool_schema %>%
      dplyr::filter(col_type == "Target") %>%
      dplyr::mutate(
        indicator =
          stringr::str_extract(
            indicator_code,"^([^\\.]+\\.[^\\.]+)"
          )
        ) %>%
      dplyr::select(indicator) %>%
      dplyr::arrange(indicator) %>%
      dplyr::distinct() #%>%
      #tibble::add_row(indicator = "(All)",.before = 1)
    
    openxlsx::writeDataTable(
      wb = wb,
      sheet = "Validations",
      x = indicators,
      xy = c(3,1),
      colNames = TRUE,
      tableName = "indicators",
      tableStyle = "none",
      withFilter = TRUE
    )
    
  # Write type options (DSD/TA) ####
    openxlsx::writeDataTable(
      wb,
      sheet = "Validations",
      x = data.frame(type = c("DSD", "TA")),
      xy = c(5,1),
      colNames = TRUE,
      tableName = "dsdta",
      tableStyle = "none",
      withFilter = TRUE
    )
    
  # Add validations to Mechanism Map tab ####
    ## Old Mechanisms
      openxlsx::dataValidation(
        wb, sheet = "Mechanism Map",
        cols = 1, rows = 4:200,
        type = "list",
        value = 'INDIRECT("old_mechs[name]")'
      )
    ## PSNUs
      openxlsx::dataValidation(
        wb, sheet = "Mechanism Map",
        cols = 2, rows = 4:200,
        type = "list",
        value = 'INDIRECT("psnus[PSNU]")'
      )
    ## Indicators
      openxlsx::dataValidation(
        wb, sheet = "Mechanism Map",
        cols = 3, rows = 4:200,
        type = "list",
        value = 'INDIRECT("indicators[indicator]")'
      )
    ## Types
      openxlsx::dataValidation(
        wb, sheet = "Mechanism Map",
        cols = 4, rows = 4:200,
        type = "list",
        value = 'INDIRECT("dsdta[type]")'
      )
    ## New Mechanisms
      openxlsx::dataValidation(
        wb, sheet = "Mechanism Map",
        cols = 6:30, rows = 3,
        type = "list",
        value = 'INDIRECT("new_mechs[name]")'
      )
      
  # Export ####
    if (is.na(output_path)) {
      output_path = getwd()
    }
      
    data_pack_name <- datapackr::dataPackMap %>%
      dplyr::filter(model_uid == datapack_uid) %>%
      dplyr::pull(data_pack_name) %>%
      unique()
    
    exportPackr(wb, output_path, "Mechanism Map", data_pack_name)
}
