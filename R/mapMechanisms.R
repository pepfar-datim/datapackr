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
    wb <- datapackr::writeHomeTab(wb = wb,
                                  data_pack_uid = datapack_uid,
                                  type = "Mechanism Map")
    
  # Add other tabs ####
    sheet_names <- c("Old Mechs", "New Mechs", "Validations", "Mechanism Map")
    visible <- c(TRUE, TRUE, FALSE, TRUE)
    for (i in 1:length(sheet_names)) {
      openxlsx::addWorksheet(wb,
                             sheetName = sheet_names[i],
                             visible = visible[i])  
    }
    
  # Write title into Mechanism Map sheet ####
    openxlsx::writeData(wb = wb,
                        sheet = "Mechanism Map",
                        x = "Mechanism Map",
                        xy = c(1,1),
                        colNames = FALSE)
    
  # Write other headers into Mech Map sheet ####
    openxlsx::writeData(wb = wb,
                        sheet = "Mechanism Map",
                        x = c(
                          A = "Old Mechanism",
                          B = "PSNU",
                          C = "Indicator",
                          D = "Type",
                          E = "Distribution Check"),
                        xy = c(1,3),
                        colNames = FALSE)
    
  # Add dist check to Mech Map ####
    checkFx <- paste0(
      '=IF(A', 4:200, '<>"",SUM(A', 4:200, ':AD', 4:200, '),"")')
    
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
    
  # Freeze Panes ####
    openxlsx::freezePane(wb = wb, sheet = "Mechanism Map",
                         firstActiveRow = 4, firstActiveCol = 6)
    
  return(wb)

}


#' @export
#' @importFrom magrittr %>% %<>%
#' 
#' @title Create Mechanism Mapping tool
#' 
#' @description 
#' Produces an Excel tool to allow PEPFAR Country Teams to map shifting
#' Implementing Mechanism service patterns to allow automated distribution of
#' site-level data between distinct mechanisms.
#' 
#' @param datapack_uid A unique ID specifying the PEPFAR Operating Unit 
#' the Site Tool belongs to.
#' @param FY Numeric value of COP Fiscal Year. (For COP19/FY20 targets, enter
#' \code{2019}.)
#' @param output_path A local path directing to the folder where you would like
#' outputs to be saved. If not supplied, will output to working directory.
#' 
#' @return Excel workbook containing needed features to allow various mechanism
#' mapping scenarios, which can be fed back into the Data Pack site distribution
#' process.
#' 
packMechMap <- function(datapack_uid, FY, output_path = NA) {

  # Log into DATIM ####
  #    If user has not already logged in using DATIM credentials, request
  #    credentials via console and getPass.
    #datapackr::loginToDATIM(getOption("secrets"))

  # Frame Workbook ####
    wb <- frameMechMap(datapack_uid)
  
  # Write PSNU list ####
    PSNUs <- datapackr::getPSNUs(datapack_uid) %>%
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
    oldMechList <- datapackr::getMechList(country_uids,
                                          FY = (FY-1))
    newMechList <- datapackr::getMechList(country_uids,
                                       FY = FY)
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
      dplyr::filter(col_type == "FY20 Target") %>%
      dplyr::mutate(
        indicator =
          stringr::str_extract(
            indicator_code,"^([^\\.]+\\.[^\\.]+)"
          )
        ) %>%
      dplyr::select(indicator) %>%
      dplyr::arrange(indicator) %>%
      dplyr::distinct() %>%
      tibble::add_row(indicator = "(All)",.before = 1)
    
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
    openxlsx::writeData(
      wb,
      sheet = "Validations",
      x = data.frame(type = c("DSD", "TA")),
      xy = c(5,1),
      colNames = F,
      name = "dsdta"
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
    
    datapackr::exportPackr(wb, output_path, "Mechanism Map", data_pack_name)
}
