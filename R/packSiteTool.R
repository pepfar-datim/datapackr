#' @title Write Site Tool sheet
#' 
#' @description 
#' Writes an individual Site Tool sheet with OU-level summary row, subtotal
#' formulas, and site-level distributed dataset.
#' 
#' @param wb Openxlsx workbook object.
#' @param sheet Name of sheet to write.
#' @param d A datapackr list object.
#' 
write_site_level_sheet <- function(wb, sheet, d) {
  # OU sum row ####
    # sums <- d$data$site$distributed %>%
    #   dplyr::filter(sheet_name == sheet) %>%
    #   dplyr::group_by()
  
  # Order Columns ####
    ## Filter and spread distributed site data
  data <- d$data$site$distributed %>%
    dplyr::filter(sheet_name == sheet) %>%
    tidyr::spread(key = indicatorCode, value = siteValue) %>%
    dplyr::mutate(Inactive = "") %>%
    dplyr::select(Inactive,dplyr::everything())

    ## Get column order from schema
  schema <- datapackr::site_tool_schema %>%
    dplyr::filter(sheet_name == sheet)
  
    ## Remember num of row_header columns
  row_header_cols <- NROW(schema[schema$col_type == "Row Header",])
  
  schema %<>%
    dplyr::select(indicator_code) %>%
    dplyr::mutate(fields = NA) %>%
    ## Transpose to look like Site Tool rows 1:3
    as.data.frame() %>%
    `row.names<-`(.[, 1]) %>%
    dplyr::select(-1) %>%
    t() %>%
    tibble::as_tibble() %>%
    dplyr::slice(rep(1:dplyr::n(), each = NROW(data)))
    
    ## Morph the distributed data into shape
  data <- schema %>%
    datapackr::swapColumns(., data) %>%
    as.data.frame(.)
    
  # Write data ####
  openxlsx::writeDataTable(wb = wb, sheet = sheet, x = data,
                           xy = c(1,5), colNames = TRUE, withFilter = TRUE,
                           stack = TRUE, tableStyle = "none",
                           tableName = tolower(sheet))
  
  # Subtotal row ####
    ## Formula
  data_cols <- names(data)[(row_header_cols + 1):length(data)]
  
  subtotal_fxs <- paste0('=SUBTOTAL(109,',tolower(sheet),'[',data_cols,'])') %>%
    t()
  
  # Inactive column ####
  
  # Conditional formatting ####
  #to code Mil, Natl, Comm, Fac, Inactive, Not Distributed
  
  
  # Validation ####
    ## Site
  
    ## Mechanism
  
    ## Type
  
    ## Age (can this be conditional based on tab?)
  
    ## Sex (can this be conditional based on tab?)
  
    ## KeyPop
  
  
  # Conform column widths ####
  
  
  # Freeze pane ####
  openxlsx::freezePane(wb = wb, sheet = sheet,
                       firstActiveRow = 6,
                       firstActiveCol = (row_header_cols + 1))
      
    
  
  #TODO ####
  # - Write data into each sheet
  # - conditional formatting on cells where data against invalid disaggs
  # - What to do with dedupes?...
  # - Add inactive column to site tool schema (first col)
  # - Add lines in frame for subtotal and Data Pack sum rows
  
  
}

#' @export
#' @title Pack a Site Tool
#' 
#' @description
#' Takes data from the Data Pack, distributes it to site level, then creates a
#' Site Tool from scratch and writes data into it, along with all other data
#' for Site Tool features.
#' 
#' @param d A datapackr list object.
#' 
#' @details
#' Executes the following operations:
#' \enumerate{
#'     \item Grabs Data Pack SNU x IM data
#'     \item Distributes this data to Site x IM level
#'     \item Marks what was not distributed for easy recognition
#'     \item Generates a full site list to allow manual distribution, including:
#'     \itemize{
#'        \item All sites
#'        \item All _Military nodes, including new sub-region _Military nodes
#'        \item All new sites added under new countries (e.g., Brazil, Nepal)
#'     }
#'     \item Generates a full list of mechanisms to allow manual distribution
#'     \item Creates a Site Tool from scratch
#'     \item Populates Site Tool with data, site list, and mech list
#'     \item Adds data validation to Site Tool to allow drop-down selections
#'     \item Exports Site Tool for use by Country Teams
#' }
#'
packSiteTool <- function(d) {
  
  # Make sure login creds allow export of data from DATIM for specified OU ####
  
  
  # Build Site Tool frame ####
    wb <- datapackr::packFrame(datapack_uid = d$info$datapack_uid,
                               type = "Site Tool")
  
  # Add data validation Options ####
    ## DSD, TA options for validation
    openxlsx::writeDataTable(
      wb = wb,
      sheet = "Validations",
      x = data.frame(type = c("DSD", "TA")),
      xy = c(1,1),
      colNames = T,
      tableName = "dsdta"
    )
    
    ## Inactive site tagging options
    openxlsx::writeDataTable(
      wb,
      sheet = "Validations",
      x = data.frame(choices = c("Active","Inactive")),
      xy = c(2,1),
      colNames = T,
      tableName = "inactive_options"
    )
    
  # Write full site list ####
    country_uids <- datapackr::dataPackMap %>%
      dplyr::filter(data_pack_name == d$info$datapack_name) %>%
      dplyr::pull(country_uid)
    
    siteList <- datapackr::getSiteList(country_uids,
                                       include_mil = TRUE)
    
    openxlsx::writeDataTable(
      wb = wb,
      sheet = "Site List",
      x = data.frame(siteID = siteList$site_tool_label,
                     status = "Active"),
      xy = c(1,1),
      colNames = TRUE,
      tableName = "site_list_table"
    )
    
    openxlsx::setColWidths(wb = wb,
                           sheet = "Site List",
                           cols = 1:2,
                           widths = c("auto",16))
    
    openxlsx::dataValidation(wb = wb,
                             sheet = "Site List",
                             cols = 2,
                             rows = 2:(NROW(siteList)+1),
                             type = "list",
                             value = 'INDIRECT("inactive_options[choices]")')
    
    openxlsx::conditionalFormatting(wb = wb,sheet = "Site List",
                                    cols = 1,rows = 2:(NROW(siteList)+1),
                                    rule = "[#Community]",
                                    style = datapackr::styleGuide$siteList$community,
                                    type = "contains")
    openxlsx::conditionalFormatting(wb = wb, sheet = "Site List",
                                    cols = 1,rows = 2:(NROW(siteList)+1),
                                    rule = "[#Facility]",
                                    style = datapackr::styleGuide$siteList$facility,
                                    type = "contains")
    openxlsx::conditionalFormatting(wb = wb,sheet = "Site List",
                                    cols = 1,rows = 2:(NROW(siteList)+1),
                                    rule = "[#National]",
                                    style = datapackr::styleGuide$siteList$national,
                                    type = "contains")
    openxlsx::conditionalFormatting(wb = wb,sheet = "Site List",
                                    cols = 1,rows = 2:(NROW(siteList)+1),
                                    rule = "[#Military]",
                                    style = datapackr::styleGuide$siteList$military,
                                    type = "contains")
    openxlsx::conditionalFormatting(wb = wb,sheet = "Site List",
                                    cols = 1,rows = 2:(NROW(siteList)+1),
                                    rule = '$B2="Inactive"',
                                    style = datapackr::styleGuide$siteList$inactive)
    
  # Write mech list ####
    mechList <- datapackr::getMechList(country_uids,
                                       FY = 2019)
    openxlsx::writeDataTable(
      wb = wb,
      sheet = "Mechs",
      x = data.frame(mechID = mechList$name),
      xy = c(1,1),
      colNames = TRUE,
      tableName = "mech_list"
    )
    
  
  # Grab Data Pack data distributed at Site x IM x DSD/TA ####
    d$data$site$distributed %<>%
    ## Pull in mechanism names
      dplyr::left_join((mechList %>%
                          dplyr::select(code,mechanism = name)),
                       by = c("mechanismCode" = "code")) %>%
    ## Mark what wasn't distributed
      dplyr::mutate(
        site_tool_label = dplyr::case_when(
          is.na(site_tool_label) ~ paste0(PSNU," > NOT YET DISTRIBUTED"),
          TRUE ~ site_tool_label
            ),
        siteValue = dplyr::case_when(is.na(siteValue) ~ value, TRUE ~ siteValue),
        siteValue = datapackr::round_trunc(siteValue),
        mechanism = dplyr::case_when(mechanismCode == "Dedupe" ~ "Dedupe", TRUE ~ mechanism)
        ) %>%
      dplyr::select(sheet_name,Site = site_tool_label,Mechanism = mechanism,
                    Age,Sex,KeyPop,Type = type,indicatorCode,siteValue)
    
  # Populate Site Tool ####
    data_sheets <- names(wb)[which(!stringr::str_detect(names(wb), "Home|Site List|Mechs|Validations"))]
    
    write_all_sheets <- function(x) {write_site_level_sheet(wb = wb, sheet = x, d = d)}
    sapply(data_sheets, write_all_sheets)
        
  # Export Site Tool ####
    output_file_name <- paste0(
      d$keychain$output_path,
      if (is.na(stringr::str_extract(d$keychain$output_path,"/$"))) {"/"} else {},
      "SiteTool_",
      d$info$datapack_name,"_",
      format(Sys.time(), "%Y%m%d%H%M%S"),
      ".xlsx"
    )
    
    openxlsx::saveWorkbook(
      wb = wb,
      file = output_file_name,
      overwrite = TRUE
    )
    print(paste0("Successfully saved output to ", output_file_name))
}
