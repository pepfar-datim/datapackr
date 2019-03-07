#' @export
#' @title Color code sites by type
#' 
#' @description 
#' Adds conditional formatting layers over site names to color code then by type:
#' \itemize{
#'   \item{Green}{Community sites.}
#'   \item{Blue}{Facility sites.}
#'   \item{Purple}{National-level.}
#'   \item{Khaki}{Military node.}
#' }
#' 
#' @param wb An Openxlsx Workbook object.
#' @param sheet A name or index of a worksheet.
#' @param cols Columns to apply conditional formatting to.
#' @param rows Rows to apply conditional formatting to.
#'
colorCodeSites <- function(wb, sheet, cols, rows) {
  sg <- datapackr::styleGuide$siteList
  
  openxlsx::conditionalFormatting(wb = wb,sheet = sheet,
                                  cols = cols,rows = rows,
                                  rule = "[#Community]",
                                  style = sg$community,
                                  type = "contains")
  openxlsx::conditionalFormatting(wb = wb,sheet = sheet,
                                  cols = cols,rows = rows,
                                  rule = "[#Facility]",
                                  style = sg$facility,
                                  type = "contains")
  openxlsx::conditionalFormatting(wb = wb,sheet = sheet,
                                  cols = cols,rows = rows,
                                  rule = "[#National]",
                                  style = sg$national,
                                  type = "contains")
  openxlsx::conditionalFormatting(wb = wb,sheet = sheet,
                                  cols = cols,rows = rows,
                                  rule = "[#Military]",
                                  style = sg$military,
                                  type = "contains")  
}


#' @export
#' @importFrom magrittr %>% %<>%
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
  print(sheet)
  
# Order Columns ####
  ## Filter and spread distributed site data
  data <- d$data$site$distributed %>%
    dplyr::filter(sheet_name == sheet) %>%
    tidyr::spread(key = indicatorCode, value = siteValue) %>%
    dplyr::mutate(Status = "") %>%
    dplyr::select(Status,dplyr::everything())

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
  
  data_cols <- names(data)[(row_header_cols + 1):length(data)]
    
# Write data ####
  ## Test for blank datasets
  if (NROW(data) != 0) {
    openxlsx::writeDataTable(wb = wb, sheet = sheet, x = data,
                             xy = c(1,5), colNames = TRUE, withFilter = TRUE,
                             stack = TRUE, tableStyle = "none",
                             tableName = tolower(sheet))  
  }
  
# Subtotal row ####
  ##Compile Formula
  subtotal_fxs <- paste0('=SUBTOTAL(109,',tolower(sheet),'[',data_cols,'])')
    
  ## Write Formula
  datapackr::writeFxColumnwise(wb, sheet, subtotal_fxs, xy = c(row_header_cols+1,4))
    
  ## Add red conditional formatting for discrepancies
  subtotal_colStart_letter <- openxlsx::int2col(row_header_cols+1)
  subtotal_cond_format_fx <- paste0(
      '!=',subtotal_colStart_letter,'3')
  openxlsx::conditionalFormatting(
    wb, sheet,
    cols = (row_header_cols+1:length(data_cols)), rows = 4,
    rule = subtotal_cond_format_fx)

# OU sum row ####
  sums <- d$data$distributedMER %>%
    dplyr::filter(sheet_name == sheet) %>%
    dplyr::select(indicatorCode, value) %>%
    dplyr::group_by(indicatorCode) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = indicatorCode, value = value, drop = FALSE)
    
  sums <- schema %>%
    dplyr::select(data_cols) %>%
    dplyr::slice(1) %>%
    datapackr::swapColumns(., sums)
  
  sums[is.na(sums)] <- 0
  
  openxlsx::writeData(wb, sheet, sums,
                      xy = c(row_header_cols + 1,3),
                      colNames = FALSE)
  
  ## Format both Site and Data Pack subtotal rows as numeric
  num <- openxlsx::createStyle(numFmt = "#,##0.00")
  openxlsx::addStyle(wb, sheet, style = num,
                     rows = 3:4, cols = (row_header_cols+1:length(data_cols)),
                     gridExpand = TRUE)
    
# Inactive column ####
  max_row_buffer <- 0
  formula_cell_numbers <- seq(1, NROW(data) + max_row_buffer) + 5
  
  inactiveFormula <- paste0(
    'IF(B',
    formula_cell_numbers,
    '<>"",IFERROR(INDEX(site_list_table[status],MATCH(B',
    formula_cell_numbers,
    ',site_list_table[siteID],0)),"NOT A SITE"),"")'
  )
  openxlsx::writeFormula(wb, sheet, inactiveFormula, xy = c(1, 6))
  
# Conditional formatting ####
  datapackr::colorCodeSites(
    wb = wb, sheet = sheet, cols = 2, rows = 6:(NROW(data) + max_row_buffer + 5))
  openxlsx::conditionalFormatting(
    wb = wb, sheet = sheet,
    cols = 1:length(data), rows = 6:(NROW(data) + max_row_buffer + 5),
    rule = 'OR($A6=="Inactive",$A6=="NOT A SITE")')
  
# Validation ####
  ## Site
  openxlsx::dataValidation(wb, sheet,
    cols = 2, rows = 6:(NROW(data) + max_row_buffer + 5),
    type = "list",
    value = 'site_list')
  
  ## Mechanism
  openxlsx::dataValidation(wb, sheet,
    cols = 3, rows = 6:(NROW(data) + max_row_buffer + 5),
    type = "list",
    value = 'mech_list')
  
  ## Type
  openxlsx::dataValidation(wb, sheet,
    cols = 4, rows = 6:(NROW(data) + max_row_buffer + 5),
    type = "list",
    value = 'dsdta')
  
  ## Age
  if ("Age" %in% names(data)) {
    age_range <-
      dplyr::case_when(
        sheet %in% c("TB_STAT_ART","TX") ~ "ages_long",
        sheet == "TB_TX_PREV" ~ "ages_coarse",
        sheet == "OVC" ~ "ages_ovc",
        sheet == "HTS" ~ "ages_no01",
        sheet == "CXCA" ~ "ages_cxca",
        sheet %in% c("PMTCT_STAT_ART","VMMC","PP") ~ "ages_noUnder10",
        sheet == "PrEP" ~ "ages_over15"
      )
    openxlsx::dataValidation(wb, sheet,
      cols = which(names(data)=="Age"),
      rows = 6:(NROW(data) + max_row_buffer + 5),
      type = "list",
      value = age_range)
  }
  
  ## Sex (can this be conditional based on tab?)
  if ("Sex" %in% names(data)) {
      sexes <- dplyr::case_when(
        sheet %in% c("TB_STAT_ART","TX","HTS","TB_TX_PREV","OVC","PP","PrEP") ~ "MF",
        sheet %in% c("PMTCT_STAT_ART","CXCA") ~ "Female",
        sheet == "VMMC" ~ "Male"
      )
    
      openxlsx::dataValidation(wb, sheet,
        cols = which(names(data)=="Sex"),
        rows = 6:(NROW(data) + max_row_buffer + 5),
        type = "list",
        value = sexes)
  }
  
  ## KeyPop
  if ("KeyPop" %in% names(data)) {
      openxlsx::dataValidation(wb, sheet,
        cols = which(names(data)=="KeyPop"),
        rows = 6:(NROW(data) + max_row_buffer + 5),
        type = "list",
        value = "kp")
  }
  
# Conform column/row sizes ####
  openxlsx::setColWidths(wb, sheet,
    cols = 1:length(data),
    widths =
      c("auto", 50, 20, "auto",
        rep("auto",row_header_cols-4),
        rep(12,length(data_cols))
        )
    )
  
  openxlsx::setRowHeights(wb, sheet,
    rows = 2,
    heights = 120)
  
# Freeze pane ####
  openxlsx::freezePane(wb = wb, sheet = sheet,
                       firstActiveRow = 6,
                       firstActiveCol = (row_header_cols + 1))
      
    
  
#TODO ####
  # - conditional formatting on cells where data against invalid disaggs
  # - What to do with dedupes?...
  # - Redo column labels to list full string of element name?
  # - Resolve issues with OVC, HTS_SELF
  
  return(wb)
}

#' @export
#' @importFrom magrittr %>% %<>%
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
  print("Building Site Tool frame...")  
  wb <- datapackr::packFrame(datapack_uid = d$info$datapack_uid,
                               type = "Site Tool")
  
# Write site list (TODO: SPEED THIS UP) ####
  print("Writing Site List...")
    country_uids <- datapackr::dataPackMap %>%
      dplyr::filter(data_pack_name == d$info$datapack_name) %>%
      dplyr::pull(country_uid)
    
    siteList <- datapackr::getSiteList(country_uids,
                                       include_mil = TRUE) %>%
      #dplyr::select(country_name,psnu,siteID = site_tool_label,site_type) %>%
      dplyr::select(siteID = site_tool_label) %>%
      dplyr::mutate(status = "Active") %>%
      dplyr::arrange(siteID)
    
    openxlsx::writeDataTable(
      wb = wb,
      sheet = "Site List",
      x = siteList,
      xy = c(1,1),
      colNames = TRUE,
      tableName = "site_list_table",
      tableStyle = "none",
      withFilter = TRUE
    )
    
    openxlsx::createNamedRegion(wb, sheet = "Site List",
                                cols = 1, rows = 2:(NROW(siteList) + 1),
                                name = "site_list")
    
    openxlsx::setColWidths(
      wb = wb, sheet = "Site List", cols = 1:2,
      #widths = c(rep("auto",4),16))
      widths = c("auto",16))
    
    openxlsx::dataValidation(
      wb = wb, sheet = "Site List", cols = 2, rows = 2:(NROW(siteList)+1),
      type = "list",
      value = 'inactive_options')
    
    datapackr::colorCodeSites(
      wb = wb, sheet = "Site List", cols = 1, rows = 2:(NROW(siteList)+1)
    )
    openxlsx::conditionalFormatting(
      wb = wb,sheet = "Site List", cols = 1,rows = 2:(NROW(siteList)+1),
      rule = '$E2="Inactive"', style = datapackr::styleGuide$siteList$inactive)
    
# Write mech list ####
    print("Writing Mechanism List")
    mechList <- datapackr::getMechList(country_uids,
                                       FY = 2019)
    openxlsx::writeDataTable(
      wb = wb,
      sheet = "Mechs",
      x = data.frame(mechID = mechList$name),
      xy = c(1,1),
      colNames = TRUE,
      tableName = "mech_list_table",
      tableStyle = "none"
    )
    
    openxlsx::createNamedRegion(wb, sheet = "Mechs",
                                cols = 1, rows = 2:(NROW(mechList) + 1),
                                name = "mech_list")
  
# Prep Site data ####
    print("Preparing Site-level data...")
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
    print("Writing site-level data into sheets...")
    data_sheets <- names(wb)[which(!stringr::str_detect(names(wb), "Home|Site List|Mechs|Validations"))]
    
    for (i in 1:length(data_sheets)) {
      wb <- datapackr::write_site_level_sheet(wb = wb,
                                              sheet = data_sheets[i],
                                              d = d)
    }
        
# Export Site Tool ####
    print("Exporting...")
    datapackr::exportPackr(wb,
                d$keychain$output_path,
                type = "Site Tool",
                d$info$datapack_name)
}
