
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
  
  
  # Grab Data Pack data distributed at Site x IM x DSD/TA ####
  
  
  
  # Mark what wasn't distributed ####
  
  
  # Build Site Tool frame ####
    wb <- datapackr::packFrame(datapack_uid = d$info$datapack_uid,
                               type = "Site Tool")
  
  # Add data validations ####
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
    
  
  # Populate Site Tool ####
    #TODO
      # 1) Write data into each sheet
      # 2) Conditional formatting on each site column
      # 3) validation on each DSD/TA column
      # 4) validation on each site column
      # 5) validation on each mech column
      # 6) validation on each age column (can this be conditional based on tab?)
      # 7) validation on each sex column (can this be conditional based on tab?)
      # 8) validation on each KP column
      # 9) 
        
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
