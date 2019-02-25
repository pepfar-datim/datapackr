
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
  
  # Write full site list ####
    country_uids <- datapackr::dataPackMap %>%
      dplyr::filter(data_pack_name == d$info$datapack_name) %>%
      dplyr::pull(country_uid)
    
    siteList <- datapackr::getSiteList(country_uids,
                                       include_mil = TRUE)
    
  # Write mech list ####
    mechList <- datapackr::getMechList(country_uids,
                                       FY = 2020)
    openxlsx::writeDataTable(
      wb = wb,
      sheet = "Mechs",
      x - data.frame()
    )
    
  # Populate Site Tool ####
    
  
  
  
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
