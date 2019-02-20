
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
  
  # Make sure login creds allow export of data from DATIM for specified OU
  
  
  # Distribute Data Pack data to Site x IM
  
  
  
  # Mark what wasn't distributed
  
  
  
  # Generate full site list
  d$data$site_list <- getSiteList(d)
  
  
  # Generate mech list
  
  
  
  # Build Site Tool frame
  
  
  
  # Populate Site Tool
  
  
  
  # Add data validations
  
  
  
  # Export Site Tool
}
