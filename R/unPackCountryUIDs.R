#' @export
#' @title Extract country uids from submitted file
#' 
#' @description 
#' When supplied a submission path, will return the list of country_uids
#' pertaining to the file, as read from the Home tab.
#' 
#' @param submission_path Local path to the file to import.
#' @param tool What type of tool is the submission file? Default is "Data Pack".
#' Other options include "Site Tool", "Mechanism Map", and "Site Filter".
#' 
#' @return Character vector of country_uids.
#' 
unPackCountryUIDs <- function(submission_path = NA,
                              tool = "Data Pack") {
  
  submission_path <- handshakeFile(path = submission_path,
                                   tool = tool)
  
  country_uids <-
    readxl::read_excel(
      path = submission_path,
      sheet = "Home",
      range = countryUIDs_homeCell()) %>%
    names() %>%
    stringr::str_remove_all("\\s") %>%
    stringr::str_split(",") %>%
    unlist()
  
  # If Regional UID provided, prompt for list of individual Country UIDs instead
  invalid_uids <-
    stringr::str_extract_all(
      country_uids, 
      paste0("Asia_Regional_Data_Pack|iD2i0aynOGm|t25400wXrNB",
             "|Caribbean_Data_Pack|nBo9Y4yZubB",
             "|Central_America_Data_Pack|vSu0nPMbq7b|IJOkeEIdw9i",
             "|Western_Africa_Data_Pack"
      )
    ) %>%
    unlist()
  
  if (length(invalid_uids) > 0) {
    msg <-
      paste0(
        "Cell ",countryUIDs_homeCell()," in the Home tab of your ",
        tool,
        " contains the following Regional OU uids: \n\n  * ",
        paste(invalid_uids, collapse = "\n  * "),
        "\n\nThis approach is no longer supported.",
        " Please return to your ",
        tool,
        " and enter a list of DATIM country-level uids separated by commas in cell",
        countryUIDs_homeCell(),
        " of the Home tab.")
    
    stop(msg)
  }
  
  return(country_uids)
  
}
