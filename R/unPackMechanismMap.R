#' @export
#' @importFrom tools file_ext
#' @title Unpack Mechanism Maps for use in site distributions.
#' 
#' @description 
#' Reads in an Excel file containing mechanism mapping from old to new
#' mechanisms for use in adjusting site distribution algorithms.
#' 
#' @param mechMap_path A local file path directing to a submitted Mechanism Map.
#' Default is NA. If left NA, will prompt user to select file via window.
#' 
#' @return A dataframe containing mechanism mapping data ready for use as right
#' object in left-join with site-level data from DATIM.
#' 
unPackMechanismMap <- function(mechMap_path = NA) {
  # Grab the file ####
    can_read_import_file <- function(mechMap_path) {
      if (is.na(mechMap_path)) {return(FALSE)}
      file.access(mechMap_path,4) == 0
    }
    
    if (!can_read_import_file(mechMap_path)) {
      print("Please choose a submission file:")
      mechMap_path <- file.choose()
    }
    
    msg <- "Checking the file exists..."
    print(msg)
    
    if (!can_read_import_file(mechMap_path)) {
      stop("Mechanism Map could not be read!")
    }
    
    if (tools::file_ext(mechMap_path) != "xlsx" ) {
      stop("File must be an XLSX file!")
    }
  
  # Read in file ####
    col_num <- length(
        readxl::read_excel(
          path = mechMap_path,
          sheet = "Mechanism Map",
          range = readxl::cell_rows(3))
      )
    
    ct <- c(rep("text",4),rep("numeric",col_num-4))
    
    mechMap <- readxl::read_excel(
        path = mechMap_path,
        sheet = "Mechanism Map",
        range = readxl::cell_limits(c(3,1), c(NA, NA)),
        col_types = ct) %>%
      tidyr::gather(
        key = "newMech",
        value = "percent",
        -`Old Mechanism`, -PSNU, -Indicator, -Type, -`Distribution Check`,
        na.rm = TRUE) %>%
      dplyr::mutate(
        newMech = stringr::str_extract(newMech, "^[^ ]+"),
        psnuid =
          stringr::str_extract(
            PSNU,
            "(?<=\\()([A-Za-z][A-Za-z0-9]{10})(?=\\)$)"
          ),
        oldMech = stringr::str_extract(`Old Mechanism`, "^[^ ]+")
      ) %>%
      tidyr::separate(
        col = Indicator,
        into = c("Technical Area","Numerator / Denominator"),
        sep = "\\.",
        remove = TRUE,
        extra = "merge") %>%
      dplyr::select(psnuid, `Technical Area`, `Numerator / Denominator`,
                    `Support Type` = Type, oldMech, newMech, percent)
    
  return(mechMap)
  
}