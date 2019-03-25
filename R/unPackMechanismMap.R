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
    col_names <- readxl::read_excel(path = mechMap_path,
                                    sheet = "Mechanism Map",
                                    range = readxl::cell_rows(3),
                                    col_names = FALSE)
    ct <- col_names %>%
      tidyr::gather(key = "col", value = "col_name") %>%
      dplyr::mutate(
        col_type = dplyr::case_when(
          col_name %in% c("Old Mechanism","PSNU","Indicator","Type") ~ "text",
          stringr::str_detect(col_name,"Distribution Check|(\\d)+") ~ "numeric",
          TRUE ~ "guess")
        ) %>%
      dplyr::pull(col_type)
    
    mechMap <- readxl::read_excel(
        path = mechMap_path,
        sheet = "Mechanism Map",
        range = readxl::cell_limits(c(3,1), c(NA, NA)),
        col_types = ct) %>%
      dplyr::select(dplyr::matches("Old Mechanism|PSNU|Indicator|Type|(\\d)+")) %>%
      tidyr::gather(
        key = "newMech",
        value = "weight",
        -`Old Mechanism`, -PSNU, -Indicator, -Type,
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
      dplyr::mutate(Indicator = dplyr::if_else(Indicator == "(ALL)", 
                                               "(ALL).(ALL)", 
                                               Indicator)) %>% 
      tidyr::separate(
        col = Indicator,
        into = c("Technical Area","Numerator / Denominator"),
        sep = "\\.",
        remove = TRUE,
        extra = "merge") %>%
      dplyr::select(psnuid, `Technical Area`, `Numerator / Denominator`,
                    `Support Type` = Type, oldMech, newMech, weight) %>% 
      dplyr::distinct()

# some validation checks on the resulting mechanism map
# 1 - weight should sum to 1 within groups - Critical
# 2 - old mech and new mech shouldn't match if weight = 1
# 3 - psnuid should be a valid uid
# 4 - indicator (technical area and numerator/denominator) should be valid
# 5 - support type should be valid

    critical_issues <- NULL

    grp_weights_ne_one <-  mechMap %>% 
      dplyr::group_by(psnuid, `Technical Area`, 
                      `Numerator / Denominator`, 
                      `Support Type`, oldMech) %>% 
      dplyr::summarise(grp_total = sum(weight)) %>% 
      dplyr::ungroup() %>% dplyr::filter(grp_total != 1) 

    if(NROW(grp_weights_ne_one) > 0){
      critical_issues <- c(critical_issues, 
                           paste("\nErrors in Mechanism to Mechanism map. Some group weights do not sum to 1."))
      }

    newMechs_same_as_oldMechs <- mechMap %>% 
      dplyr::filter(oldMech == newMech, 
                    weight == 1)

    if (NROW(newMechs_same_as_oldMechs) > 0) {
      msg <-
        "Mechanism map contains mappings to and from the SAME mechanism with weight of 1."
      interactive_print(paste(str(newMechs_same_as_oldMechs), msg))
      # d$info$warningMsg <- append(msg, d$info$warningMsg) 
      }

    all_psnuid_are_uid <- grepl("^[A-Za-z][A-Za-z0-9]{10}$", mechMap$psnuid) %>% 
      all()

    if(!all_psnuid_are_uid){
      critical_issues <- c(critical_issues, 
                           paste("\nErrors in Mechanism to Mechanism map. Some psnuids are not valid uids."))
      }

# TODO copied and pasted from mapMechanisms should be pulled into a function    
    indicators <- datapackr::site_tool_schema %>%
      dplyr::filter(col_type == "Target") %>%
      dplyr::mutate(
        indicator = stringr::str_extract(
        indicator_code,"^([^\\.]+\\.[^\\.]+)"
        )
        ) %>%
      dplyr::select(indicator) %>%
      dplyr::arrange(indicator) %>%
      dplyr::distinct()

    unmatched_indicators <- stringr::str_c(mechMap$`Technical Area`, "." , mechMap$`Numerator / Denominator`) %>% 
      dplyr::setdiff(c(indicators$indicator, "(ALL).(ALL)"))

    if(length(unmatched_indicators) > 0){
      critical_issues <- c(critical_issues, 
                           paste("\nErrors in Mechanism to Mechanism map. Some indicators are not valid. ", 
                                 unmatched_indicators))
      }

    unmatched_support_types <- dplyr::setdiff(mechMap$`Support Type`,
                                              c("DSD", "TA", "(BOTH)", "(ALL)"))

    if(length(unmatched_support_types) > 0){
      critical_issues <- c(critical_issues, 
                           paste("\nErrors in Mechanism to Mechanism map. Some support types are not valid. ", 
                                 unmatched_support_types))
      }

    if(!is.null(critical_issues)){
      stop(critical_issues)
      }

    return(mechMap)
    }