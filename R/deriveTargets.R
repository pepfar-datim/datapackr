#' @title Derive targets from others in the Data Pack/Site Tool
#' 
#' @description
#' Takes Data Pack or Site Tool data and derives other targets not explicitly
#' set during COP.
#' 
#' @param data Dataframe with either Data Pack or Site Tool data.
#' @param type Type of data, either \code{Data Pack}, or \code{Site Tool}.
#' 
#' @return Dataframe with added, derived targets.
#' 
deriveTargets <- function(data, type) {
  if (type == "Site Tool") {
    derive_from <- data %>%
      dplyr::filter(
        stringr::str_detect(
          indicator_code, 
          paste0("VMMC_CIRC\\.N\\.Age/Sex/HIVStatus\\.20T")
        )
      )
  } else if (type == "Data Pack") {
    derive_from <- NULL
    
    # If we derive the SUBNAT/IMPATT ones, filter here from data
  }
  
  if(NROW(derive_from) == 0) {
    return(data)
  }
  
  derived <- derive_from %>%
    dplyr::mutate(
      indicator_code =
        dplyr::case_when(
          stringr::str_detect(
            indicator_code,
            "VMMC_CIRC\\.N\\.Age/Sex/HIVStatus\\.20T")
          ~ "VMMC_CIRC.N.Age/Sex.20T",
          # If we derive SUBNAT/IMPATT ones, add conditions here
          TRUE ~ indicator_code
        )
    ) %>%
    dplyr::group_by_at(dplyr::vars(-value)) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()
    
  combined <- data %>%
    dplyr::bind_rows(derived)
    
  return(combined)
  
}
