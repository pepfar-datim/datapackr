
#' @title Derive non-Data Pack targets from others in the Data Pack/Site Tool
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
  derived <- data %>%
    dplyr::filter(
      stringr::str_detect(
        indicatorCode, 
        paste0(
          "VMMC_CIRC\\.N\\.Age/Sex/HIVStatus\\.20T"
          # If we derive the SUBNAT/IMPATT ones, paste0 here
        )
      )
    )
  
  if(NROW(derived) > 0) {
    derived %<>%
      dplyr::mutate(
        indicatorCode =
          dplyr::case_when(
            stringr::str_detect(
              indicatorCode,
              "VMMC_CIRC\\.N\\.Age/Sex/HIVStatus\\.20T")
            ~ "VMMC_CIRC.N.Age/Sex.20T",
            # If we derive SUBNAT/IMPATT ones, add conditions here
            TRUE ~ indicatorCode
          )
      ) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
    
    combined <- data %>%
      dplyr::bind_rows(derived)
    
    return(combined)
    
  } else {return(data)}
  
}
