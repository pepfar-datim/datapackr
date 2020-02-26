#' @export
#' @importFrom magrittr %>% %<>%
#' @title createAnalytics(d)
#'
#' @description Wrapper function for creation of d$data$analtyics object 
#' which is suitable for export to external analytics sytems. 
#'
#' @param d
#' 
#' @return Modified d object with d$data$analtyics
#' 
#' 
createAnalytics <- function(d) {
  #Append the distributed MER data and subnat data together
  d$data$analytics <- dplyr::bind_rows(
    d$data$distributedMER,
    dplyr::mutate(
      d$data$SUBNAT_IMPATT,
      mechanism_code = "HllvX50cXC0",
      support_type = "DSD"
    )
  )
  #Adorn with analaytical dimensions
  d <- adornPSNUs(d)
  
  d$data$analytics %<>%  dplyr::left_join(
    .,
    (
      datapackr::map_DataPack_DATIM_DEs_COCs %>%
        dplyr::rename(
          Age = valid_ages.name,
          Sex = valid_sexes.name,
          KeyPop = valid_kps.name
        )
    ),
    by = c("Age", "Sex", "KeyPop", "indicator_code", "support_type")
  )
  
  d
}