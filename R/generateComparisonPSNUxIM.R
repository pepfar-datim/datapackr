
#' Title
#'
#' @description Prepare a set of DATIM import files by comparing the 
#' DataPack to DATIM
#' @param d 
#' @param include_no_prio 
#' @param d2_session 
#'
#' @return
#' @export
#'
generateUpdateFiles <- function(d, include_no_prio = TRUE,
                               d2_session = dynGet("d2_default_session",
                                                   inherits = TRUE)) {
  
  dp_data <- d$data$analytics %>% 
    dplyr::select(dataElement = dataelement_id,
                  period = fiscal_year,
                  orgUnit = psnu_uid,
                  categoryOptionCombo = categoryoptioncombo_id,
                  attributeOptionCombo = mechanism_code,
                  datapack_value = target_value) %>% 
    dplyr::mutate(period = paste0(period,"Oct")) #TODO: This feels like a hack.
  
  dedupes <- dplyr::filter(dp_data,
                           attributeOptionCombo %in%
                             c("00000", "00001")) %>%
    dplyr::rename(value = datapack_value)
  
  #If we have not already prepared the memo data, do so now
  if (is.null(d$memo$datim$analytics)) {
    d <-prepareMemoData(d,include_no_prio = include_no_prio,d2_session = d2_session)
  }
  
  datim_data <- d$memo$datim$analytics %>% 
    dplyr::select(dataElement = dataelement_id,
                  period = fiscal_year,
                  orgUnit = psnu_uid,
                  categoryOptionCombo = categoryoptioncombo_id,
                  attributeOptionCombo = mechanism_code,
                  datim_value = target_value) %>% 
    dplyr::mutate(period = paste0(period,"Oct"))
  
  #Ignore AGYW_PREV data if this is an OPU
  if (d$info$tool == "OPU Data Pack") {
    datim_data %<>% dplyr::filter(!indicator_code == "AGYW_PREV") 
  }
  
  compare_data <- dplyr::full_join(dp_data,datim_data)
  
  data_differences <- compare_data %>%
    dplyr::filter(datapack_value != datim_value |
                    is.na(datapack_value) |
                    is.na(datim_value)) %>%
    dplyr::filter(!(attributeOptionCombo %in%
                      c("00000", "00001")))
  
  # cases in which datim has a value but datapack does not
  deletes <- dplyr::filter(data_differences, is.na(datapack_value)) %>%
    dplyr::select(-datapack_value) %>%
    dplyr::rename(value = datim_value) %>% 
    dplyr::mutate(value = as.character(value))
  
  # cases in which datapack has a new or updated value
  updates <- dplyr::filter(data_differences, !is.na(datapack_value)) %>%
    dplyr::select(-datim_value) %>%
    dplyr::rename(value = datapack_value) %>% 
    dplyr::mutate(value = as.character(value))

  #Save these in the object for use in the import script  
  d$datim$import <- list(
    updates = updates,
    deletes = deletes,
    dedupes = dedupes
  )
  
  d
  
}

#' Title
#'
#' @param d 
#'
#' @return d with d$memo$comparison_psnuxim object
#' @export
#'

generatePSNUxIMComparison <- function(d) {
  
  datapack_data <- d$data$analytics %>%
    dplyr::rename(datapack_value  = target_value)
  
  #TODO: Should we deal with this in create analytics?
  if (d$info$tool == "OPU Data Pack") {
    datapack_data<- updateExistingPrioritization(d$memo$datim$prios, datapack_data)
  }

  datim_data <- d$memo$datim$analytics %>%
    dplyr::rename(datim_value  = target_value)
  
  compare_data <- dplyr::full_join(datapack_data, datim_data) %>%
    dplyr::filter(!(mechanism_code %in%
                      c("00000", "00001"))) %>% 
    dplyr::filter(
      !dplyr::near(datapack_value, datim_value, tol = .Machine$double.eps ^ 0.00001) |
        is.na(datapack_value) | is.na(datim_value)
    ) %>%
  dplyr::mutate(
    difference = dplyr::case_when(
      is.na(datapack_value) ~ -datim_value,
      is.na(datim_value) ~ datapack_value,
      TRUE ~ round(datapack_value - datim_value, 5)
    )
  ) %>%
    dplyr::mutate(
      effect = dplyr::case_when(
        is.na(difference) & is.na(datapack_value) ~ "Delete",
        is.na(difference) &
          is.na(datim_value) ~ "Create", !is.na(difference) &
          difference != 0 ~ "Update",
        difference == 0 ~ "No Change"
      )
    )
  
  d$memo$comparison_psnuxim <- compare_data
  
  d
  
}
