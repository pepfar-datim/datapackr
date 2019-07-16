#' @export
#' @title rePackSNUxIM(d)
#'
#' @description Takes the output of the \code{\link{unPackSNUxIM}} and
#'     \code{\link{unPackSheets}} functions and delicatety combines these to create
#'     a single dataframe at the PSNU x IM level.
#'
#' @param d Datapackr object
#' 
#' @return d
#' 
rePackPSNUxIM <- function(d) {
  
  d$data$distributedMER <- d$data$MER %>%
    dplyr::mutate(
      CoarseAge = dplyr::case_when(
        stringr::str_detect(indicator_code,"OVC_SERV")
          & Age %in% c("<01","01-04","05-09","10-14","15-17") ~ "<18",
        stringr::str_detect(indicator_code,"OVC_HIVSTAT") ~ NA_character_, #TODO: Fix in Data Pack, not here.
        stringr::str_detect(indicator_code,"Malnutrition|Pediatric") ~ "01-04", #TODO: Fix in Data Pack, not here.
        stringr::str_detect(indicator_code,"HTS_SELF(.)+Unassisted") ~ NA_character_, #TODO: Fix in Data Pack, not here.
        Age %in% c("<01","01-04","05-09","10-14") ~ "<15",
        Age %in% c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+") ~ "15+",
        TRUE ~ Age),
      Sex = dplyr::case_when(
        stringr::str_detect(indicator_code,"OVC_HIVSTAT") ~ NA_character_, #TODO: Fix in Data Pack, not here.
        stringr::str_detect(indicator_code,"KP_MAT") ~ stringr::str_replace(KeyPop," PWID",""), #TODO: Fix in Data Pack, not here.
        TRUE ~ Sex),
      KeyPop = dplyr::case_when(
        stringr::str_detect(indicator_code,"KP_MAT") ~ NA_character_, #TODO: Fix in Data Pack, not here.
        TRUE ~ KeyPop)
    ) %>%
    dplyr::full_join(dplyr::select(d$data$SNUxIM,-PSNU))
  
  # TEST where Data Pack targets not fully distributed.
  undistributed <- d$data$distributedMER %>%
    dplyr::filter(!is.na(value) & is.na(distribution))
  
  if (NROW(undistributed) > 0) {
    d$tests$undistributed <- undistributed
    
    undistributed_inds <- undistributed %>%
      dplyr::select(indicator_code) %>%
      dplyr::distinct() %>%
      dplyr::arrange(indicator_code) %>%
      dplyr::pull(indicator_code)
    
    warning_msg <-
      paste0(
        "ERROR!: ",
        NROW(undistributed),
        " cases where no distribution was attempted for Targets.",
        " To identify these, go to your SNU x IM tab and filter the Rollup column for Pink cells.",
        " This has affected the following indicators -> \n\t* ",
        paste(undistributed_inds, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
    
  }
  
  # TEST where distribution attempted where no target set
  noTargets <- d$data$distributedMER %>%
    dplyr::filter(is.na(value) & !is.na(distribution))
  
  if (NROW(noTargets) > 0) {
    d$tests$noTargets <- noTargets
    
    noTargets_inds <- noTargets %>%
      dplyr::select(indicator_code) %>%
      dplyr::distinct() %>%
      dplyr::arrange(indicator_code) %>%
      dplyr::pull(indicator_code)
    
    warning_msg <-
      paste0(
        "ERROR!: ",
        NROW(noTargets),
        " cases where distribution attempted where no Target set.",
        " To identify these, go to your SNU x IM tab and filter the Rollup column for Pink cells.",
        " This has affected the following indicators -> \n\t* ",
        paste(noTargets_inds, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
    
  }
  
  # TEST where attempted distribution sum != target
  imbalancedDistribution <- d$data$distributedMER %>%
    tidyr::drop_na(value, distribution) %>%
    dplyr::select(-Age, -distribution, -mechanism_code) %>%
    dplyr::group_by_at(dplyr::vars(dplyr::everything(), -value)) %>%
    dplyr::summarize(value = round(sum(value), digits = 5)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at(dplyr::vars(dplyr::everything(), -SNUxIM_value)) %>%
    dplyr::summarize(SNUxIM_value = round(sum(SNUxIM_value), digits = 5)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(value != SNUxIM_value)
  
  if (NROW(imbalancedDistribution) > 0) {
    d$tests$imbalancedDistribution <- imbalancedDistribution
    
    imbalancedDistribution_inds <- imbalancedDistribution %>%
      dplyr::select(indicator_code) %>%
      dplyr::distinct() %>%
      dplyr::arrange(indicator_code) %>%
      dplyr::pull(indicator_code)
    
    warning_msg <-
      paste0(
        "ERROR!: ",
        NROW(imbalancedDistribution),
        " cases where distributed total is either more or less than total Target.",
        " To identify these, go to your SNU x IM tab and filter the Rollup column for Pink cells.",
        " This has affected the following indicators -> \n\t* ",
        paste(imbalancedDistribution_inds, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
    
  }
  
  d$data$distributedMER %<>%
    dplyr::mutate(newValue = value * distribution) %>%
    dplyr::select(PSNU, psnuid, sheet_name, indicator_code, Age, CoarseAge,
                  Sex, KeyPop, mechanism_code, value = newValue) %>%
    dplyr::filter(value != 0) %>%
    tidyr::drop_na(value)
  #TODO: Current approach is to leave decimals until right before writing to Site Tool. Is this justified?
  
  return(d)
  
}
