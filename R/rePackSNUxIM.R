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
    dplyr::full_join(d$data$SNUxIM)
  
  # TEST where distribution attempted where no target set
  noTargets <- d$data$distributedMER %>%
    dplyr::filter((is.na(value) | value == 0)
                  & !is.na(distribution)
                  & distribution != 0)
  
  if (NROW(noTargets) > 0) {
    d$tests$noTargets <- noTargets
    
    noTargets_inds <- noTargets %>%
      dplyr::select(indicator_code) %>%
      dplyr::distinct() %>%
      dplyr::arrange(indicator_code) %>%
      dplyr::pull(indicator_code)
    
    warning_msg <-
      paste0(
        "WARNING!: ",
        NROW(noTargets),
        " cases where distribution attempted where no Target set.",
        "NOTE that these will be ignored and won't prevent further processing.",
        " This has affected the following indicators -> \n\t* ",
        paste(noTargets_inds, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
    
  }

  #Do not round at this point. Only just prior to some output step. 
  d$data$distributedMER %<>%
    tidyr::drop_na(value, distribution) %>%
    dplyr::mutate(distributed_value = round_trunc(value * distribution))
  
  # TEST where attempted distribution sum != target
  imbalancedDistribution <- d$data$distributedMER %>%
    dplyr::group_by_at(
      dplyr::vars(
        dplyr::everything(),
        -mechanism_code, -support_type,
        -value, -distributed_value, -distribution)) %>%
    dplyr::mutate(distributed_value_total = sum(distributed_value)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(value != distributed_value_total)

  if (NROW(imbalancedDistribution) > 0) {
    d$tests$imbalancedDistribution <- imbalancedDistribution
    
    imbalancedDistribution_inds <- imbalancedDistribution %>%
      dplyr::select(indicator_code) %>%
      dplyr::distinct() %>%
      dplyr::arrange(indicator_code) %>%
      dplyr::pull(indicator_code)
   
    warning_msg <-
      paste0(
        "WARNING!: ",
        NROW(imbalancedDistribution),
        " cases where distributed total across all mechanisms and Dedupe is",
        " either more or less than PSNU-level Target.",
        " To identify these, go to your PSNUxIM tab and filter the Rollup column ",
        "to find cases where this is not equal to 100%.",
        " NOTE that this may be due to any invalid mechanism names in row 14 of your PSNUxIM tab.",
        " For reference, this has affected the following indicators -> \n\t* ",
        paste(imbalancedDistribution_inds, collapse = "\n\t* "),
      "\n")
  
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  d$data$distributedMER %<>%
    dplyr::select(PSNU, psnuid, sheet_name, indicator_code, Age,
                  Sex, KeyPop, mechanism_code, support_type,
                  value = distributed_value) %>%
    dplyr::filter(value != 0) %>%
    tidyr::drop_na(value)
  
  # TEST for negatives against non-dedupes
  # TEST for invalid DSD TA
  
  return(d)
  
}
