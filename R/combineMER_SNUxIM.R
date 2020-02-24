#' @export
#' @title combineMER_SNUxIM(d)
#'
#' @description Takes the output of the \code{\link{unPackSNUxIM}} and
#'     \code{\link{unPackSheets}} functions and delicatety combines these to create
#'     a single dataframe at the PSNU x IM level.
#'
#' @param d Datapackr object
#' 
#' @return d
#' 
combineMER_SNUxIM <- function(d) {
  
  d$data$distributedMER <- d$data$MER %>%
    dplyr::full_join(d$data$SNUxIM,
                     by =c("PSNU", "psnuid", "indicator_code", "Age", "Sex", "KeyPop"))
  
  # TEST where distribution attempted where no target set ####
  d$tests$no_targets <- d$data$distributedMER %>%
    dplyr::filter((is.na(value) | value == 0)
                  & !is.na(distribution)
                  & distribution != 0)
  attr(d$tests$no_targets ,"test_name")<-"Distribution with no targets"
  
  
  if (NROW(d$tests$no_targets) > 0) {
    
    no_targets_inds <- d$tests$no_targets %>%
      dplyr::select(indicator_code) %>%
      dplyr::distinct() %>%
      dplyr::arrange(indicator_code) %>%
      dplyr::select(sheet_name,indicator_code)
    
    warning_msg <-
      paste0(
        "WARNING!: ",
        NROW(no_targets_inds),
        " cases where distribution attempted where no Target set.",
        " NOTE that these will be ignored and won't prevent further processing.",
        " This has affected the following indicators -> \n\t* ",
        paste(unique(no_targets_inads$indicator_code), collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    
  }

  # Now it's okay to drop NAs ####
  d$data$distributedMER %<>%
    tidyr::drop_na(value) %>%
    dplyr::mutate(distributed_value = value * distribution,
                  distributed_value_rounded = round_trunc(distributed_value))
  
  # TEST where attempted distribution sum != target ####
  d$tests$imbalanced_distribution <- d$data$distributedMER %>%
    dplyr::group_by_at(
      dplyr::vars(
        dplyr::everything(),
        -mechanism_code, -support_type,
        -value, -distributed_value, -distributed_value_rounded, -distribution)) %>%
    dplyr::mutate(distributed_value_total = sum(distributed_value),
                  diff = value - distributed_value_total) %>%
    dplyr::ungroup() %>%
    dplyr::filter(round_trunc(value) != round_trunc(distributed_value_total))

  if (NROW(d$tests$imbalancedDistribution) > 0) {
    
    imbalancedDistribution_inds <-  d$tests$imbalanced_distribution%>%
      dplyr::select(indicator_code) %>%
      dplyr::distinct() %>%
      dplyr::arrange(indicator_code) %>%
      dplyr::pull(indicator_code)
   
    warning_msg <-
      paste0(
        "ERROR!: ",
        NROW(d$tests$imbalancedDistribution),
        " cases where distributed total across all mechanisms and Dedupe is",
        " either more or less than PSNU-level Target.",
        " To identify these, go to your PSNUxIM tab and filter the Rollup column ",
        "to find cases where this is not equal to 100%.",
        " NOTE that this may be due to any invalid mechanism names in row 14 of your PSNUxIM tab.",
        " For reference, this has affected the following indicators -> \n\t* ",
        paste(imbalancedDistribution_inds, collapse = "\n\t* "),
      "\n")
  
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
  }
  
  # Flag rounding discrepancies for user ####
  d$tests$PSNUxIM_rounding_diffs <- d$data$distributedMER %>%
    dplyr::group_by_at(
      dplyr::vars(
        dplyr::everything(),
        -mechanism_code, -support_type,
        -value, -distributed_value, -distributed_value_rounded, -distribution)) %>%
    dplyr::mutate(distributed_value_rounded_total = sum(distributed_value_rounded),
                  diff = abs(value-distributed_value_rounded_total)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(diff <= 1,
                  diff > 0) %>%
    dplyr::select(PSNU, indicator_code, Age, Sex, KeyPop, original_value = value,
                  distributed_value_rounded_total, diff) %>%
    dplyr::distinct()
  
  if (NROW(d$tests$PSNUxIM_rounding_diffs) > 0) {
    warning_msg <-
      paste0(
        "WARNING: ",
        NROW(d$tests$PSNUxIM_rounding_diffs),
        " cases where rounding based on PSNUxIM distributions has caused a small",
        " amount of variation from original targets set in other sheets.",
        " A small amount of rounding may be unavoidable given the nature of the",
        " target-setting process. You can review these cases in the FlatPack",
        " provided as an output from this app.\n"
      )
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  # TEST for positives against dedupes ####
  d$tests$invalid_dedupes <- d$data$distributedMER %>%
    dplyr::filter(mechanism_code == "99999" & distribution > 0)
  
  if (NROW(d$tests$invalid_dedupes) > 0) {
    warning_msg <- 
      paste0(
        "WARNING!: ",
        NROW(d$tests$invalid_dedupes),
        " cases where positive numbers are being used for Dedupe allocations.",
        " You can find these by filtering on the Dedupe column in the PSNUxIM tab.")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  # TEST for negatives against non-dedupes ####
  d$tests$negative_distributed_targets <- d$data$distributedMER %>%
    dplyr::filter(mechanism_code != "99999" & distribution < 0)
  
  if (NROW(d$tests$negative_distributed_targets) > 0) {
    warning_msg <- 
      paste0(
        "WARNING!: ",
        NROW(d$tests$negative_distributed_targets),
        " cases where negative numbers are being used for mechanism allocations.",
        " The following mechanisms have been affected. -> \n\t* ",
        paste(unique(d$tests$negative_distributed_targets$mechanism_code), collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  # Prepare for Export ####
  d$data$distributedMER %<>%
    dplyr::select(PSNU, psnuid, sheet_name, indicator_code, Age,
                  Sex, KeyPop, mechanism_code, support_type,
                  value = distributed_value) %>%
    
  # Coerce decimals to integers now
    dplyr::mutate(value = round_trunc(value)) %>%
  
  # Drop zeros and NAs
    dplyr::filter(value != 0) %>%
    tidyr::drop_na(value)
  
  # TEST for invalid DSD TA ####
  d$tests$invalid_DSDTA <- d$data$distributedMER %>%
    dplyr::filter(mechanism_code != "99999" & is.na(support_type))
  
  if (NROW(d$tests$invalid_DSDTA) > 0) {
    warning_msg <- 
      paste0(
        "WARNING!: ",
        NROW(d$tests$invalid_DSDTA),
        " cases where column headers in row 14 of your PSNUxIM tab have prevented",
        " us from determining whether you intended data to be distributed to DSD or TA.",
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  return(d)
  
}
