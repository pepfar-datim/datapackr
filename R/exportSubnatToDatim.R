#' @export
#' @title exportSubnatToDATIM(d)
#'
#' @description Takes the outputs of the \code{\link{unPackSheets}} function and
#'     adds  a dataframe containing SUBNAT and IMPATT data,
#'     \code{d$data$SUBNAT_IMPATT} into a standard DATIM import file.
#'
#' @param d Datapackr object
#'
#' @return Datapackr d object
#'
exportSubnatToDATIM <- function(d) {

  datim_map <- datapackr::getMapDataPack_DATIM_DEs_COCs(d$info$cop_year)

  SUBNAT_IMPATT <- d$data$SUBNAT_IMPATT %>%
    dplyr::left_join(datim_map,
                     by = c("indicator_code" = "indicator_code",
                            "Age" = "valid_ages.name",
                            "Sex" = "valid_sexes.name",
                            "KeyPop" = "valid_kps.name")) %>%
    dplyr::mutate(
      attributeOptionCombo = datapackr::default_catOptCombo()
    ) %>%
    dplyr::mutate(
      value =
        dplyr::case_when(
          value_type == "integer" ~ datapackr::round_trunc(value),
          TRUE ~ value))

  # Form into DATIM import file ####
  SUBNAT_IMPATT %<>%
    dplyr::select(
      dataElement = dataelementuid,
      period,
      orgUnit = psnuid,
      categoryOptionCombo = categoryoptioncombouid,
      attributeOptionCombo,
      value) %>%
  # Aggregate across 50+ age bands ####
    dplyr::group_by(dplyr::across(c(-value))) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()

 # TEST: Duplicate Rows; Error; Continue ####
  duplicated_rows <- SUBNAT_IMPATT %>%
    dplyr::group_by(dataElement, orgUnit, categoryOptionCombo, attributeOptionCombo, period) %>%
    dplyr::tally() %>%
    dplyr::filter(n > 1)

  d$tests$duplicated_subnat_impatt <- duplicated_rows
  attr(d$tests$duplicated_subnat_impatt, "test_name") <- "Duplicated SUBNAT/IMPATT data"

  # TEST: Whether any NAs in any columns
  if (NROW(duplicated_rows) > 0) {
    warning_msg <-
      paste0(
        "ERROR! In tab SUBNATT/IMPATT. Duplicate rows. Contact support.")
    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    d$info$has_error <- TRUE
  }

  # TEST: Blank Rows; Error;
  blank_rows <- SUBNAT_IMPATT %>%
    dplyr::filter_all(dplyr::any_vars(is.na(.)))

  # TEST: Whether any NAs in any columns
  if (NROW(blank_rows) > 0) {
    d$tests$blank_rows_datim_subnat_impatt <- blank_rows
    attr(d$tests$blank_rows_datim_subnat_impatt, "test_name") <- "SUBNAT/IMPATT data with blanks"
    warning_msg <-
      paste0(
        "ERROR! In tab SUBNATT/IMPATT. DATIM Export has blank rows. Contact support.")
    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    d$info$has_error <- TRUE
  }

  # TEST: Negative values; Error;
  if (any(SUBNAT_IMPATT$value < 0)) {
    warning_msg <- "ERROR occurred. Negative values present in SUBNAT/IMPATT data."
    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    d$info$has_error <- TRUE
  }

  # Drop any rows with any NA to prevent breakage in iHub ####
  SUBNAT_IMPATT %<>%
    tidyr::drop_na()

  d$datim$subnat_impatt <- SUBNAT_IMPATT %>%
    # PATCH: Drop TX_CURR_SUBNAT.R for now ####
    dplyr::filter(
      dataElement != "MktYDp33kd6"
    )

  d$datim$subnat_fy20 <-  SUBNAT_IMPATT %>%
    dplyr::filter(
      period == "2020Q3",
      dataElement %in%
        (datim_map %>%
           dplyr::filter(period_dataset == "FY20 SUBNAT Results" & !is.na(indicator_code)) %>%
           dplyr::pull(dataelementuid)
        )
    )

  d$datim$subnat_fy21 <-  SUBNAT_IMPATT %>%
    dplyr::filter(
      period == "2020Oct",
      dataElement %in%
        (datim_map %>%
           dplyr::filter(period_dataset == "FY21 SUBNAT Targets" & !is.na(indicator_code)) %>%
           dplyr::pull(dataelementuid)
        )
    )

  d$datim$subnat_fy22 <- SUBNAT_IMPATT %>%
    dplyr::filter(
      period == "2021Oct",
      dataElement %in%
        (datim_map %>%
           dplyr::filter(period_dataset == "FY22 SUBNAT Targets" & !is.na(indicator_code)) %>%
           dplyr::pull(dataelementuid)
        )
    )

  d$datim$impatt_fy22 <- SUBNAT_IMPATT %>%
    dplyr::filter(
      period == "2021Oct",
      dataElement %in%
        (datim_map %>%
           dplyr::filter(period_dataset == "FY22 IMPATT" & !is.na(indicator_code)) %>%
           dplyr::pull(dataelementuid)
      )
    )
  
  #Add the following top level targets by aggregation
  appendTotals <- function(data) {
    totals_map <- tibble::tribble(
      ~from, ~to, ~categoryoptioncomboid,
      'SSun4i7nHlV','BLUT96oHPxO','QXtyoEEa0I7',  # VMMC_CIRC_SUBNAT (N, SUBNAT, Sex) TARGET
      'ZayJeEa6pCa','ywCrpZgX1P9', 'QXtyoEEa0I7',   # VMMC_TOTALCIRC_SUBNAT (N, SUBNAT, Sex) TARGET
      'ctGo7s0K63z', 'CShdIv7wNUB','HllvX50cXC0', # #KP_MAT_SUBNAT (N, SUBNAT) 
      'xghQXueYJxu', 'qjBMdjQFy26', 'HllvX50cXC0', #T X_CURR_SUBNAT (N, SUBNAT) TARGET
      'zoKiMGRucOY', 'qXWRBZTRrUm', 'LVcCRCAVjwj', #VL_SUPPRESSION_SUBNAT (N, SUBNAT, HIVStatus) TARGET
      'nF19GOjcnoD', 'mcDTjVP9B0e', 'LVcCRCAVjwj' #DIAGNOSED_SUBNAT (N, SUBNAT, Age/Sex/HIVStatus) TARGET
    )
    
    createAggregatedTotals <-
      function (data, from, to, categoryoptioncomboid) {
        data %>% dplyr::filter(`dataElement` == from) %>%
          dplyr::mutate(`dataElement` = to) %>%
          dplyr::mutate(categoryOptionCombo = categoryoptioncomboid) %>%
          dplyr::group_by(across(c(-value))) %>%
          dplyr::summarise(value = sum(value), .groups = "drop")
      }
    
    total_results <- data[0, ]
    for (i in seq_len(NROW(totals_map))) {
      these_totals <-
        createAggregatedTotals(data,
                               totals_map$from[i],
                               totals_map$to[i],
                               totals_map$categoryoptioncomboid[i])
      total_results <- dplyr::bind_rows(total_results, these_totals)
    }
    
    dplyr::bind_rows(data, total_results)
  }
  
  d$datim$impatt_fy22 <- appendTotals(d$datim$impatt_fy22)
  
  d$datim$fy22_prioritizations <- SUBNAT_IMPATT %>%
    dplyr::filter(#period == "2021Oct",
      dataElement %in%
        datim_map$dataelementuid[which(datim_map$indicator_code == "IMPATT.PRIORITY_SNU.T")])
  
  return(d)
}
