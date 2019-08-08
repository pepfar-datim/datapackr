#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom stats complete.cases
#' @title packSNUxIM(data)
#'
#' @description Packs SNUxIM data prepared from unPackSNUxIM for import to DATIM.
#'
#' @param data SNUxIM dataframe to pack for DATIM.
#' 
#' @return Dataframe of SNUxIM data ready for DATIM import.
#' 
packSNUxIM <- function(data) {
  
  # Confirm data structure is as expected.
  SNUxIM.schema.names <-
    c("PSNU","psnuid","sheet_name","indicator_code","Age","CoarseAge",
      "Sex","KeyPop","mechanism_code","value")
  
  if (any(names(data) != SNUxIM.schema.names)) {
    error_msg <- "ERROR occurred while preparing SNUxIM data for DATIM. Columns not as expected."
    stop(error_msg)
  }

  SNUxIM <- data %>%
    dplyr::mutate(
      period = datapackr::periodInfo$iso) %>% 
    dplyr::left_join(datapackr::PSNUxIM_to_DATIM %>% #TODO: Build PSNUxIM_to_DATIM from API call.
                       dplyr::filter(dataset == "MER") %>%
                       dplyr::select(-sheet_name, -typeOptions, -dataset),
                     by = c("indicator_code" = "indicatorCode",
                            "Age" = "validAges",
                            "Sex" = "validSexes",
                            "KeyPop" = "validKPs")) %>%
    # Under COP19 requirements, after this join, TX_PVLS N will remain NA for dataelementuid and categoryoptioncombouid
    # Select and rename based on DATIM protocol
    dplyr::select(
      dataElement = dataelementuid,
      period,
      orgUnit = psnuid,
      categoryOptionCombo = categoryoptioncombouid,
      mechanism_code,
      value) %>%
    dplyr::group_by(dataElement, period, orgUnit,categoryOptionCombo,
                    mechanism_code) %>% #TODO: Coordinate with self-service on this name change
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
  # Coerce decimals to integers now
    dplyr::mutate(value = round_trunc(value)) %>%
  # Remove anything which is NA here. Under COP19 guidance, this will include only TX_PVLS.N.Age/Sex/Indication/HIVStatus.20T.Routine
    dplyr::filter(complete.cases(.))
  
  return(SNUxIM)

}