#' @importFrom magrittr %>% %<>%
#' @title packForDATIM(data)
#' 
#' @description 
#' Flexible function that allows packaging of a variety of datapackr outputs as
#' DATIM import files.
#' 
#' @param d Data packr object.
#' @param type Type of dataset to prep for DATIM. Choose from \code{PSNUxIM},
#' \code{SUBNAT_IMPATT}, or \code{Site}.
#' 
#' @return Data frame ready for DATIM import
#' 
packForDATIM <- function(d, type = NA) {
  
  if (is.na(type)) {
    stop("Please specify data type in parameters: 'PSNUxIM', 'SUBNAT_IMPATT', or 'Site'")
  }
  
  if (type == "SUBNAT_IMPATT") {
    
    d$datim$SUBNAT_IMPATT <- packSUBNAT_IMPATT(d$data$SUBNAT_IMPATT)
    
  }
  
  if (type == "PSNUxIM") {
    
    d$datim$PSNUxIM <- d$data$distributedMER %>%
      dplyr::mutate(
        period = datapackr::periodInfo$iso,
        mechanismCode = stringr::str_replace(mechanism_code,"Dedupe","00000")) %>% 
      dplyr::left_join(datapackr::PSNUxIM_to_DATIM %>%
                         dplyr::filter(dataset == "MER") %>%
                         dplyr::select(-sheet_name, -typeOptions, -dataset),
                       by = c("indicator_code" = "indicator_code",
                              "Age" = "validAges",
                              "Sex" = "validSexes",
                              "KeyPop" = "validKPs")) %>%
      dplyr::select(
        dataElement = dataelementuid,
        period,
        orgUnit = psnuid,
        categoryOptionCombo = categoryoptioncombouid,
        mechanismCode,
        value) %>%
      dplyr::group_by(dataElement,
                      period,
                      orgUnit,
                      categoryOptionCombo,
                      mechanismCode) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup() %>%
      #Remove anything which is NA here, 
      #like TX_PVLS.N.Age/Sex/Indication/HIVStatus.20T.Routine
      dplyr::filter(complete.cases(.))
    
  }
  
  if (type == "Site") {
    
    d$datim$decimal_values <- d$data$targets %>% 
      dplyr::filter(value %% 1 != 0)
    
    d$datim$negative_values <- d$data$targets %>% 
      dplyr::filter(mech_code != "00000"
                    & value < 0 )
    
    importFile <- d$data$targets %>%
      dplyr::select(site_uid,mech_code,indicator_code,Type,Age,Sex,KeyPop,value) %>%
      dplyr::filter(
        !is.na(suppressWarnings(as.numeric(value)))) %>%
      dplyr::mutate(
        period = datapackr::periodInfo$iso,
        value = round_trunc(as.numeric(value))) %>%
      dplyr::left_join(datapackr::SiteToDATIM %>%
                         dplyr::filter(dataset == "MER") %>%
                         dplyr::select(-sheet_name, -dataset, -tech_area, -num_den),
                       by = c("indicator_code" = "indicator_code",
                              "Type" = "type_options",
                              "Age" = "valid_ages",
                              "Sex" = "valid_sexes",
                              "KeyPop" = "valid_kps")) %>%
      tidyr::drop_na(dataelementuid) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        dataElement = dataelementuid,
        period,
        orgUnit = site_uid,
        categoryOptionCombo = categoryoptioncombouid,
        attributeOptionCombo = mech_code,
        value) 
    
    if(any(is.na(importFile)) ) {
      
      msg <- paste0("ERROR! Empty values found in DATIM export. These will
                     be filtered.")
      d$info$warningMsg <- append(msg,d$info$warningMsg)
      d$info$has_error <- TRUE
    }
    
    d$datim$site_data <- importFile %>% 
      dplyr::filter(purrr::reduce(purrr::map(., is.na), `+`) == 0 )
    
  }
  
  return(d)
}
