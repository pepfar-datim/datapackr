
autoResolveDuplicates <- function(d, keep_dedup ) {

  #We need to now indentify any cases where there was exactly 100% distribution, but there was a dedupe.
  #This is the section for pure duplicates.
  pure_duplicates<-d$data$SNUxIM %>%
    dplyr::filter(mechanism_code != '99999') %>%
    dplyr::filter(distribution != 0) %>%
    dplyr::group_by(PSNU,psnuid,indicator_code,Age,Sex,KeyPop,support_type) %>%
    dplyr::summarize(distribution = sum(distribution),
                     n = dplyr::n()) %>%
    dplyr::filter(n > 1 ) %>%
    dplyr::mutate(distribution_diff = abs(distribution - 1.0))

  over_allocated <- pure_duplicates %>%
    dplyr::filter(distribution > 1.0)

  if (NROW(over_allocated) > 0) {
    warning_msg <-
      paste0(
        "INFO! ",
        NROW(over_allocated),
        " pure duplicates with allocation greater than 100% were identified. These will",
        " need to be deduplicated in DATIM. Ensure all necessary deduplication values are",
        " 100% addressed. Please consult the Data Pack User Guide for more information.",
        "/n"
      )

    d$info$messages <- appendMessage(d$info$messages, warning_msg,"INFO")
  }

  auto_resolve_pure_dupes <- pure_duplicates %>%
    dplyr::filter(distribution_diff < 1e-3 ) %>%
    dplyr::mutate(mechanism_code ='00000',
                  value = 0,
                  sheet_name = NA) %>%
    dplyr::select(names(d$data$distributedMER))

  #DSD_TA Crosswalk dupes which should be autoresolved
  if ( setequal(unique(d$data$SNUxIM$support_type),c("DSD","TA")) ) {
    crosswalk_dupes_ids <- d$data$SNUxIM %>%
      dplyr::filter(mechanism_code != '99999') %>%
      dplyr::filter(distribution != 0) %>%
      dplyr::group_by(PSNU,psnuid,indicator_code,Age,Sex,KeyPop,support_type) %>%
      dplyr::summarize(n = dplyr::n()) %>%
      tidyr::pivot_wider(names_from = support_type,
                         values_from = n) %>%
      tidyr::drop_na(DSD,TA) %>%
      dplyr::filter(TA >=1 & DSD >=1 ) %>%
      dplyr::select(-TA,-DSD)

    crosswalk_dupes <- d$data$SNUxIM %>%
      dplyr::filter(mechanism_code != '99999') %>%
      dplyr::filter(distribution != 0) %>%
      dplyr::inner_join(crosswalk_dupes_ids)

    if ( NROW(crosswalk_dupes) > 0  ) {
      crosswalk_dupes %<>%
        dplyr::group_by(PSNU,psnuid,indicator_code,Age,Sex,KeyPop) %>%
        dplyr::summarise(total_distribution = sum(distribution,na.rm=TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(distribution_diff = abs(total_distribution - 1.0))

      over_allocated <- crosswalk_dupes %>%
        dplyr::filter(distribution_diff > 1e-3)

      if (NROW(over_allocated) > 0) {
        warning_msg <-
          paste0(
            "INFO! ",
            NROW(over_allocated),
            " crosswalk duplicates with allocation greater than 100% were identified. These",
            " will need to be deduplicated in DATIM. Ensure all necessary crosswalk",
            " deduplication values are 100% addressed. Please consult the Data Pack User Guide for more information.",
            "/n"
          )

        d$info$messages <- appendMessage(d$info$messages, warning_msg,"INFO")
      }

      crosswalk_dupes_auto_resolved <- crosswalk_dupes %>%
        dplyr::filter(distribution_diff <= 1e-3 ) %>%
        dplyr::select(PSNU,psnuid,indicator_code,Age,Sex,KeyPop) %>%
        dplyr::mutate(support_type = 'TA',
                      sheet_name = NA,
                      mechanism_code = '00001',
                      value = 0) %>%
        dplyr::select(names(d$data$distributedMER))
    }
} else {
  crosswalk_dupes_auto_resolved<-data.frame(foo=character())
  }

  if ( keep_dedup == TRUE ) {
    d$datim$MER <- d$data$distributedMER
  } else {
    #Filter the pseudo-dedupe mechanism data out
    d$datim$MER <- d$data$distributedMER %>%
      dplyr::filter(mechanism_code != '99999')
  }


  exists_with_rows <- function(x) {

    sym <- deparse(substitute(x))
    env <- parent.frame()
    if (!exists(sym, env)) {
      return(FALSE)
    } else
      if (NROW(x) > 0) {
        return(TRUE)
      } else
      {
        FALSE
      }
  }

  #Bind pure dupes

  if ( exists_with_rows(auto_resolve_pure_dupes) ) {
    d$datim$MER<-dplyr::bind_rows(d$datim$MER,auto_resolve_pure_dupes)
    warning_msg<-paste0("INFO! ", NROW(auto_resolve_pure_dupes), " zero-valued pure deduplication adjustments will be added to your DATIM import.
                  Please consult the DataPack wiki section on deduplication for more information. ")

    d$info$messages <- appendMessage(d$info$messages, warning_msg,"INFO")
  }

  #Bind crosswalk dupes
  if ( exists_with_rows(crosswalk_dupes_auto_resolved)  ) {
    d$datim$MER<-dplyr::bind_rows(d$datim$MER,crosswalk_dupes_auto_resolved)
    warning_msg<-paste0("INFO! ", NROW(crosswalk_dupes_auto_resolved), " zero-valued crosswalk deduplication adjustments will be added to your DATIM import.
                  Please consult the DataPack wiki section on deduplication for more information. ")

    d$info$messages <- appendMessage(d$info$messages, warning_msg,"INFO")
  }

  d
}



#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom stats complete.cases
#' @title exportDistributedDataToDATIM(data)
#'
#' @description Packs distributed MER data prepared from unPackSNUxIM for import to DATIM.
#'
#' @param d Datapackr object
#' @param keep_dedup T/F as to whether to include Dedupe values in export file.
#'
#' @return Modified d object with  a DATIM compatible data frame for import id d$datim$MER
#'
exportDistributedDataToDATIM <- function(d, keep_dedup = FALSE) {

  d<-autoResolveDuplicates(d,keep_dedup)

  # align   map_DataPack_DATIM_DEs_COCs with  d$datim$MER/d$data$distributedMER for KP_MAT

  map_des_cocs_local <- datapackr::getMapDataPack_DATIM_DEs_COCs(d$info$cop_year)

  map_des_cocs_local$valid_sexes.name[map_des_cocs_local$indicator_code == "KP_MAT.N.Sex.T" &
                                                       map_des_cocs_local$valid_kps.name == "Male PWID"] <- "Male"
  map_des_cocs_local$valid_sexes.name[map_des_cocs_local$indicator_code == "KP_MAT.N.Sex.T" &
                                                       map_des_cocs_local$valid_kps.name == "Female PWID"] <- "Female"
  map_des_cocs_local$valid_kps.name[map_des_cocs_local$indicator_code == "KP_MAT.N.Sex.T" &
                                                     map_des_cocs_local$valid_kps.name == "Male PWID"] <- NA_character_
  map_des_cocs_local$valid_kps.name[map_des_cocs_local$indicator_code == "KP_MAT.N.Sex.T" &
                                                     map_des_cocs_local$valid_kps.name == "Female PWID"] <- NA_character_

  # Readjust for PMTCT_EID
  d$datim$MER %<>% dplyr::mutate(
      Age =
        dplyr::case_when(
          indicator_code %in% c("PMTCT_EID.N.Age.T.2mo","PMTCT_EID.N.Age.T.2to12mo")
            ~ NA_character_,
          TRUE ~ Age)
    ) %>%

  # Pull in all dataElements and categoryOptionCombos
    dplyr::left_join(., ( map_des_cocs_local %>%
                            dplyr::rename(Age = valid_ages.name,
                                          Sex = valid_sexes.name,
                                          KeyPop = valid_kps.name) )) %>%

    # Add period
    dplyr::mutate(
      period = paste0(d$info$cop_year,"Oct") ) %>%
    # Under COP19 requirements, after this join, TX_PVLS N will remain NA for dataelementuid and categoryoptioncombouid
    # Select and rename based on DATIM protocol
    dplyr::select(
      dataElement = dataelement,
      period,
      orgUnit = psnuid,
      categoryOptionCombo = categoryoptioncombouid,
      attributeOptionCombo = mechanism_code,
      value) %>%

    # Make sure no duplicates
    dplyr::group_by(dataElement, period, orgUnit,categoryOptionCombo,
                    attributeOptionCombo) %>% #TODO: Coordinate with self-service on this name change
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%

    # Remove anything which is NA here. Under COP19 guidance, this will include only TX_PVLS.N.Age/Sex/Indication/HIVStatus.20T.Routine
    dplyr::filter(complete.cases(.))


  return(d)

}
