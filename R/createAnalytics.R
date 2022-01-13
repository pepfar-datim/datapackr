
#' @title getFY22Prioritizations(d)
#'
#' @param  d Datapackr object
#'
#' @return Dataframe of psnu_uid and value
#' @export
#'
getFY22Prioritizations <- function(d) {

  psnu_prioritizations <- d$datim$fy22_prioritizations %>%
    dplyr::select(orgUnit, value)

  psnus <- dplyr::filter(datapackr::valid_PSNUs, psnu_type == "SNU") %>%
    dplyr::filter(country_uid %in% d$info$country_uids) %>%
    dplyr::select(ancestor_uid = psnu_uid, ancestor_name = psnu)

  #Classify any DREAMS districts the same as their PSNU parents
  dreams_prioritizations <- dplyr::filter(valid_PSNUs, DREAMS == "Y") %>%
    dplyr::select(psnu_uid, psnu, ancestors) %>%
    tidyr::unnest("ancestors") %>%
    dplyr::select(-organisationUnitGroups) %>%
    dplyr::group_by(psnu_uid, psnu) %>%
    dplyr::summarise(path = paste(id, sep = "", collapse = "/")) %>%
    dplyr::ungroup() %>%
    fuzzyjoin::regex_inner_join(psnus, by = c("path" = "ancestor_uid")) %>%
    dplyr::inner_join(psnu_prioritizations, by = c("ancestor_uid" = "orgUnit")) %>%
    dplyr::select(orgUnit = psnu_uid, value)

  dplyr::bind_rows(psnu_prioritizations, dreams_prioritizations)

}


#' @export
#' @importFrom magrittr %>% %<>%
#' @title createAnalytics(d)
#'
#' @description Wrapper function for creation of d$data$analytics object
#' which is suitable for export to external analytics sytems.
#'
#' @param d Datapackr object
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return Modified d object with d$data$analytics
#'
#'
createAnalytics <- function(d,
                            d2_session = dynGet("d2_default_session",
                                                inherits = TRUE)) {
  # Append the distributed MER data and subnat data together
  if (d$info$tool == "OPU Data Pack") {
    d$data$analytics <- d$datim$OPU %>%
      adorn_import_file(cop_year = d$info$cop_year,
                        psnu_prioritizations = NULL,
                        d2_session = d2_session)
  } else if (d$info$tool == "Data Pack") {
    if (d$info$cop_year == 2020) {
      d$data$analytics <- dplyr::bind_rows(
        d$data$distributedMER,
        d$data$SUBNAT_IMPATT %>%
          dplyr::mutate(
            mechanism_code = "HllvX50cXC0",
            support_type = "DSD")
        )
    } else if (d$info$cop_year %in% c(2021, 2022)) {
      # For COP21+, get data from import files for better consistency ####
      prioritizations <- getFY22Prioritizations(d)

      if (d$info$has_psnuxim) {
        targets <- d$datim$MER
      } else {
        targets <- d$data$UndistributedMER
      }

      d$data$analytics <-
        dplyr::bind_rows(
          targets,
          d$datim$subnat_impatt) %>%
        adorn_import_file(cop_year = d$info$cop_year,
                          psnu_prioritizations = prioritizations,
                          d2_session = d2_session)
      
      if (d$info$unallocatedIMs) {
        d$data$analytics %<>%
          dplyr::mutate(
            mechanism_code =
              dplyr::case_when(
                is.na(mechanism_code)
                  & !indicator_code %in% c("AGYW_PREV.D.T", "AGYW_PREV.N.T")
                  & support_type != "Sub-National"
                  ~ "Unallocated",
                TRUE ~ mechanism_code)
          )
      }
      
    } else {
      stop("createAnalytics does not work on Data Packs for that COP Year.")
    }
  } else {
    stop("Sorry, we don't recognize that tool type.")
  }

  d
}
