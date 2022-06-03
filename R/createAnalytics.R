
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
    if (d$info$cop_year %in% c(2021, 2022)) {
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

      if (d$info$unallocatedIMs | !d$info$has_psnuxim) {
        d$data$analytics %<>%
              dplyr::mutate(dplyr::across(
                c(mechanism_code, mechanism_desc, partner_desc, funding_agency),
                ~ dplyr::case_when(
                  is.na(.x) &
                    stringr::str_detect(support_type, "DSD|TA") ~ "Unallocated",
                  is.na(.x) &
                    stringr::str_detect(support_type, "Sub-National") ~ "default",
                  is.na(.x) &
                    stringr::str_detect(support_type, "No Support Type") ~ "default",
                  TRUE ~ .x
                )
              ))
          }

    } else {
      stop("createAnalytics does not work on Data Packs for that COP Year.")
    }
  } else {
    stop("Sorry, we don't recognize that tool type.")
  }

  d
}
