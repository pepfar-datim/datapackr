
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
  if (!d$info$cop_year %in% supportedCOPYears()) {
    stop("createAnalytics does not work on tools for that COP Year.")
  }

  #Choose the correct DE/COC map
  map_des_cocs <- getMapDataPack_DATIM_DEs_COCs(cop_year = d$info$cop_year,
                                                datasource = d$info$tool)

  # Append the distributed MER data and subnat data together
  if (d$info$tool == "OPU Data Pack") {

    if (is.null(d$info$psnus)) {
      d$info$psnus <- datapackr::valid_OrgUnits %>%
        dplyr::filter(country_uid %in% d$info$country_uids) %>%
        dplyr::select(ou, country_name, snu1, psnu = name, psnu_uid = uid)
    }
    #OPU datapacks have no prioritizations, so we need to get them from DATIM
    prios <- fetchPrioritizationTable(psnus = d$info$psnus,
                                      cop_year = d$info$cop_year,
                                      d2_session = d2_session)



    d$data$analytics <- d$datim$OPU %>%
      adorn_import_file(cop_year = d$info$cop_year,
                        map_des_cocs = map_des_cocs,
                        psnu_prioritizations = prios,
                        d2_session = d2_session)
    return(d)

  } else if (d$info$tool != "Data Pack") {
    stop("Sorry, we don't recognize that tool type.")
  }

  # For COP21+, get data from import files for better consistency ####
  pzns <- d$datim$prioritizations %>%
    dplyr::select(orgUnit, value)

  d$data$analytics <-
    switch(ifelse(d$info$has_psnuxim, "MER", "UndistributedMER"),
           MER = d$datim$MER,
           UndistributedMER = d$data$UndistributedMER) %>%
    dplyr::bind_rows(d$datim$subnat_impatt) %>%
    adorn_import_file(cop_year = d$info$cop_year,
                      psnu_prioritizations = pzns,
                      map_des_cocs = map_des_cocs,
                      d2_session = d2_session)

  if (d$info$unallocatedIMs || !d$info$has_psnuxim) {
    d$data$analytics %<>%
          dplyr::mutate(dplyr::across(
            c(mechanism_code, mechanism_desc, partner_desc, funding_agency),
            ~ dplyr::case_when(
              is.na(.x) &
                stringr::str_detect(support_type, "DSD|TA") ~ "Unallocated",
              is.na(.x) &
                stringr::str_detect(support_type, "Sub-National|No Support Type") ~ "default",
              TRUE ~ .x
            )
          ))
    }

  d
}
