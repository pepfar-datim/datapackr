
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


  # Append the distributed MER data and subnat data together
  if (d$info$tool %in% c("OPU Data Pack", "PSNUxIM", "PSNUxIM Template")) {

    map_des_cocs <- getMapDataPack_DATIM_DEs_COCs(cop_year = d$info$cop_year,
                                                  datasource = d$info$tool)

    #TODO: Fix the names here as this is not aligned with the orgunit structure now
    if (is.null(d$info$psnus)) {
      d$info$psnus <- getValidOrgUnits(d$info$cop_year) %>%
        dplyr::filter(country_uid %in% d$info$country_uids) %>%
        dplyr::select(ou, country_name, snu1, psnu = name, psnu_uid = uid)
    }


    #OPU datapacks have no prioritizations, so we need to get them from DATIM
    #PSNUxIM tools in COP23 May have incoming prioritizations if they are
    #used with a datapack, but may be missing them if the tool is validated
    #as a standalone tool.

    if (is.null(d$datim$prioritizations)) {
      prios <- fetchPrioritizationTable(psnus = d$info$psnus$psnu_uid,
                                        cop_year = d$info$cop_year,
                                        d2_session = d2_session)
    } else {
      prios <- d$datim$prioritizations
    }

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

  #TODO: We need to clean this logic up.
  if (d$info$has_psnuxim) {
    data <- d$datim$MER %>%
      dplyr::bind_rows(d$datim$subnat_impatt)
    map_des_cocs <- getMapDataPack_DATIM_DEs_COCs(d$info$cop_year, "PSNUxIM")
  } else {
    data <- d$datim$UndistributedMER %>%
      dplyr::bind_rows(d$datim$subnat_impatt)
    map_des_cocs <- getMapDataPack_DATIM_DEs_COCs(d$info$cop_year, "Data Pack")
  }

  d$data$analytics <-
    adorn_import_file(
      psnu_import_file = data,
      cop_year = d$info$cop_year,
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
