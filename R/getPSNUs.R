#' @export
#' @title Return a data frame of valid organisation units
#' based on the COP year
#'
#' @param cop_year The COP Year
#'
#' @return A data frame of organisation units along with their attributes.
getValidOrgUnits <- function(cop_year = NULL) {

  cop_year <- cop_year %missing% NULL

  if (length(cop_year) != 1L) {

    stop("You must specify a single COP Year!")
  }

  if (is.na(cop_year) || is.null(cop_year))  {

    stop(paste("COP Year was not specified"))
  }

  if (!(cop_year %in% supportedCOPYears())) {
    stop(paste("COP Year", cop_year, "has no valid orgunits."))
  }

  switch(as.character(cop_year),
         "2023" = valid_OrgUnits,
         "2024" = valid_OrgUnits_2024,
         "2025" = valid_OrgUnits_2024 #NEEDS to be updated to 25 when available
         )

}



#' @export
#' @title Pull list of Org Units approved for use in Data Packs from DATIM based
#' on provided country UIDs.
#'
#' @description
#' Queries DATIM to extract tibble of Org Units approved for use in Data Pack
#' target setting.
#'
#' @inheritParams datapackr_params
#' @param include_mil Logical. If \code{TRUE}, will also include _Military nodes
#' related to \code{country_uids}. Default is \code{TRUE}.
#' @param include_DREAMS Logical. If \code{TRUE}, will also include DREAMS
#' organisation units.
#' @param additional_fields Character string of any fields to return from DATIM
#' API other than those returned by default.
#' @param use_cache If \code{TRUE}, will first check to see if a cached export
#' of Data Pack Org Units already exists. If so, will use this cached version to
#' save time.
#'
#' @return Tibble of Data Pack Org Units.
#'
getDataPackOrgUnits <- function(include_mil = TRUE,
                                include_DREAMS = TRUE,
                                additional_fields = NULL,
                                use_cache = TRUE,
                                cop_year = NULL,
                                d2_session = dynGet("d2_default_session",
                                                    inherits = TRUE)) {

  if (use_cache) {

    if (!is.null(additional_fields)) {
      stop("Sorry, can't use additional_fields and use_cache at the same time.")
    }

    orgunits <- getValidOrgUnits(cop_year)

    if (!include_mil) {
      orgunits %<>%
        dplyr::filter(org_type != "Military")
    }

    if (!include_DREAMS) {
      orgunits %<>%
        dplyr::filter(org_type != "DSNU")
    }

    return(orgunits)
  }

  # If not using cache, then pulling from API. Remainder helps do that.

  api_filters <-
    c(paste0("organisationUnitGroups.id:in:[AVy8gJXym2D", # Filter for COP Prioritization SNU
             ifelse(include_mil, ",nwQbMeALRjL", ""), # Add military SNUs if requested
             ifelse(include_DREAMS, ",mRRlkbZolDR", ""), # Add DREAMS SNUs if requested
             "]"))

  fields <-
    paste0(
      "id,name,lastUpdated,ancestors[id,name,organisationUnitGroups[id,name]],organisationUnitGroups[id,name]",
      ifelse(!is.null(additional_fields), paste0(",", additional_fields), ""))

  # Pull Org Units ####
  organisationUnits <-
    datimutils::getMetadata(
      "organisationUnits",
      api_filters,
      fields = fields,
      d2_session = d2_session)

  # orgunits <- datimutils::getSqlView(sql_view_uid = ""), #Replace once DP-786 resolved

  # Extract metadata ####
  orgunits <- organisationUnits %>%
    tibble::as_tibble(.) %>%
    dplyr::rename(uid = id) %>%
    dplyr::mutate(
      level_4_type = purrr::map(ancestors, list("organisationUnitGroups", 4, "id"), .default = NA),
      org_type =
        dplyr::case_when(
          stringr::str_detect(as.character(organisationUnitGroups), "nwQbMeALRjL") ~ "Military",
          stringr::str_detect(as.character(organisationUnitGroups), "cNzfcPWEGSH") ~ "Country",
          stringr::str_detect(as.character(organisationUnitGroups), "AVy8gJXym2D") ~ "PSNU",
          stringr::str_detect(as.character(organisationUnitGroups), "mRRlkbZolDR") ~ "DSNU"),
            # While some PSNUs may also be DSNUs, the above will only categorize
            # as a DSNU when it has not already been tagged as a PSNU, making it
            # easier to distinguish the DSNUs below PSNU level in Eswatini & Rwanda.
            # See DP-768 for more.
      country_uid = dplyr::case_when(
        org_type == "Country" ~ uid,
        stringr::str_detect(as.character(level_4_type), "cNzfcPWEGSH") ~ # i.e., when a country under regional OU...
          purrr::map_chr(ancestors, list("id", 4), .default = NA),
        TRUE ~ purrr::map_chr(ancestors, list("id", 3), .default = NA)),
      DREAMS =
        dplyr::case_when(
          stringr::str_detect(as.character(organisationUnitGroups), "mRRlkbZolDR") ~ "Y"),
      country_name = dplyr::case_when(
        org_type == "Country" ~ name,
        stringr::str_detect(as.character(level_4_type), "cNzfcPWEGSH") ~
          purrr::map_chr(ancestors, list("name", 4), .default = NA),
        TRUE ~ purrr::map_chr(ancestors, list("name", 3), .default = NA)),
      ou_uid = purrr::map_chr(ancestors, list("id", 3), .default = NA),
      ou = purrr::map_chr(ancestors, list("name", 3), .default = NA),
      snu1_uid = purrr::map_chr(ancestors, list("id", 4), .default = NA)
          %|% uid,
      snu1 = purrr::map_chr(ancestors, list("name", 4), .default = NA)
          %|% name) %>%
    dplyr::select(name, uid, org_type,
                  ou, ou_uid, country_name, country_uid, snu1, snu1_uid,
                  tidyselect::everything(), -level_4_type)

  # Sort pretty
  orgunits %<>%
    dplyr::arrange(ou, country_name, snu1, org_type, name)

  return(orgunits)
}


#' @export
#' @title Modify Data Pack Org Unit list to add datapackr IDs.
#'
#' @description
#' Adds Org Unit label used in Data Packs.
#'
#' @param orgunits Data frame of Data Pack org units produced by \code{\link{getDataPackOrgUnits}}.
#' @param cop_year COP Year. For COP years less than 2023,
#' the organisation unit type will be added to tbe DP label
#'
#' @return Data frame of Data Pack Org units with added Data Pack label, \code{dp_label}.
#'
add_dp_label <- function(orgunits, cop_year) {
  this_cop_year <- as.numeric(cop_year)

  country_count <- unique(orgunits$country_uid) %>% length()

    orgunits %>%
      dplyr::mutate(dp_label = paste0(
        dplyr::if_else(
          country_count > 1 & country_uid != uid,
          paste0(country_name, " > "),
          ""
        ),
        name,
        " [",
        uid,
        "]"
      ))
  }
