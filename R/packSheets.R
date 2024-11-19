#' @export
#' @title Loop through and populate normal Data Pack sheets
#'
#' @description
#' Loops through all normally structured sheets in a submitted Data Pack
#' and writes data.
#'
#' @param wb datapackr list object.
#' @param country_uids Character vector of Country UIDs from DATIM.
#' @param ou_level Level in DATIM hierarchy to pull orgUnits from. Choose from:
#' "Prioritization", "Community", "Facility", or the numbers 4 through 7.
#' @param org_units Allows for specification of custom list of orgUnits to include
#' in Data Pack sheets.
#' @param model_data Dataset to use as input for packing Data Pack. If left NULL,
#' will produce a Data Pack with orgUnits and disagg specifications, but no data.
#' @param schema Defaults to standard Data Pack schema, but allows for provision
#' of custom schema if needed.
#' @param sheets Sheets to pack. Defaults to all those available in \code{wb},
#' minus the first few front-matter/summary tabs.
#' @param cop_year COP year for dating as well as selection of
#' templates.
#'
#' @return wb with all sheets written except SNU x IM
#'
packDataPackSheets <- function(wb,
                               country_uids,
                               ou_level = "Prioritization",
                               org_units = NULL,
                               model_data = NULL,
                               schema = pick_schema(), # Load in current COP year schema from package
                               sheets = NULL,
                               cop_year = getCurrentCOPYear()) {

  # Resolve parameter issues. ####
  if (is.null(model_data)) {
    stop("Must provide model_data. Leaving this blank is not an option at this time.")
  }

  # Get org_units to write into Data Pack based on provided parameters. ####
  if (is.null(org_units)) {
    if (ou_level == "Prioritization") {


      if (cop_year > 2024) {

      # extract historic regions
      historic_regional_psnus <-
        getValidOrgUnits(cop_year) %>%
        dplyr::filter(!is.na(HISTORIC_PSNU)) %>%
        dplyr::select(name, country_uid)

      # if this country historically had its country as psnu include country
      if (country_uids %in% historic_regional_psnus$country_uid) {

        org_units <- getValidOrgUnits(cop_year) %>%
          dplyr::filter(country_uid %in% country_uids) %>%
          add_dp_label(orgunits = ., cop_year = cop_year) %>%
          dplyr::arrange(dp_label) %>%
          ## Remove DSNUs
          dplyr::filter(!org_type %in% c("DSNU")) %>%
          dplyr::select(PSNU = dp_label, psnu_uid = uid, snu1)

      } else {
      # otherwise business as usual

        org_units <- getValidOrgUnits(cop_year) %>%
          dplyr::filter(country_uid %in% country_uids) %>%
          add_dp_label(orgunits = ., cop_year = cop_year) %>%
          dplyr::arrange(dp_label) %>%
          ## Remove DSNUs and country
          dplyr::filter(!org_type %in% c("DSNU", "Country")) %>%
          dplyr::select(PSNU = dp_label, psnu_uid = uid, snu1)

      }
      } else {
        org_units <- getValidOrgUnits(cop_year) %>%
          dplyr::filter(country_uid %in% country_uids) %>%
          add_dp_label(orgunits = ., cop_year = cop_year) %>%
          dplyr::arrange(dp_label) %>%
          ## Remove DSNUs and country
          dplyr::filter(!org_type %in% c("DSNU")) %>%
          dplyr::select(PSNU = dp_label, psnu_uid = uid, snu1)

      }

    } else if (ou_level %in% c(4:7, "Facility", "Community")) {
      stop("Sorry! I'm learning how to pack a Data Pack at a non-Prioritization
           level, but I'm not quite there yet.")

    } else {
      stop("Hmmm... The ou_level you've provided doesn't look like what I'm used
           to. Please choose from: 'Prioritization', 'Community', 'Facility', 4,
           5, 6, or 7.")
    }
  }

  # Prepare data ####
  if (!all(country_uids %in% names(model_data))) {#Checks if all country_uids are in model data.
    missing <- country_uids[!country_uids %in% names(model_data)]
    stop(
      paste0(
        "Model data file does not have data for the following country_uids: \r\n\t- ",
        paste(missing, collapse = "\r\n\t- ")
      )
    )
  }

  data <- model_data[country_uids] %>%
    dplyr::bind_rows() %>%
    tidyr::drop_na(value) %>%
    dplyr::select(-period) # Drops period column

  # Get sheets to loop if not provided as parameter. ####
  if (is.null(sheets)) {
    wb_sheets <- names(wb)
    schema_sheets <- schema %>%
      dplyr::filter(data_structure == "normal"
                    & !(sheet_name %in% c("SNU x IM", "PSNUxIM"))
                    & sheet_name %in% names(wb)) %>%
      dplyr::pull(sheet_name) %>% # Extracts the column sheet_name
      unique()
    skip_pack_tabs <- skip_tabs(tool = "Data Pack", cop_year = cop_year)$pack

    sheets <- wb_sheets[wb_sheets %in% schema_sheets]
    sheets <- sheets[!sheets %in% skip_pack_tabs]

    if (length(sheets) == 0) {
      stop("This template file does not appear to be normal.")
    }
  }

  # Loop through sheets ####
  interactive_print("Writing Sheets...")

  for (sheet in sheets) {
    interactive_print(sheet)
    sheet_codes <- schema %>%
      dplyr::filter(sheet_name == sheet
                    & col_type %in% c("past", "calculation")) %>%
      dplyr::pull(indicator_code) # Extracts the column indicator_code

    ## If no model data needed for a sheet, forward a NULL dataset to prevent errors
    if (length(sheet_codes) != 0) {
      sheet_data <- data %>%
        dplyr::filter(indicator_code %in% sheet_codes)
    } else {
      sheet_data <- NULL
    }


    # cop year condition will determine packing procedure
    if (cop_year > 2024) {

      # non prioritization sheets should not have prioritization
      if (!sheet %in% c("Prioritization", "AGYW")) {
        temp_org_units_sheet <- getValidOrgUnits(cop_year) %>%
          dplyr::filter(country_uid %in% country_uids) %>%
          add_dp_label(orgunits = ., cop_year = cop_year) %>%
          dplyr::arrange(dp_label) %>%
          # dplyr::filter(!org_type %in% c("DSNU", "PSNU")) %>%
          dplyr::filter(!is.na(TSNU) | org_type == "Military") %>%
          dplyr::select(PSNU = dp_label, psnu_uid = uid, snu1)
        if (NROW(temp_org_units_sheet) == 0) {
          next
        }
      } else if (sheet == "AGYW") {
        temp_org_units_sheet <- getValidOrgUnits(cop_year) %>%
          dplyr::filter(country_uid %in% country_uids) %>%
          add_dp_label(orgunits = ., cop_year = cop_year) %>%
          dplyr::arrange(dp_label) %>%
          dplyr::filter(!is.na(DREAMS)) %>%
          dplyr::select(PSNU = dp_label, psnu_uid = uid, snu1)
        if (NROW(temp_org_units_sheet) == 0) {
          next
        }
      } else {
        temp_org_units_sheet <- org_units
      }

    # else treat 2024 and below
    } else {

      if (sheet == "AGYW") {
        temp_org_units_sheet <- getValidOrgUnits(cop_year) %>% # Load in valid_PSNUs list from package
          dplyr::filter(country_uid %in% country_uids) %>%
          add_dp_label(orgunits = ., cop_year = cop_year) %>%
          dplyr::arrange(dp_label) %>% # Order rows based on dp_psnu col values
          dplyr::filter(!is.na(DREAMS)) %>%
          dplyr::select(PSNU = dp_label, psnu_uid = uid, snu1)# Only keep these columns

        if (NROW(temp_org_units_sheet) == 0) {
          next
        }
      } else {
        temp_org_units_sheet <- org_units
      }


    }


    wb <- packDataPackSheet(wb = wb,
                            sheet = sheet,
                            org_units = temp_org_units_sheet,
                            schema = schema,
                            sheet_data = sheet_data,
                            cop_year = cop_year)
  }


  return(wb)
}
