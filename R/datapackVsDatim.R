# internal beutify function to avoid repeated code used in the main function
# just handles some formatting/ decoding of UIDs
.compare_beautify <-  function(data,
                               cop_year,
                               d2_session = dynGet("d2_default_session",
                                                                    inherits = TRUE)) {
    data$data_element <-
      datimvalidation::remapDEs(data$dataElement,
                                mode_in = "id",
                                mode_out = "shortName",
                                d2session = d2_session)

    data$disagg <-
      datimvalidation::remapCategoryOptionCombos(data$categoryOptionCombo,
                                                 mode_in = "id",
                                                 mode_out = "name",
                                                 d2session = d2_session)

    psnus <-
      getValidOrgUnits(cop_year) %>% dplyr::select(psnu = name, psnu_uid = uid)

    # calculate diff between data pack and datim handling NAs like a 0
    # round diff to 5 decimal places so we don't get differences due to floating point error
    # add column summarizing the difference

    data %>%
      dplyr::left_join(psnus, by = c("orgUnit" = "psnu_uid")) %>%
      dplyr::mutate(
        difference = dplyr::case_when(
          is.na(datapack_value) ~ -datim_value,
          is.na(datim_value) ~ datapack_value,
          TRUE ~ round(as.numeric(datapack_value) - datim_value, 5)
        )
      ) %>%
      dplyr::mutate(
        effect = dplyr::case_when(
          is.na(datapack_value) ~ "Delete",
            is.na(datim_value) ~ "Create",
           abs(difference) < 1e-5 ~ "Update",
          abs(difference) >= 1e-5 ~ "No Change"
        )
      ) %>%
     dplyr::select(tidyselect::any_of(c(
        "psnu",
        "data_element",
        "disagg",
        "attributeOptionCombo",
        "datapack_value",
        "datim_value",
        "difference",
        "effect"
      )))
}



#' @export
#' @title compareData_DatapackVsDatim
#'
#' @description Compares the data in a parsed data pack that would be destined for DATIM with target data in in DATIM.
#' @inheritParams datapackr_params
#' @param datim_data A data frame resulting from datimutils::getDataValueSets. If null, the data will be fetched
#' from DATIM.
#' @return  list object of diff result $psnu_x_im_wo_dedup, $psnu_w_dedup,
#' $updates (import to bring DATIM up to date with datapack), $deletes
#' (import to bring DATIM up to date with datapack)

compareData_DatapackVsDatim <-
  function(d,
           d2_session = dynGet("d2_default_session",
                               inherits = TRUE),
           datim_data = NULL,
           datastreams = c("mer_targets", "subnat_targets", "impatt")) {


# start main processing
# start off with dedupes included

    if (!(d$info$cop_year %in% supportedCOPYears())) {
      stop("Attempting to use compareData_DatapackVsDatim for unsupported COP year")
    }


    included_data_elements <- getMapDataPack_DATIM_DEs_COCs(d$info$cop_year) %>%
      dplyr::select(dataelementuid, dataset) %>%
      dplyr::distinct()

    # Do not consider AGYW_PREV if this is a OPU Data Pack aka PSNUxIM
    if (d$info$tool == "OPU Data Pack") {
      included_data_elements <- included_data_elements %>%
        dplyr::filter(dataset != "dreams")
    }

    included_data_elements <- included_data_elements %>%
      dplyr::mutate(dataset = dplyr::case_when(dataset == "impatt" ~ "subnat_targets",
                                               dataset == "mer" ~ "mer_targets",
                                               dataset == "subnat" ~ "subnat_targets",
                                               dataset == "dreams" ~ "mer_targets",
                                               TRUE ~ dataset)) %>%
      dplyr::filter(dataset %in% datastreams)

    datapack_data <- createDATIMExport(d) %>%
      dplyr::filter(dataElement %in% included_data_elements$dataelementuid)

    #Need to make value a numeric
    datapack_data$value <- as.numeric(datapack_data$value)

# recoding to account for code change in DATIM for the default COC
# if all other code is updated to use uids instead of codes this can be removed
    datapack_data$categoryOptionCombo[datapack_data$categoryOptionCombo ==
                                      "HllvX50cXC0"] <- "default"
    datapack_data$attributeOptionCombo[datapack_data$attributeOptionCombo ==
                                       "HllvX50cXC0"] <- "default"

    # ensure datapack_data has the expected columns
    if (!setequal(
      names(datapack_data),
      c(
        "dataElement",
        "period",
        "orgUnit",
        "categoryOptionCombo",
        "attributeOptionCombo",
        "value"
      )
    )) {
      stop("The column names of your data aren't as expected by compareData_DatapackVsDatim.")
    }

    # extract dedupes from import file to handle separately
    dedupes <-  dplyr::filter(datapack_data,
                             attributeOptionCombo %in%
                               c("00000", "00001"))

    # Thu Sep 28 21:16:26 2023 ------------------------------
    # We could just drop dedupes from data_pack data here?

    # rename columns to fit standards
    datapack_data <- datapack_data %>%
      dplyr::rename(
        datapack_value = value) %>%
      dplyr::filter(datapack_value != 0) #I don't understand why this is needed

# Sum over IM including dedupe
    datapack_data_psnu <- dplyr::group_by(datapack_data,
                                                  dataElement,
                                                  orgUnit,
                                                  categoryOptionCombo) %>%
      dplyr::summarise(datapack_value = sum(datapack_value, na.rm = TRUE), .groups = "drop")

    datapack_data_psnu_x_im <- datapack_data

# Get data from DATIM using data value sets
if (d$info$cop_year == 2022) {

  if (is.null(datim_data)) {

    datim_data <- dplyr::bind_rows(#NOTE ONLY 2022 Data
      getCOPDataFromDATIM(country_uids = d$info$country_uids,
                          cop_year = d$info$cop_year,
                          datastreams = datastreams,
                          d2_session = d2_session),
      getCOPDataFromDATIM(country_uids = d$info$country_uids,
                          cop_year = d$info$cop_year - 1,
                          datastreams = c("subnat_targets"),
                          d2_session = d2_session))

    if (!is.null(datim_data))  {
      # We should just do this and be done with it 9/18/23
      # I see no tangible difference between 22 and 23 data
      datim_data %<>%
        dplyr::filter(value != "") %>%
        dplyr::rename(datim_value = value)

    #Ignore SUBNATT/IMPATT if we are dealing with a standalone OPU
    if (is.null(d$data$SUBNAT_IMPATT)) {

      mer_des <- datapackr::getMapDataPack_DATIM_DEs_COCs(d$info$cop_year) %>%
        dplyr::filter(dataset == "mer") %>%
        dplyr::pull(dataelementuid) %>%
        unique()

      datim_data %<>%
        dplyr::filter(dataElement %in% mer_des)
    }

    }

  }

} else if (d$info$cop_year == 2023) {
#Based on this Idk if the 0's are getting cleaned up.
if (is.null(datim_data)) {
  datim_data <-
    getCOPDataFromDATIM(country_uids = d$info$country_uids,
                        cop_year = d$info$cop_year,
                        datastreams = datastreams,
                        d2_session = d2_session)
  }


  if (!is.null(datim_data)) {
    datim_data %<>%
      dplyr::filter(value != "") %>%
      #MIGHT NEED TO FILTER 0 here?
      dplyr::rename(datim_value = value)

    #Ignore SUBNATT/IMPATT if we are dealing with a standalone OPU
    if (is.null(d$data$SUBNAT_IMPATT)) {

      mer_des <- datapackr::getMapDataPack_DATIM_DEs_COCs(d$info$cop_year) %>%
        dplyr::filter(dataset == "mer") %>%
        dplyr::pull(dataelementuid) %>%
        unique()

      datim_data %<>%
        dplyr::filter(dataElement %in% mer_des)
    }
  }

}

  #There might not be any data in DAITM
if (is.null(datim_data)) {
    datim_data <- datapack_data_psnu_x_im %>%
      dplyr::mutate(datim_value = NA_real_) %>%
      dplyr::select(-datapack_value)
  }
    # Sum over IM including dedup
    datim_data_psnu <-
      dplyr::group_by(datim_data,
                      dataElement,
                      orgUnit,
                      categoryOptionCombo) %>%
      dplyr::summarise(datim_value = sum(datim_value), .groups = "drop")

    # get rid of dedups in the data dissagregated by IM
    datim_data_psnu_x_im <- datim_data

    # join the data pack data and the datim data
    data_psnu <- dplyr::full_join(datim_data_psnu,
                                  datapack_data_psnu)

    data_psnu_x_im <-
      dplyr::full_join(datim_data_psnu_x_im,
                       datapack_data_psnu_x_im)



# Find the cases with different values. These should be imported into DATIM
    data_different_value <-
      dplyr::filter(
        data_psnu_x_im,
         !dplyr::near(datim_value, datapack_value, 1e-5) | is.na(datim_value)
      ) %>%
      dplyr::select(
        dataElement,
        period,
        orgUnit,
        categoryOptionCombo,
        attributeOptionCombo,
        datapack_value
      )

    # data in datim but not in the data pack
    data_datim_only <-
      dplyr::filter(data_psnu_x_im,
                    is.na(datapack_value)) %>%
      dplyr::select(
        dataElement,
        period,
        orgUnit,
        categoryOptionCombo,
        attributeOptionCombo,
        datim_value
      )

    data_psnu_x_im %<>% .compare_beautify(cop_year = d$info$cop_year,
                                          d2_session = d2_session)

    data_psnu %<>% .compare_beautify(cop_year = d$info$cop_year,
                                     d2_session = d2_session) %>% dplyr::select(-effect)

    #Try to FIX the 0's not being deleted renames datim_value into value
    data_datim_only <- data_datim_only %>%
      dplyr::rename(
        value = datim_value)

    #Could do this or an anti join. I simply don't want dedupes here?
    # data_datim_only %>%
    #   #Filter any non-dedupe zeros
    #   dplyr::filter(!(value == "0" & grepl("^0000[01]", attributeOptionCombo)))

    #OR
    #TEST to see if deletes == dedupes. They do
    # data_datim_only <- data_datim_only %>%
    #   dplyr::anti_join(dedupes,by=c("dataElement","period","orgUnit",
    #                                         "categoryOptionCombo",
    #                                         "attributeOptionCombo", "value"))


    list(
      psnu_x_im = data_psnu_x_im,
      psnu = data_psnu,
      updates = data_different_value,
      deletes = data_datim_only, # data in datim but not in the data pack
      dedupes = dedupes
    )
  }



#' #' @export
#' #' @title compareData_OpuDatapackVsDatim
#' #'
#' #' @description Legacy function maintained for backwards compatibility. If
#' #' processing an Datapack (either Datapack or a standalone PSNU tab)
#' #' as an OPU, only MER targets should be considered for processing.
#' #' @inheritParams datapackr_params
#' #' @param datim_data A data frame resulting from datimutils::getDataValueSets. If null, the data will be fetched
#' #' from DATIM.
#' #' @return  list object of diff result $psnu_x_im_wo_dedup, $psnu_w_dedup,
#' #' $updates (import to bring DATIM up to date with datapack), $deletes
#' #' (import to bring DATIM up to date with datapack)
#' compareData_OpuDatapackVsDatim <-
#'   function(d,
#'            d2_session = dynGet("d2_default_session",
#'                                inherits = TRUE),
#'            datim_data = NULL) {
#'     warning("This function is deprecated. Please use compareData_DatapackVsDatim instead. ")
#'     compareData_DatapackVsDatim(
#'       d,
#'       d2_session = d2_session,
#'       datim_data = datim_data,
#'       datastreams = "mer_targets"
#'     )
#'   }



#' @export
#' @title compareData_OpuDatapackVsDatim
#'
#' @description Compares the data in a parsed data pack that would be destined for DATIM with target data in in DATIM.
#' @param d list object - parsed data pack object
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return  list object of diff result $psnu_x_im_wo_dedup, $psnu_w_dedup,
#' $updates (import to bring DATIM up to date with datapack), $deletes
#' (import to bring DATIM up to date with datapack)

compareData_OpuDatapackVsDatim <-
  function(d,
           d2_session = dynGet("d2_default_session",
                                  inherits = TRUE),
           datim_data = NULL,
           datastreams = c("mer_targets", "subnat_targets", "impatt")) {


    if (!(d$info$cop_year %in% supportedCOPYears())) {
      stop("Attempting to use compareData_OpuDatapackVsDatim for unsupported COP year")
    }

################################################################################
    included_data_elements <- getMapDataPack_DATIM_DEs_COCs(d$info$cop_year) %>%
      dplyr::select(dataelementuid, dataset) %>%
      dplyr::distinct()

    # Do not consider AGYW_PREV if this is a OPU Data Pack aka PSNUxIM
    if (d$info$tool == "OPU Data Pack") {
      included_data_elements <- included_data_elements %>%
        dplyr::filter(dataset != "dreams")
    }

    included_data_elements <- included_data_elements %>%
      dplyr::mutate(dataset = dplyr::case_when(dataset == "impatt" ~ "subnat_targets",
                                               dataset == "mer" ~ "mer_targets",
                                               dataset == "subnat" ~ "subnat_targets",
                                               dataset == "dreams" ~ "mer_targets",
                                               TRUE ~ dataset)) %>%
      dplyr::filter(dataset %in% datastreams)
################################################################################

    # current assumption is that d$datim$OPU has mech codes but this is planned to change
    # this assertion alerts us if the change is made and we forget to make necessary changes here:
    assertthat::assert_that(
      !any(datapackr::is_uidish(d$datim$OPU$attributeOptionCombo))
    )

    # datapack_data <- createDATIMExport(d)
    datapack_data <- d$datim$OPU %>%
      dplyr::filter(dataElement %in% included_data_elements$dataelementuid)

    #Need to make value a numeric
    datapack_data$value <- as.numeric(datapack_data$value)

    # recoding to account for code change in DATIM for the default COC
    # if all other code is updated to use uids instead of codes this can be removed
    datapack_data$categoryOptionCombo[datapack_data$categoryOptionCombo ==
                                        "HllvX50cXC0"] <- "default"
    datapack_data$attributeOptionCombo[datapack_data$attributeOptionCombo ==
                                         "HllvX50cXC0"] <- "default"

    # ensure datapack_data has the expected columns
    if (!setequal(
      names(datapack_data),
      c(
        "dataElement",
        "period",
        "orgUnit",
        "categoryOptionCombo",
        "attributeOptionCombo",
        "value"
      )
    )) {
      stop("The column names of your data aren't as expected by compareData_DatapackVsDatim.")
    }

    # Thu Sep 28 19:31:38 2023 ------------------------------
    # NOTE we grab dedupes here on the standard compare
    #Is this correct? It also has dplyr::filter(datapack_value != 0) at the bottom

    # rename columns to fit standards
    datapack_data <- datapack_data %>%
      dplyr::rename(
        datapack_value = value)

################################################################################
    #Refactor starts to diverge here.


    # Sum over IM including dedup
    datapack_data_psnu <- dplyr::group_by(datapack_data,
                                          dataElement,
                                          orgUnit,
                                          categoryOptionCombo) %>%
      dplyr::summarise(datapack_value = sum(datapack_value, na.rm = TRUE), .groups = "drop")

    datapack_data_psnu_x_im <- datapack_data




    # Get mer target data from DATIM using data value sets
    dataset_uids <- getCOPDatasetUids(d$info$cop_year,
                                      c("mer_targets"))

    # package parameters for getDataValueSets function call
    parameters <-
      dplyr::bind_rows(
        tibble::tibble(key = "dataSet", value = dataset_uids),
        tibble::tibble(key = "orgUnit", value = d$info$country_uids),
        tibble::tribble(~ key, ~ value,
                        "children", "true",
                        "categoryOptionComboIdScheme", "code",
                        "attributeOptionComboIdScheme", "code",
                        "includeDeleted", "false",
                        "period", paste0(d$info$cop_year, "Oct")
        )
      )

    # get data from datim using dataValueSets
    # rename to standard names
    datim_data <-
      datimutils::getDataValueSets(parameters$key,
                                   parameters$value,
                                   d2_session = d2_session) %>%
      dplyr::rename(datim_value = value) %>%
      dplyr::select(dataElement,
                    period,
                    orgUnit,
                    categoryOptionCombo,
                    attributeOptionCombo,
                    datim_value) %>%
      # AGYW data don't have mechs and aren't in OPU data packs
      # exclude them from comparison or any other data without mech
      dplyr::filter(attributeOptionCombo != "default")

################################################################################
    # extract dedupes from import file to handle separately
    dedupes <- dplyr::filter(datapack_data,
                             attributeOptionCombo %in%
                               c("00000", "00001")) %>%
      dplyr::rename(value = datapack_value)
################################################################################

    # for non dedups, extract cases where datapack and datim have different values
    data_differences <- dplyr::full_join(datapack_data, datim_data) %>%
      dplyr::filter(datapack_value != datim_value |
                      is.na(datapack_value) |
                      is.na(datim_value))

    # cases in which datim has a value but datapack does not
    deletes <- dplyr::filter(data_differences, is.na(datapack_value)) %>%
      dplyr::select(-datapack_value) %>%
      dplyr::rename(value = datim_value)

    # cases in which datapack has a new or updated value
    updates <- dplyr::filter(data_differences, !is.na(datapack_value)) %>%
      dplyr::select(-datim_value) %>%
      dplyr::rename(value = datapack_value)

    list(
      updates = updates,
      deletes = deletes,
      dedupes = dedupes
    )
  }
