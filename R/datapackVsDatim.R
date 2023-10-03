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

    if (!(d$info$cop_year %in% supportedCOPYears())) {
      stop("Attempting to use compareData_DatapackVsDatim for unsupported COP year")
    }

# filter to data elements in the stream specified
    included_data_elements <- getMapDataPack_DATIM_DEs_COCs(d$info$cop_year) %>%
      dplyr::select(dataelementuid, dataset) %>%
      dplyr::distinct() %>%
      dplyr::mutate(dataset = dplyr::case_when(dataset == "impatt" ~ "impatt",
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

# rename columns to fit standards
    datapack_data <- datapack_data %>%
      dplyr::rename(
        datapack_value = value) %>%
      dplyr::filter(datapack_value != 0 |
                      grepl("^0000[01]", attributeOptionCombo))

# Sum over IM including dedup
    datapack_data_psnu <- dplyr::group_by(datapack_data,
                                          dataElement,
                                          orgUnit,
                                          categoryOptionCombo) %>%
      dplyr::summarise(datapack_value = sum(datapack_value, na.rm = TRUE),
                       .groups = "drop")

    datapack_data_psnu_x_im <- datapack_data

# Get data from DATIM using data value sets
    if (is.null(datim_data)) {
      if (d$info$cop_year == 2022 &&
          "subnat_targets" %in% datastreams &&
          d$info$tool = "Data Pack") { # Get last year's subnat targets too
        datim_data <- dplyr::bind_rows(
          getCOPDataFromDATIM(country_uids = d$info$country_uids,
                              cop_year = d$info$cop_year,
                              datastreams = datastreams,
                              d2_session = d2_session),
          getCOPDataFromDATIM(country_uids = d$info$country_uids,
                              cop_year = d$info$cop_year - 1,
                              datastreams = c("subnat_targets"),
                              d2_session = d2_session))
      } else {
        datim_data <-
          getCOPDataFromDATIM(country_uids = d$info$country_uids,
                              cop_year = d$info$cop_year,
                              datastreams = datastreams,
                              d2_session = d2_session)
      }
    }


    if (!is.null(datim_data)) {
      datim_data %<>% dplyr::rename(datim_value = value)
      if (d$info$tool == "OPU Data Pack") {
### data in OPU datapacks must have a valid Mech or dedupe mech
### so we do not compare dreams or any other data without mech
        datim_data <- dplyr::filter(datim_data,
                                    attributeOptionCombo != "default")
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
      dplyr::summarise(datim_value = sum(datim_value),
                       .groups = "drop")

# get rid of dedups in the data dissagregated by IM
    datim_data_psnu_x_im <- datim_data

# join the data pack data and the datim data
    data_psnu <- dplyr::full_join(datim_data_psnu,
                                  datapack_data_psnu)

    data_psnu_x_im <-
      dplyr::full_join(datim_data_psnu_x_im,
                       datapack_data_psnu_x_im)

# Find the cases with different values. These should be  imported into DATIM
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
        value = datapack_value
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
        value = datim_value
      )

    data_psnu_x_im %<>% .compare_beautify(cop_year = d$info$cop_year,
                                          d2_session = d2_session)

    data_psnu %<>% .compare_beautify(cop_year = d$info$cop_year,
                                     d2_session = d2_session) %>% dplyr::select(-effect)

    list(
      psnu_x_im = data_psnu_x_im,
      psnu = data_psnu,
      updates = data_different_value,
      deletes = data_datim_only
    )
  }



#' @export
#' @title compareData_OpuDatapackVsDatim
#'
#' @description Legacy function maintained for backwards compatibility. If
#' processing an Datapack (either Datapack or a standalone PSNU tab)
#' as an OPU, only MER targets should be considered for processing.
#' @inheritParams datapackr_params
#' @param datim_data A data frame resulting from datimutils::getDataValueSets. If null, the data will be fetched
#' from DATIM.
#' @return  list object of diff result $psnu_x_im_wo_dedup, $psnu_w_dedup,
#' $updates (import to bring DATIM up to date with datapack), $deletes
#' (import to bring DATIM up to date with datapack)
compareData_OpuDatapackVsDatim <-
  function(d,
           d2_session = dynGet("d2_default_session",
                               inherits = TRUE),
           datim_data = NULL) {
    warning("This function is deprecated. Please use compareData_DatapackVsDatim instead. ")
    compareData_DatapackVsDatim(
      d,
      d2_session = d2_session,
      datim_data = datim_data,
      datastreams = "mer_targets"
    )
  }
