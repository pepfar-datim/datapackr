#' @export
#' @title Compare Data Pack Targets to DATIM
#'
#' @description Compares the data in a parsed data pack that would be
#' destined for DATIM with target data in in DATIM.
#' @inheritParams datapackr_params
#' @return  list object of diff result $psnu_x_im_wo_dedup, $psnu_w_dedup,
#' $updates (import to bring DATIM up to date with datapack), $deletes
#' (import to bring DATIM up to date with datapack)

compareData_DatapackVsDatim <-
  function(d,
           d2_session = dynGet("d2_default_session",
                               inherits = TRUE)) {

# internal beutify function to avoid repeated code used in the main function
# just handles some formatting/ decoding of UIDs
    beautify <- function(data) {
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
        datapackr::valid_PSNUs %>% dplyr::select(psnu, psnu_uid)

  # calculate diff between data pack and datim handling NAs like a 0
  # round diff to 5 decimal places so we don't get differences due to floating point error
  # add column summarizing the difference

      data %<>%
        dplyr::left_join(psnus, by = c("orgUnit" = "psnu_uid")) %>%
        dplyr::mutate(
          difference = dplyr::case_when(
            is.na(datapack_value) ~ -datim_value,
            is.na(datim_value) ~ datapack_value,
            TRUE ~ round(datapack_value - datim_value, 5)
          )
        ) %>%
        dplyr::mutate(
          effect = dplyr::case_when(
            is.na(difference) & is.na(datapack_value) ~ "Delete",
            is.na(difference) &
              is.na(datim_value) ~ "Create", !is.na(difference) &
              difference != 0 ~ "Update",
            difference == 0 ~ "No Change"
          )
        )
# select the columns of interest
# use one_of since the PSNU without dedups won't have mechanism
      suppressWarnings(dplyr::select(
        data,
        dplyr::one_of(
          "psnu",
          "data_element",
          "disagg",
          "attributeOptionCombo",
          "datapack_value",
          "datim_value",
          "difference",
          "effect"
        )
      ))
    }
# End Beautify function

# start main processing
# start off with dedups included

    if (d$info$cop_year != 2021) {
      stop("Attempting to use compareData_DatapackVsDatim for unsupported COP year")
    }
    # d <- datapackr::exportDistributedDataToDATIM(d, keep_dedup = TRUE)

    d$datim$MER$value <- as.numeric(d$datim$MER$value)

    d$datim$subnat_impatt$value <-
      as.numeric(d$datim$subnat_impatt$value)
    datapack_data <-
      dplyr::bind_rows(d$datim$MER, d$datim$subnat_impatt)

# recoding to account for code change in DATIM for the default COC
# if all other code is updated to use uids instead of codes this can be removed
    datapack_data$categoryOptionCombo[datapack_data$categoryOptionCombo ==
                                      "HllvX50cXC0"] <- "default"
    datapack_data$attributeOptionCombo[datapack_data$attributeOptionCombo ==
                                       "HllvX50cXC0"] <- "default"

    # ensure datapack_data has the expected columns
    if (!identical(
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
      dplyr::filter(datapack_value != 0)

# Sum over IM including dedup
    datapack_data_psnu <- dplyr::group_by(datapack_data,
                                                  dataElement,
                                                  orgUnit,
                                                  categoryOptionCombo) %>%
      dplyr::summarise(datapack_value = sum(datapack_value)) %>%
      dplyr::ungroup()

    datapack_data_psnu_x_im <- datapack_data

# Get data from DATIM using data value sets

    datim_data <- dplyr::bind_rows(
      getCOPDataFromDATIM(country_uids = d$info$country_uids,
                          cop_year = d$info$cop_year,
                          d2_session = d2_session),
      getCOPDataFromDATIM(country_uids = d$info$country_uids,
                          cop_year = d$info$cop_year - 1,
                          datastreams = c("subnat_targets"),
                          d2_session = d2_session)) %>%
      dplyr::filter(value != 0) %>% # we don't import 0s up front so we should ignore any here
      dplyr::filter(value != "") %>%
      dplyr::rename(datim_value = value)

# Sum over IM including dedup
    datim_data_psnu <-
      dplyr::group_by(datim_data,
                      dataElement,
                      orgUnit,
                      categoryOptionCombo) %>%
      dplyr::summarise(datim_value = sum(datim_value)) %>%
      dplyr::ungroup()

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
        abs(datapack_value - datim_value) > .000001 |
          is.na(datim_value)
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

    data_psnu_x_im %<>% beautify()

    data_psnu %<>% beautify() %>% dplyr::select(-effect)

    list(
      psnu_x_im = data_psnu_x_im,
      psnu = data_psnu,
      updates = data_different_value,
      deletes = data_datim_only
    )
  }


#' @export
#' @title Compare OPU Data Pack Targets to Datim
#'
#' @description Compares the data in a parsed data pack that would be destined
#' for DATIM with target data in in DATIM.
#' @inheritParams datapackr_params
#' @return  list object of diff result $psnu_x_im_wo_dedup, $psnu_w_dedup,
#' $updates (import to bring DATIM up to date with datapack), $deletes
#' (import to bring DATIM up to date with datapack)
#'
compareData_OpuDatapackVsDatim <-
  function(d, d2_session = dynGet("d2_default_session",
                                  inherits = TRUE)) {
# current assumption is that d$datim$OPU has mech codes but this is planned to change
# this assertion alerts us if the change is made and we forget to make necessary changes here:

    assertthat::assert_that(
      !any(datapackr::is_uidish(d$datim$OPU$attributeOptionCombo))
    )

    if (!(d$info$cop_year %in% c(2020, 2021))) {
      stop("Attempting to use compareData_OpuDatapackVsDatim for unsupported COP year")
    }
    datapack_data <- d$datim$OPU

# recoding to account for code change in DATIM for the default COC
# if all other code is updated to use uids instead of codes this can be removed
    datapack_data$categoryOptionCombo[datapack_data$categoryOptionCombo ==
                                        "HllvX50cXC0"] <- "default"
    datapack_data$attributeOptionCombo[datapack_data$attributeOptionCombo ==
                                         "HllvX50cXC0"] <- "default"

    # ensure datapack_data has the expected columns
    if (!identical(
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
        datapack_value = value)


    # Get mer target data from DATIM using data value sets
    dataset_uids <- getDatasetUids(d$info$cop_year + 1,
                                   c("mer_targets"))

    # package parameters for getDataValueSets function call
    parameters <-
      dplyr::bind_rows(
        tibble::tibble(key = "dataSet", value = dataset_uids),
        tibble::tibble(key = "orgUnit", value = d$info$country_uids),
        tibble::tribble(~ key, ~ value,
                        "children", "true",
                        "categoryOptionComboIdScheme", "code",
                        "includeDeleted", "false",
                        "period", paste0(d$info$cop_year, "Oct")
        )
      )

    # get data from datim using dataValueSets
    # rename to standard names
    datim_data <-
      getDataValueSets(parameters$key,
                       parameters$value,
                       d2_session = d2_session) %>%
      dplyr::rename(
        dataElement = data_element,
        orgUnit = org_unit,
        categoryOptionCombo = category_option_combo,
        attributeOptionCombo = attribute_option_combo,
        datim_value = value) %>%
      dplyr::select(dataElement,
                    period,
                    orgUnit,
                    categoryOptionCombo,
                    attributeOptionCombo,
                    datim_value) %>%
# AGYW data don't have mechs and aren't in OPU data packs
# exclude them from comparison or any other data without mech
      dplyr::filter(attributeOptionCombo != "default")

# extract dedupes from import file to handle seperately
    dedupes <- dplyr::filter(datapack_data,
                             attributeOptionCombo %in%
                               c("00000", "00001")) %>%
                               dplyr::rename(value = datapack_value)

# for non dedups, extract cases where datapack and datim have different values
    data_differences <- dplyr::full_join(datapack_data, datim_data) %>%
      dplyr::filter(datapack_value != datim_value |
                      is.na(datapack_value) |
                      is.na(datim_value)) %>%
      dplyr::filter(!(attributeOptionCombo %in%
                      c("00000", "00001")))

# cases in which datim has a value but datapack does not
    deletes <- dplyr::filter(data_differences, is.na(datapack_value)) %>%
      dplyr::select(-datapack_value) %>%
      dplyr::rename(value = datim_value)

# cases in which datapack has a new orupdated value
    updates <- dplyr::filter(data_differences, !is.na(datapack_value)) %>%
      dplyr::select(-datim_value) %>%
      dplyr::rename(value = datapack_value)

    list(
      updates = updates,
      deletes = deletes,
      dedupes = dedupes
    )
  }
