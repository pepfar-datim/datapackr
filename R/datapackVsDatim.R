#' @export
#' @title compareData_DatapackVsDatim
#'
#' @description Compares the data in a parsed data pack that would be destined for DATIM with target data in in DATIM.
#' @param d list object - parsed data pack object
#' @param base_url string - base address of instance (text before api/ in URL)
#' @return  list object of diff result $psnu_x_im_wo_dedup, $psnu_w_dedup,
#' $updates (import to bring DATIM up to date with datapack), $deletes
#' (import to bring DATIM up to date with datapack)

compareData_DatapackVsDatim <-
  function(d, base_url = getOption("baseurl")) { 
    
# internal beutify function to avoid repeated code used in the main function
# just handles some formatting/ decoding of UIDs
    beautify <- function(data) {
      data$data_element <-
        datimvalidation::remapDEs(data$data_element_uid,
                                  mode_in = "id",
                                  mode_out = "shortName")
      
      data$disagg <-
        datimvalidation::remapCategoryOptionCombos(data$category_option_combo_uid,
                                                   mode_in = "id",
                                                   mode_out = "name")
      
      psnus <-
        datapackr::valid_PSNUs %>% dplyr::select(psnu, psnu_uid)

  # calculate diff between data pack and datim handling NAs like a 0 
  # round diff to 5 decimal places so we don't get differences due to floating point error
  # add column summarizing the difference
      
      data %<>%
        dplyr::left_join(psnus, by = c("org_unit_uid" = "psnu_uid")) %>%
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
              is.na(datim_value) ~ "Create",!is.na(difference) &
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
          "attribute_option_combo_code",
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
    d <- datapackr::exportDistributedDataToDATIM(d, keep_dedup = TRUE)
    
    d$datim$MER$value <- as.numeric(d$datim$MER$value)
    d$datim$subnat_impatt$value <-
      as.numeric(d$datim$subnat_impatt$value)
    datapack_data <-
      dplyr::bind_rows(d$datim$MER, d$datim$subnat_impatt)
    
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
        datapack_value = value,
        data_element_uid = dataElement,
        org_unit_uid = orgUnit,
        category_option_combo_uid = categoryOptionCombo,
        attribute_option_combo_code = attributeOptionCombo
      ) %>%
      dplyr::filter(datapack_value != 0)

# Sum over IM including dedup    
    datapack_data_psnu_w_dedup <- dplyr::group_by(datapack_data,
                                                  data_element_uid,
                                                  org_unit_uid,
                                                  category_option_combo_uid) %>%
      dplyr::summarise(datapack_value = sum(datapack_value)) %>%
      dplyr::ungroup()
    
# data pack dedups use code 99999 - implies pure and crosswalk
# Get rid of dedups in data disaggregated by IM
    datapack_data_psnu_x_im_wo_dedup <- datapack_data %>%
      dplyr::filter(attribute_option_combo_code != "99999") 
    
    
# Get data from DATIM using data value sets
    
    org_unit_uids <- d$info$country_uids
    cop_yyyy <- d$info$cop_year %>% as.character()
    fiscal_yy <- (d$info$cop_year + 1) %>%
      stringr::str_sub(3, 4)
    
    dataset_uids <-
      c(datapackcommons::getDatasetUids(fiscal_yy, "targets"),
        datapackcommons::getDatasetUids(fiscal_yy, "subnat_impatt"))
    
# package parameters for getDataValueSets function call
    parameters <- 
      dplyr::bind_rows( 
        tibble::tibble(key = "dataSet", value = dataset_uids),
        tibble::tibble(key = "orgUnit", value = org_unit_uids),
        tibble::tribble(~ key, ~ value,
                        "children", "true",
                        "categoryOptionComboIdScheme", "code",
                        "includeDeleted", "false",
                        "period", paste0(cop_yyyy, "Oct")
                        )
        )
    
# get data from datim usinfg dataValueSets
# rename to standard names
    datim_data <-
      getDataValueSets(parameters$key,
                       parameters$value,
                       base_url = base_url) %>%
      dplyr::rename(
        datim_value = value,
        data_element_uid = data_element,
        org_unit_uid = org_unit,
        category_option_combo_uid = category_option_combo,
        attribute_option_combo_code = attribute_option_combo
      ) %>%
      dplyr::filter(datim_value != 0) %>% # we don't import 0s up front so we should ignore any here
      dplyr::filter(datim_value != "")
    
# recode dedups to be 99999 to match data pack  
    datim_data$attribute_option_combo_code[datim_data$attribute_option_combo_code == "00000" |
                                             datim_data$attribute_option_combo_code == "00001"] <- "99999"
    
# Sum over IM including dedup    
    datim_data_psnu_w_dedup <- 
      dplyr::group_by(datim_data,
                      data_element_uid,
                      org_unit_uid,
                      category_option_combo_uid) %>%
      dplyr::summarise(datim_value = sum(datim_value)) %>%
      dplyr::ungroup()

# get rid of dedups in the data dissagregated by IM        
    datim_data_psnu_x_im_wo_dedup <- datim_data %>%
      dplyr::filter(attribute_option_combo_code != "99999")
    
# join the data pack data and the datim data
    data_psnu_w_dedup <- dplyr::full_join(datim_data_psnu_w_dedup,
                                          datapack_data_psnu_w_dedup)
    
    data_psnu_x_im_wo_dedup <-
      dplyr::full_join(datim_data_psnu_x_im_wo_dedup,
                       datapack_data_psnu_x_im_wo_dedup)
    
# Find the cases with different values. These should be  imported into DATIM
    data_different_value <-
      dplyr::filter(
        data_psnu_x_im_wo_dedup,
        abs(datapack_value - datim_value) > .000001 |
          is.na(datim_value)
      ) %>%
      dplyr::select(
        data_element_uid,
        period,
        org_unit_uid,
        category_option_combo_uid,
        attribute_option_combo_code,
        datapack_value
      )
    
# data in datim but not in the data pack
    data_datim_only <- 
      dplyr::filter(data_psnu_x_im_wo_dedup,
                    is.na(datapack_value)) %>%
      dplyr::select(
        data_element_uid,
        period,
        org_unit_uid,
        category_option_combo_uid,
        attribute_option_combo_code,
        datim_value
      )
    
    data_psnu_x_im_wo_dedup %<>% beautify()
    
    data_psnu_w_dedup %<>% beautify() %>% dplyr::select(-effect)
    
    list(
      psnu_x_im_wo_dedup = data_psnu_x_im_wo_dedup,
      psnu_w_dedup = data_psnu_w_dedup,
      updates = data_different_value,
      deletes = data_datim_only
    )
  }
