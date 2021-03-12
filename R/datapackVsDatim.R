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
                                  d2_session = d2_session)
      
      data$disagg <-
        datimvalidation::remapCategoryOptionCombos(data$categoryOptionCombo,
                                                   mode_in = "id",
                                                   mode_out = "name",
                                                   d2_session = d2_session)
      
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
    
    if(d$info$cop_year != 2020){
      stop("Attempting to use compareData_DatapackVsDatim for unsupported COP year")
    }
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
        datapack_value = value) %>%
      dplyr::filter(datapack_value != 0)

# Sum over IM including dedup    
    datapack_data_psnu_w_dedup <- dplyr::group_by(datapack_data,
                                                  dataElement,
                                                  orgUnit,
                                                  categoryOptionCombo) %>%
      dplyr::summarise(datapack_value = sum(datapack_value)) %>%
      dplyr::ungroup()
    
# data pack dedups use code 99999 - implies pure and crosswalk
# Get rid of dedups in data disaggregated by IM
    datapack_data_psnu_x_im_wo_dedup <- datapack_data %>%
      dplyr::filter(attributeOptionCombo != "99999") 
    
# Get data from DATIM using data value sets
    
    datim_data <- getCOPDataFromDATIM(country_uid = d$info$country_uids, 
                        cop_year = d$info$cop_year,
                        d2_session = d2_session) %>%
      dplyr::filter(value != 0) %>% # we don't import 0s up front so we should ignore any here
      dplyr::filter(value != "")
    
# recode dedups to be 99999 to match data pack  
    datim_data$attributeOptionCombo[datim_data$attributeOptionCombo == "00000" |
                                             datim_data$attributeOptionCombo == "00001"] <- "99999"
    
# Sum over IM including dedup    
    datim_data_psnu_w_dedup <- 
      dplyr::group_by(datim_data,
                      dataElement,
                      orgUnit,
                      categoryOptionCombo) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::rename(datim_value = value) %>%
      dplyr::ungroup()

# get rid of dedups in the data dissagregated by IM        
    datim_data_psnu_x_im_wo_dedup <- datim_data %>%
      dplyr::filter(attributeOptionCombo != "99999")
    
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
        dataElement,
        period,
        orgUnit,
        categoryOptionCombo,
        attributeOptionCombo,
        datapack_value
      )
    
# data in datim but not in the data pack
    data_datim_only <- 
      dplyr::filter(data_psnu_x_im_wo_dedup,
                    is.na(datapack_value)) %>%
      dplyr::select(
        dataElement,
        period,
        orgUnit,
        categoryOptionCombo,
        attributeOptionCombo,
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


#' @export
#' @title compareData_OpuDatapackVsDatim
#'
#' @description Compares the data in a parsed data pack that would be destined for DATIM with target data in in DATIM.
#' @param d list object - parsed data pack object
#' @param base_url string - base address of instance (text before api/ in URL)
#' @return  list object of diff result $psnu_x_im_wo_dedup, $psnu_w_dedup,
#' $updates (import to bring DATIM up to date with datapack), $deletes
#' (import to bring DATIM up to date with datapack)

compareData_OpuDatapackVsDatim <-
  function(d, d2_session = dynGet("d2_default_session",
                                  inherits = TRUE)) {
    
    if(d$info$cop_year != 2020){
      stop("Attempting to use compareData_DatapackVsDatim for unsupported COP year")
    }
    
    datapack_data <- d$datim$OPU
    
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
    
    # get data from datim usinfg dataValueSets
    # rename to standard names
    datim_data <-
      getDataValueSets(parameters$key,
                       parameters$value,
                       d2_session = d2_session) %>%
      dplyr::rename(
        datim_value = value) %>% 
      dplyr::select(dataElement,
                    period,
                    orgUnit,
                    categoryOptionCombo,
                    attributeOptionCombo,
                    datim_value)

# extract dedupes from import file to handle seperatly  
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