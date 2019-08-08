#' @export
#' @title Flatten Data Pack Model 19
#' 
#' @description
#' Takes a COP 19 Data Pack Model data file and removes non-results content and flattens
#' the results hierarchy 
#' 
#' @param data list object with COP 19 model data
#' @param country_uids string vector listing country uids at the top level of the list hierarchy.
#' 
#' @return list object with one result set for each country named by country uid
#' @examples 
#' fake_data <- list(AAAAAAAAAAA = list(ou_name = "Country", ou_psnu_level = 4L, 
#'     TX = list(`TX_CURR.N.Age/Sex/HIVStatus.19T` = list(results = structure(list(
#'         indicator_uid = c("XC0nrb9ZbQR", "XC0nrb9ZbQR"), period = c("2018Oct", 
#'         "2018Oct"), org_unit_uid = c("AAAAAAAAAAA", "BBBBBBBBBBB"
#'         ), `Age: Cascade Age bands` = c("Z8MTaDxRBP6", "Z8MTaDxRBP6"
#'         ), `Cascade sex` = c("Gxcf2DK8vNc", "Gxcf2DK8vNc"), value = c(0, 
#'         0), age_dim_uid = c("e485zBiR7vG", "e485zBiR7vG"), age_dim_name = c("Age: Cascade Age bands", 
#'         "Age: Cascade Age bands"), age_dim_cop_type = c("age", 
#'         "age"), age_dim_item_name = c("<1", "<1"), age_option_name = c("<1", 
#'         "<1"), age_option_uid = c("sMBMO5xAq5T", "sMBMO5xAq5T"
#'         ), age_sort_order = c(10, 10), age_weight = c(1, 1), 
#'         age_model_sets = c("<1-50+", "<1-50+"), sex_dim_uid = c("jyUTj5YC3OK", 
#'         "jyUTj5YC3OK"), sex_dim_name = c("Cascade sex", "Cascade sex"
#'         ), sex_dim_cop_type = c("sex", "sex"), sex_dim_item_name = c("Unspecified sex", 
#'         "Unspecified sex"), sex_option_name = c("Female", "Male"
#'         ), sex_option_uid = c("Z1EnpTPaUfq", "Qn0I5FbKQOA"), 
#'         sex_sort_order = c(201, 202), sex_weight = c(0.5, 0.5
#'         ), sex_model_sets = c("F/M/U", "F/M/U")), class = c("tbl_df", 
#'     "tbl", "data.frame"), row.names = c(NA, -2L)), time = structure(1547800692.79299, class = c("POSIXct", 
#'     "POSIXt"), tzone = "UTC"), api_call = "")), TB_TX_PREV = list(
#'         TX_TB.D.newTBYield = list(api_call = "", time = structure(1547800712.5703, class = c("POSIXct", 
#'         "POSIXt"), tzone = "UTC"), results = structure(list(indicator_uid = character(0), 
#'             period = character(0), org_unit_uid = character(0), 
#'             value = numeric(0), age_option_uid = character(0), 
#'             sex_option_uid = character(0), kp_option_uid = character(0)), class = c("tbl_df", 
#'         "tbl", "data.frame"), row.names = c(NA, 0L))), TX_TB.D.alreadyTBYield = list(
#'             api_call = "", time = structure(1547800716.23698, class = c("POSIXct", 
#'             "POSIXt"), tzone = "UTC"), results = structure(list(
#'                 indicator_uid = character(0), period = character(0), 
#'                 org_unit_uid = character(0), value = numeric(0), 
#'                 age_option_uid = character(0), sex_option_uid = character(0), 
#'                 kp_option_uid = character(0)), class = c("tbl_df", 
#'             "tbl", "data.frame"), row.names = c(NA, 0L)))), Prioritization = list(
#'         IMPATT.PRIORITY_SNU.19T = list(results = structure(list(
#'             de = "r4zbW3owX9n", org_unit_uid = "AAAAAAAAAAA", 
#'             period = "2018Q4", value = 2), class = c("tbl_df", 
#'         "tbl", "data.frame"), row.names = c(NA, -1L), spec = structure(list(
#'             cols = list(Data = structure(list(), class = c("collector_character", 
#'             "collector")), `Organisation unit` = structure(list(), class = c("collector_character", 
#'             "collector")), Period = structure(list(), class = c("collector_character", 
#'             "collector")), Value = structure(list(), class = c("collector_double", 
#'             "collector"))), default = structure(list(), class = c("collector_character", 
#'             "collector"))), class = "col_spec")), time = structure(1547800722.09859, class = c("POSIXct", 
#'         "POSIXt"), tzone = "UTC"), api_call = ""))))
#'         
#'         str(fake_data)
#'         flattenDataPackModel_19(fake_data)
#' 
flattenDataPackModel_19 <- function(data, country_uids = NULL) {
  
  flattenCountry <- function(data){ # takes country level list and flattens sheet level
    sheets = c("PMTCT_STAT_ART", "PMTCT_EID", 
               "TB_STAT_ART", "VMMC", "TX", "HTS", "TB_TX_PREV", "OVC", "KP", 
               "PP", "PrEP", "GEND", "Prioritization")
    purrr::map(sheets, ~flattenSheet(data[[.x]])) %>% 
      dplyr::bind_rows() %>% 
      dplyr::rename(psnu_uid = org_unit_uid) %>% 
       dplyr::select(indicator_code,	
                     period,	
                     psnu_uid,	
                     age_option_uid,	
                     sex_option_uid,	
                     kp_option_uid,	
                     value)
    }
  
  flattenSheet <- function(data){ # takes sheet level list, extracts results data frames, 
                                  # adds indicator_code column and binds them
    indicators <-  names(data)
    purrr::map(indicators, 
               ~ data[[.x]][["results"]] %>% dplyr::mutate(indicator_code = .x)) %>% 
      dplyr::bind_rows()  
  }
  
  if (is.null(country_uids)){
# If no country list is provided - flatten them all

    country_uids = names(data)
    } else {
# make sure all the country uids provided are valid (e.g. part of the input file)
      if(!all(country_uids %in% names(data))){
        stop("One or more of the country uids is not contained in the data file")
      }
# filter down to the relevant subset of countries
      data <- data[country_uids]
    }
  
  purrr::map(country_uids, ~flattenCountry(data[[.x]])) %>% setNames(country_uids)
}
