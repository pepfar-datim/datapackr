#' @export
#' @title Prepare PSNUxIM Model Data
#'
#' @description Prepare PSNUxIM Model Data
#'
#' @inheritParams datapackr_params
#'
#' @return d
#'
prepare_model_data.PSNUxIM <- function(snuxim_model_data,
                                       country_uids) {

  interactive_print("Getting data about your FY21 Mechanism Allocations from DATIM...")

  # Pivot wider ####
  if (inherits(snuxim_model_data, "list")) {
    #TODO: Consider replacing with something more straightforward like is.list()
    snuxim_model_data <- snuxim_model_data[country_uids] %>%
      dplyr::bind_rows()
  }



  # Drop all data that can't be allocated across mech & DSD/TA ####
  snuxim_model_data %<>%
    dplyr::filter(stringr::str_detect(mechanism_code, "\\d{4,}"),
                  stringr::str_detect(type, "DSD|TA"))

  # Pivot mechs/type wider ####
  snuxim_model_data %<>%
    tidyr::unite(col = mechcode_supporttype, mechanism_code, type) %>%
    dplyr::select(psnu_uid, indicator_code, Age = age_option_name,
                  Sex = sex_option_name, KeyPop = kp_option_name,
                  mechcode_supporttype, percent, value) %>%
    dplyr::mutate(
      mechcode_supporttype = dplyr::case_when(
        mechcode_supporttype == "00000_DSD" ~ "DSD Dedupe",
        mechcode_supporttype == "00000_TA" ~ "TA Dedupe",
        mechcode_supporttype == "00001_TA" ~ "Crosswalk Dedupe",
        TRUE ~ mechcode_supporttype
      )
    )

  percents <- snuxim_model_data %>%
    dplyr::select(-value) %>%
    tidyr::pivot_wider(names_from = mechcode_supporttype,
                       values_from = percent)

  values <- snuxim_model_data %>%
    dplyr::select(-percent, -mechcode_supporttype) %>%
    dplyr::group_by(dplyr::across(c(-value))) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()

  if (NROW(percents) != NROW(values)) {
    stop("Aggregating values and percents led to different row counts!")
  }

  snuxim_model_data <- values %>%
    dplyr::left_join(percents,
                     by = c("psnu_uid", "indicator_code", "Age", "Sex", "KeyPop"))

  # EID: Align model data age bands with Data Pack ####
  snuxim_model_data %<>%
    dplyr::mutate(
      Age = dplyr::if_else(
        indicator_code %in% c("PMTCT_EID.N.2.T", "PMTCT_EID.N.12.T"),
        NA_character_,
        Age
      )
    )

  # Double check that Dedupe cols all exist as expected ####
  snuxim_model_data %<>%
    datapackr::addcols(cnames = c("DSD Dedupe",
                                  "TA Dedupe",
                                  "Crosswalk Dedupe"),
                       type = "numeric")

  # Create Deduplicated Rollups ####
  snuxim_model_data %<>%
    dplyr::mutate(
      `Total Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}|HllvX50cXC0")), na.rm = TRUE),
      `DSD Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}_DSD")), na.rm = TRUE),
      `TA Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}_TA")), na.rm = TRUE)) %>%

  # Create Duplicated Rollups ####
    dplyr::mutate(
      `Deduplicated DSD Rollup` =
        rowSums(dplyr::select(., tidyselect::all_of(c("DSD Duplicated Rollup", "DSD Dedupe"))),
                na.rm = T),
      `Deduplicated TA Rollup` =
        rowSums(dplyr::select(., tidyselect::all_of(c("TA Duplicated Rollup", "TA Dedupe"))),
                na.rm = T)) %>%
    dplyr::mutate(
      `Total Deduplicated Rollup` =
        rowSums(
          dplyr::select(.,
                        tidyselect::all_of(c("Deduplicated DSD Rollup",
                                             "Deduplicated TA Rollup",
                                             "Crosswalk Dedupe"))),
          na.rm = TRUE
        )
    )

  # Create Max columns ####
  snuxim_model_data %<>%
    datapackr::rowMax(cn = "Max_TA.T_1", regex = "\\d{4,}_TA") %>%
    datapackr::rowMax(cn = "Max_DSD.T_1", regex = "\\d{4,}_DSD") %>%
    dplyr::mutate(
      `Max_Crosswalk.T_1` =
        pmax(`Deduplicated DSD Rollup`, `Deduplicated TA Rollup`, na.rm = T))

  # Create Dedupe Resolution columns ####
  interactive_print("Studying your deduplication patterns...")

  snuxim_model_data %<>%
    dplyr::rowwise() %>%
    dplyr::mutate(ta_im_count = sum(!is.na(dplyr::c_across(tidyselect::matches("\\d{4,}_TA")))),
                  dsd_im_count = sum(!is.na(dplyr::c_across(tidyselect::matches("\\d{4,}_DSD"))))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      `TA Dedupe Resolution (FY22)` = dplyr::case_when(
        `TA Duplicated Rollup` == 0 | ta_im_count <= 1 ~ NA_character_,
        # or where count(TA IMs) == 1
        `Deduplicated TA Rollup` == `TA Duplicated Rollup` ~ "SUM",
        `Deduplicated TA Rollup` == `Max_TA.T_1` ~ "MAX",
        TRUE ~ "CUSTOM"),
      `DSD Dedupe Resolution (FY22)` = dplyr::case_when(
        `DSD Duplicated Rollup` == 0 | dsd_im_count <= 1 ~ NA_character_,
        `Deduplicated DSD Rollup` == `DSD Duplicated Rollup` ~ "SUM",
        `Deduplicated DSD Rollup` == `Max_DSD.T_1` ~ "MAX",
        TRUE ~ "CUSTOM"),
      `Crosswalk Dedupe Resolution (FY22)` = dplyr::case_when(
        `Total Duplicated Rollup` == 0 | `Deduplicated TA Rollup` == 0 | `Deduplicated DSD Rollup` == 0
        ~ NA_character_,
        `Total Deduplicated Rollup` == `Total Duplicated Rollup` ~ "SUM",
        `Total Deduplicated Rollup` == `Max_Crosswalk.T_1` ~ "MAX",
        TRUE ~ "CUSTOM"),
      `Custom DSD Dedupe Allocation (FY22) (% of DataPackTarget)` = `DSD Dedupe`,
      `Custom TA Dedupe Allocation (FY22) (% of DataPackTarget)` = `TA Dedupe`,
      `Custom Crosswalk Dedupe Allocation (FY22) (% of DataPackTarget)` = `Crosswalk Dedupe`
    ) %>%
    dplyr::select(psnu_uid, indicator_code, Age, Sex, KeyPop,
                  tidyselect::matches("\\d{4,}"),
                  `Custom DSD Dedupe Allocation (FY22) (% of DataPackTarget)`,
                  `Custom TA Dedupe Allocation (FY22) (% of DataPackTarget)`,
                  `Custom Crosswalk Dedupe Allocation (FY22) (% of DataPackTarget)`,
                  `DSD Dedupe Resolution (FY22)`,
                  `TA Dedupe Resolution (FY22)`,
                  `Crosswalk Dedupe Resolution (FY22)`,
                  `DSD Dedupe`, `TA Dedupe`, `Crosswalk Dedupe`)

  return(snuxim_model_data)

}
