

extractDataPackModel <- function(d) {
  #The percentage columns are the first in the PSNUxIM tab.
  percent_cols <- duplicated(names(d$sheets$PSNUxIM))
  p <- d$sheets$PSNUxIM[, !percent_cols]

  #Map for Indicator codes/Age/Sex/KP/SupportType

  de_map <- getMapDataPack_DATIM_DEs_COCs(d$info$cop_year) %>%
    dplyr::select(indicator_code,
                  type = support_type,
                  Age = valid_ages.name,
                  Sex = valid_sexes.name,
                  KeyPop = valid_kps.name,
                  kp_option_uid = valid_kps.id,
                  categoryoptioncombouid,
                  dataelementuid,
                  period)



  header_cols <- c("PSNU", "indicator_code", "Age", "Sex", "KeyPop", "DataPackTarget")
  index_cols <- which(names(p) %in% header_cols)
  mech_cols <- which(grepl("^\\d{4,}_(DSD|TA)$", names(p)))

  p <- p[, c(index_cols, mech_cols)]

  p <- p %>% tidyr::pivot_longer(cols = -tidyselect::all_of(header_cols),
                                 names_to = "mechCode_supportType",
                                 values_to = "value",
                                 values_drop_na = TRUE) %>%
    tidyr::separate(mechCode_supportType,
                    into = c("mechanism_code", "type"),
                    sep = "_") %>%
    dplyr::mutate(psnu_uid = stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)")) %>%
    dplyr::select(-PSNU) %>%
    #Get the UIDs
    dplyr::left_join(de_map, by = c("indicator_code", "type", "Age", "Sex", "KeyPop")) %>%
    # Rectify the names
    dplyr::select(dataElement = dataelementuid,
                  period,
                  orgUnit = psnu_uid,
                  categoryOptionCombo = categoryoptioncombouid,
                  attributeOptionCombo = mechanism_code,
                  psnuxim_value = DataPackTarget,
                  percent = value
    )

  p
}
