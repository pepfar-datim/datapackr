#' @export
#' @title unPackSNUxIM(d)
#'
#' @description Looks inside submitted Data Pack to extract SNU x IM data from
#'     \code{SNU x IM} tab and restructure this to be ready for cross-
#'     pollination with PSNU-level MER data coming from
#'     \code{\link{unPackSheets}}. This data is also analyzed to identify
#'     structural or data anomalies and print any issues into running Warning
#'     Message queue.
#'
#' @param d Datapackr object

#' @return d
#' 
unPackSNUxIM <- function(d) {
  msg <- NULL
  
  d$data$SNUxIM <-
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet = "SNU x IM",
      range = readxl::cell_limits(c(5,1), c(NA, NA)),
      col_types = "text"
      )
  
  # Run structural checks
  d <- checkColStructure(d, "SNU x IM")
  
  # Keep only columns we need
  toKeep <- datapackr::data_pack_schema %>%
    dplyr::filter(sheet_name == "SNU x IM"
                  & !indicator_code %in% c("Mechanism1","ID","sheet_num","Rollup")) %>%
    dplyr::pull(indicator_code)
  
  d$data$SNUxIM %<>%
    dplyr::select(
      toKeep,
      dplyr::matches("Dedupe|(\\d){4,6}")) %>%
    dplyr::rename(indicator_code = indicatorCode) %>%
    
  # We don't need columns with all NA targets -- Drop them.
    dplyr::select_if(~!all(is.na(.))) %>%
    
  # TEST where Data Pack targets not fully distributed.
    dplyr::mutate_at(
      dplyr::vars(dplyr::matches("Data{aclTarget|Rollup|Dedupe|(\\d){4,6}")),
      as.numeric) %>%
    dplyr::mutate(
      mechanisms = rowSums(dplyr:select(., dplyr::matches("(\\d){4,6}|Dedupe")),
                           na.rm = TRUE),
      DataPackTarget = round_trunc(DataPackTarget),
      mechanisms = round_trunc(mechanisms)
    )
    
  d$info$SNUxIM_undistributed <- d$data$SNUxIM %>%
    dplyr::filter(DataPackTarget != mechanisms) %>%
    dplyr::select(PSNU,
                  indicator_code,
                  CoarseAge,
                  Sex,
                  KeyPop,
                  DataPackTarget,
                  mechanisms)
  
  if (NROW(d$info$SNUxIM_undistributed) > 0) {
    msg <- paste0(
      msg,
      "    ",
      NROW(d$info$SNUxIM_undistributed),
      " cases where Data Pack Targets are not correctly distributed among mechanisms. ",
      "To address this, go to your Data Pack's SNU x IM tab and filter the Rollup column for Pink cells."
      )
    d$info$warning_msg <- append(msg, d$info$warning_msg)
  }
  
  # Align PMTCT_EID Age bands with rest of Data Pack (TODO: Fix in Data Pack, not here)
  d$data$SNUxIM %<>%
    dplyr::mutate(
      CoarseAge = dplyr::case_when(
        stringr::str_detect(indicator_code, "PMTCT_EID(.)+2to12mo") ~ "02 - 12 months",
        stringr::str_detect(indicator_code, "PMTCT_EID(.)+2mo") ~ "<= 02 months",
        TRUE ~ CoarseAge),
      Sex = dplyr::case_when(
  # Drop Unknown Sex from PMTCT_EID (TODO: Fix in Data Pack, not here)
        stringr::str_detect(indicator_code, "PMTCT_EID") ~ NA_character_,
  # Fix issues with HTS_SELF (duplicate and split by Male/Female) (TODO: Fix in Data Pack, not here)
        stringr::str_detect(indicator_code, "HTS_SELF(.)+Unassisted") ~ "Male|Female",
        TRUE ~ Sex)) %>%
    tidyr::separate_rows(Sex, sep = "\\|") %>%
  # Create distribution matrix 
    dplyr::filter(mechanisms != 0) %>%
    dplyr::select(-DataPackTarget, -mechanisms) %>%
    tidyr::gather(
      key = "mechanismCode",
      value = "value",
      -PSNU, -sheet_name, -indicator_code, -CoarseAge, -Sex, -KeyPop) %>%
    tidyr::drop_na(value) %>%
    dplyr::group_by(PSNU, sheet_name, indicator_code, CoarseAge, Sex, KeyPop) %>%
    dplyr::mutate(distribution = value / sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      psnuid = stringr::str_extract(PSNU, "(?<=\\()([A-Za-z][A-Za-z0-9]{10})(?=\\)$)"),
      mechanismCode = stringr::str_extract(mechanismCode, "(\\d{4,6})|Dedupe")) %>%
    dplyr::select(PSNU, psnuid, sheet_name, indicator_code, CoarseAge, Sex,
                  KeyPop, mechanismCode, distribution)
  
  return(d)
}
