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
  
  d$data$SNUxIM <-
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet = "SNU x IM",
      range = readxl::cell_limits(c(5, 1), c(NA, NA)),
      col_types = "text"
    )
  
  # Run structural checks ####
  d <- checkColStructure(d, "SNU x IM")
  
  # Keep only columns we need
  toKeep <- d$info$schema %>%
    dplyr::filter(sheet_name == "SNU x IM"
                  & !indicator_code %in% c("Rollup", "sheet_num", "DataPackTarget", "ID")
  # Filter by what's in submission to avoid unknown column warning messages
                  & indicator_code %in% colnames(d$data$SNUxIM)) %>%
    dplyr::pull(indicator_code)
  
  d$data$SNUxIM %<>%
    dplyr::select(
      dplyr::one_of(toKeep),
      dplyr::matches("Dedupe|(\\d){4,6}")) %>%
    dplyr::rename(indicator_code = indicatorCode) %>% #TODO: Fix with other naming issues, preferably inside Data Pack
    
  # We don't need columns or rows with all NA targets -- Drop them.
    dplyr::select_if(~!all(is.na(.))) %>%
    dplyr::filter(
      rowSums(!is.na(dplyr::select(., dplyr::matches("Dedupe|(\\d){4,6}")))) != 0
      ) %>%
  
  # Align PMTCT_EID Age bands with rest of Data Pack (TODO: Fix in Data Pack, not here)
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
    tidyr::gather(
      key = "mechanism_code",
      value = "value",
      -PSNU, -sheet_name, -indicator_code, -CoarseAge, -Sex, -KeyPop,
      na.rm = TRUE) %>%
    dplyr::mutate(value = as.numeric(value)) %>%
    dplyr::filter(value != 0) %>%
    dplyr::group_by(PSNU, sheet_name, indicator_code, CoarseAge, Sex, KeyPop) %>%
    dplyr::mutate(distribution = value / sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      psnuid = stringr::str_extract(PSNU, "(?<=\\()([A-Za-z][A-Za-z0-9]{10})(?=\\)$)"),
      mechanism_code = stringr::str_extract(mechanism_code, "(\\d{4,6})|Dedupe"),
      mechanism_code = stringr::str_replace(mechanism_code, "Dedupe", "00000")) %>%
    dplyr::select(PSNU, psnuid, sheet_name, indicator_code, CoarseAge, Sex,
                  KeyPop, mechanism_code, distribution, SNUxIM_value = value) %>%
    dplyr::arrange(PSNU, psnuid, sheet_name, indicator_code, CoarseAge, Sex,
                   KeyPop, mechanism_code, distribution, SNUxIM_value)
    
  return(d)
}
