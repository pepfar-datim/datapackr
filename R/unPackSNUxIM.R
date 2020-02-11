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
  
  if (d$info$cop_year == 2020) {sheet = "PSNUxIM"} else {sheet = "SNU x IM"}
  
  d$data$SNUxIM <-
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet = sheet,
      range =
        readxl::cell_limits(
          c(headerRow(tool = d$info$tool, cop_year = d$info$cop_year),
            1),
          c(NA, NA)),
      col_types = "text"
    )
  
  if (NROW(d$data$SNUxIM) == 0) {return(d)}
  
  # Run structural checks ####
  d <- checkColStructure(d, "PSNUxIM")
  
  # Keep only columns we need
  toKeep <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet_name
                  & !indicator_code %in% c("Rollup", "sheet_num", "DataPackTarget", "ID")
  # Filter by what's in submission to avoid unknown column warning messages
                  & indicator_code %in% colnames(d$data$SNUxIM)) %>%
    dplyr::pull(indicator_code) %>% 
    unique(.)
  
  sj <- d$data$SNUxIM %>%
    dplyr::select(
      PSNU,
      indicator_code,
      dplyr::one_of(toKeep),
      dplyr::matches("Dedupe|(\\d){4,6}")) %>%
  # We don't need columns or rows with all NA targets -- Drop them.
    #dplyr::select_if(~!all(is.na(.))) %>%
    dplyr::filter(
      rowSums(!is.na(dplyr::select(., dplyr::matches("Dedupe|(\\d){4,6}")))) != 0
      ) %>%
  # Recalculate Dedupe
    dplyr::mutate_at(dplyr::vars(dplyr::matches("Dedupe|(\\d){4,6}")), as.numeric) %>%
    dplyr::mutate(
      mechanisms = rowSums(dplyr::select(., dplyr::matches("(\\d){4,6}")), na.rm = TRUE),
      Dedupe = dplyr::if_else(mechanisms > 1, 1 - mechanisms, 0)
    ) %>%
    dplyr::filter(mechanisms != 0) %>%
    dplyr::select(-mechanisms) %>%
  # # Align PMTCT_EID Age bands with rest of Data Pack (TODO: Fix in Data Pack, not here)
  #  dplyr::mutate(
  #    Age = dplyr::case_when(
  #      stringr::str_detect(indicator_code, "PMTCT_EID(.)+2to12mo") ~ "02 - 12 months",
  #      stringr::str_detect(indicator_code, "PMTCT_EID(.)+2mo") ~ "<= 02 months",
  #      TRUE ~ Age),
  #    Sex = dplyr::case_when(
  # # Drop Unknown Sex from PMTCT_EID (TODO: Fix in Data Pack, not here)
  #      stringr::str_detect(indicator_code, "PMTCT_EID") ~ NA_character_,
  # # Fix issues with HTS_SELF (duplicate and split by Male/Female) (TODO: Fix in Data Pack, not here)
  #      stringr::str_detect(indicator_code, "HTS_SELF(.)+Unassisted") ~ "Male|Female",
  #      TRUE ~ Sex)) %>%
  # tidyr::separate_rows(Sex, sep = "\\|") %>%
  # Create distribution matrix 
    tidyr::gather(
      key = "mechanism_code",
      value = "value",
      -PSNU, -indicator_code, -Age, -Sex, -KeyPop,
      na.rm = TRUE) %>%
    dplyr::group_by(PSNU, indicator_code, Age, Sex, KeyPop) %>%
    dplyr::mutate(
      distribution = value / sum(value)) %>%
    dplyr::ungroup() %>%
    tidyr::separate(mechanism_code, into=c("mechanism_code","support_type"), sep = "_") %>% 
    dplyr::mutate(
      psnuid = stringr::str_extract(PSNU, "(?<=\\[)([A-Za-z][A-Za-z0-9]{10})(?<!\\])"),
      mechanism_code = stringr::str_extract(mechanism_code, "(\\d{4,6})|Dedupe"),
      mechanism_code = stringr::str_replace(mechanism_code, "Dedupe", "99999")) %>%
    dplyr::select(PSNU, psnuid,  indicator_code, Age, Sex,
                  KeyPop, mechanism_code, distribution, SNUxIM_value = value)
    
  return(d)
}
