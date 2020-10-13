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
      col_types = "text",
      .name_repair = "minimal"
    )
  
  if (NROW(d$data$SNUxIM) == 1 & is.na(d$data$SNUxIM[[1,1]])) {
    d$info$has_psnuxim <- FALSE
    
    return(d)
  } else {d$info$has_psnuxim <- TRUE}
  
  # Run structural checks ####
  d <- checkColStructure(d, "PSNUxIM")
  
  # Remove duplicate columns (Take the first example) ####
  duplicate_cols <- duplicated(names(d$data$SNUxIM))
  
  if (any(duplicate_cols)) {
    d$data$SNUxIM <- d$data$SNUxIM[,-which(duplicate_cols)]
  }
  
  # Make sure no blank column names ####
  d$data$SNUxIM %<>%
    tibble::as_tibble(.name_repair = "unique") %>%
  
  # Correct indicator_code name ####
    dplyr::rename_at(dplyr::vars(dplyr::matches("indicatorCode")), ~"indicator_code") %>%
  
  # Remove rows with NAs in key cols ####
    dplyr::filter_at(dplyr::vars(PSNU, indicator_code, ID), dplyr::any_vars(!is.na(.)))
  
  # TEST for missing metadata (PSNU, indicator_code, ID) ####
  d <- checkMissingMetadata(d, sheet)
  
  # TEST Column headers for appropriate structure ####
  expected_cols <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet,
                  !indicator_code %in% c("12345_DSD","12345_TA")) %>%
    dplyr::pull(indicator_code) %>% 
    unique(.)
  
  invalid_mech_headers <- d$data$SNUxIM %>%
    dplyr::select(-dplyr::one_of(expected_cols)) %>%
    dplyr::select(-dplyr::matches("(\\d){4,6}_(DSD|TA)")) %>%
    names()
  
  d$tests$invalid_mech_headers<-data.frame(invalid_mech_headers = invalid_mech_headers )
  attr(d$tests$invalid_mech_headers,"test_name")<-"Invalid mechanism headers"
  
  if (length(invalid_mech_headers) > 0) {
    
        warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ", INVALID COLUMN HEADERS: The following column headers are invalid and
        will be dropped in processing. Please use only the form 12345_DSD. ->  \n\t* ",
        paste(invalid_mech_headers, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  # Keep only columns we need ####
  toKeep <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet
                  & !indicator_code %in% c("Rollup", "sheet_num", "DataPackTarget", "ID")
  # Filter by what's in submission to avoid unknown column warning messages ####
                  & indicator_code %in% colnames(d$data$SNUxIM)) %>%
    dplyr::pull(indicator_code) %>% 
    unique(.)
  
  d$data$SNUxIM %<>%
    dplyr::select(
      PSNU,
      indicator_code,
      dplyr::one_of(toKeep),
      dplyr::matches("Dedupe|(\\d){4,6}_(DSD|TA)")) %>%
  
  # We don't need columns or rows with all NA targets -- Drop them. ####
    #dplyr::select_if(~!all(is.na(.))) %>%
    # dplyr::filter(
    #   rowSums(!is.na(dplyr::select(., dplyr::matches("Dedupe|(\\d){4,6}")))) != 0
    #   ) %>%
    
  # Align PMTCT_EID Age bands with rest of Data Pack (TODO: Fix in Data Pack, not here)
    dplyr::mutate(
      Age = dplyr::case_when(
        stringr::str_detect(indicator_code, "PMTCT_EID(.)+2to12mo") ~ "02 - 12 months",
        stringr::str_detect(indicator_code, "PMTCT_EID(.)+2mo") ~ "<= 02 months",
        TRUE ~ Age),
      
  # Align KP_MAT disaggs
      Sex = dplyr::case_when(indicator_code == "KP_MAT.N.Sex.T"
                             ~ stringr::str_replace(KeyPop, " PWID", ""),
                             TRUE ~ Sex),
      KeyPop = dplyr::case_when(indicator_code == "KP_MAT.N.Sex.T" ~ NA_character_,
                                TRUE ~ KeyPop),
      
  # Accommodate cases where user accidentally deletes Dedupe formula. This allows
  # rePackSNUxIM function to know the data is there somewhere.
      Dedupe = dplyr::case_when(is.na(Dedupe) ~ "0", TRUE ~ Dedupe),
  
  # Get other metadata needed for joining with other targets data
      psnuid = stringr::str_extract(PSNU, "(?<=\\[)([A-Za-z][A-Za-z0-9]{10})(?<!\\])"))
  
  # Prior to gathering, document all combos used in submitted PSNUxIM tab. ####
  # This ensures tests for new combinations are correctly matched
  d$data$PSNUxIM_combos <- d$data$SNUxIM %>%
    dplyr::select(PSNU, psnuid, indicator_code, Age, Sex, KeyPop) %>%
    dplyr::distinct()
  
  d$data$missingCombos <- d$data$MER %>%
    dplyr::anti_join(d$data$PSNUxIM_combos,
                     by =  c("PSNU", "psnuid", "indicator_code", "Age", "Sex", "KeyPop"))
  
  d$tests$missing_combos<-d$data$missingCombos
  attr(d$tests$missing_combos,"test_name")<-"Missing target combinations"

  d$info$missing_psnuxim_combos <- ( NROW(d$data$missingCombos) > 0 )
  
  if (d$info$missing_psnuxim_combos) {
    warning_msg <- 
      paste0(
        "INFO! Your DataPack may need a new PSNUxIM tab.",
        "This can be done via the self-service app in order to ",
        " receive an updated version of this tab. You can also submit a help desk ticket at",
        "DATIM.ZenDesk.com, or via logging in to www.DATIM.org and choosing the support app.",
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    
  }
  
  # TEST for duplicate rows ####
  d <- checkDuplicateRows(d, sheet)
  
  # Gather for joining ####
  d$data$SNUxIM %<>%
    tidyr::gather(
      key = "mechCode_supportType",
      value = "distribution",
      -PSNU, -indicator_code, -Age, -Sex, -KeyPop, -psnuid,
      na.rm = TRUE)
  
  # TEST for non-numeric values ####
  non_numeric <- d$data$SNUxIM %>%
    dplyr::mutate(distribution_numeric = suppressWarnings(as.numeric(distribution))) %>%
    dplyr::filter(is.na(distribution_numeric)) %>%
    dplyr::select(mechCode_supportType, distribution) %>%
    dplyr::distinct() %>%
    dplyr::group_by(mechCode_supportType) %>%
    dplyr::arrange(distribution) %>%
    dplyr::summarise(values = paste(distribution, collapse = ", ")) %>%
    tidyr::unite(row_id, c(mechCode_supportType, values), sep = ":  ") %>%
    dplyr::ungroup() %>%
    dplyr::arrange(row_id) %>%
    dplyr::select(row_id) %>% 
    dplyr::mutate(sheet=sheet)
  
  d$tests$non_numeric<-dplyr::bind_rows(d$tests$non_numeric, non_numeric)
  attr(d$tests$non_numeric,"test_name")<-"Non-numeric values"
  
  if(NROW(non_numeric) > 0) {

    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ": NON-NUMERIC VALUES found! ->  \n\t* ", 
        paste(non_numeric$row_id, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  # Drop non-numeric values
  d$data$SNUxIM %<>%
    dplyr::mutate(distribution = suppressWarnings(as.numeric(distribution))) %>%
    tidyr::drop_na(distribution) #%>%
    #dplyr::mutate(distribution = as.numeric(distribution)) # %>%
    #dplyr::filter(distribution != 0)
  
  # Get mech codes and support types ####
  d$data$SNUxIM %<>%
    dplyr::mutate(
      mechanism_code = stringr::str_extract(mechCode_supportType, "(\\d{4,6})|Dedupe"),
      mechanism_code = stringr::str_replace(mechanism_code, "Dedupe", "99999"),
      support_type = stringr::str_extract(mechCode_supportType, "(?<=_)DSD|TA")) %>%
    dplyr::select(PSNU, psnuid,  indicator_code, Age, Sex,
                  KeyPop, mechanism_code, support_type, distribution) %>% 
    dplyr::mutate(support_type = dplyr::case_when(mechanism_code == "99999" ~ 'DSD',
                                                  TRUE ~ support_type))
  
  return(d)
}
