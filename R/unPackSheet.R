#' @export
#' @title Unpack a Data Pack sheet.
#'
#' @description Within a submitted Data Pack or Site Tool (directed to by
#'    \code{d$keychain$submission_path}), extract data from a single sheet specified
#'    in \code{d$data$sheet}.
#'
#' @param d Datapackr object.
#' @param sheet Sheet to unpack.
#' 
#' @return d
#' 
unPackDataPackSheet <- function(d, sheet) {
  
  d$data$extract <-
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet = sheet,
      range = readxl::cell_limits(c(5, 1), c(NA, NA)),
      col_types = "text"
    )
  
  # Run structural checks ####
  d <- checkColStructure(d, sheet)
  
  # List Target Columns
  target_cols <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet,
                  col_type == "Target") %>%
    dplyr::pull(indicator_code)
  
  # Add cols to allow compiling with other sheets ####
  d$data$extract %<>%
    addcols(c("KeyPop", "Age", "Sex")) %>%
  # Extract PSNU uid
    dplyr::mutate(
      psnuid = stringr::str_extract(PSNU, "(?<=\\()([A-Za-z][A-Za-z0-9]{10})(?=\\)$)"),
  # Tag sheet name
      sheet_name = sheet
      ) %>%
  # Select only target-related columns
    dplyr::select(PSNU,
                  psnuid,
                  sheet_name,
                  Age,
                  Sex,
                  KeyPop,
                  dplyr::one_of(target_cols))
  
  if (sheet == "Prioritization") {
    d$data$extract %<>%
      dplyr::mutate(
        IMPATT.PRIORITY_SNU.20T =
          as.numeric(
            stringr::str_sub(
              IMPATT.PRIORITY_SNU.20T,
              start = 1,
              end = 2
            )
          )
      )
  }
  
  # Gather all indicators as single column for easier processing
  d$data$extract %<>%
    tidyr::gather(key = "indicator_code",
                  value = "value",
                  -PSNU,
                  -psnuid,
                  -Age,
                  -Sex,
                  -KeyPop,
                  -sheet_name) %>%
    dplyr::select(PSNU, psnuid, sheet_name, indicator_code, Age, Sex, KeyPop, value) %>%
  # Drop NAs ####
    tidyr::drop_na(value)

  # TODO: Move Prioritization mutate here?
  
  # TEST for non-numeric values ####
  d$tests$non_numeric <- d$data$extract %>%
    dplyr::mutate(value_numeric = as.numeric(value)) %>%
    dplyr::filter(is.na(value_numeric)) %>%
    dplyr::select(indicator_code, value) %>%
    dplyr::distinct() %>%
    dplyr::group_by(indicator_code) %>%
    dplyr::arrange(value) %>%
    dplyr::summarise(values = paste(value, collapse = ", ")) %>%
    dplyr::mutate(row_id = paste(indicator_code, values, sep = ":    ")) %>%
    dplyr::arrange(row_id) %>%
    dplyr::pull(row_id)
  
  if(length(d$tests$non_numeric) > 0) {
    warning_msg <-
      paste0(
        "In tab ",
        sheet,
        ": NON-NUMERIC VALUES found! ->  \n", 
        paste(d$tests$non_numeric, collapse = "\n"))
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  # Now that non-numeric cases noted, convert all to numeric & drop non-numeric ####
  d$data$extract %<>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
    tidyr::drop_na(value) %>%
  # Filter out zeros ####
    dplyr::filter(value != 0)
  
  # TEST for Negative values ####
  if (any(d$data$extract$value < 0)) {
    d$tests$neg_cols <- d$data$extract %>%
      dplyr::filter(value < 0) %>%
      dplyr::pull(indicator_code) %>%
      unique()
    
    warning_msg <- 
      paste0(
        "ERROR! In tab ",
        sheet,
        ": NEGATIVE VALUES found in the following columns! -> \n",
        paste(d$tests$neg_cols, collapse = "\n"),
        "")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
  }
  
  # TEST for duplicates ####
  d$tests$duplicates <- d$data$extract %>%
    dplyr::select(sheet_name, PSNU, Age, Sex, KeyPop, indicator_code) %>%
    dplyr::group_by(sheet_name, PSNU, Age, Sex, KeyPop, indicator_code) %>%
    dplyr::summarise(n = (dplyr::n())) %>%
    dplyr::filter(n > 1) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::mutate(row_id = paste(PSNU, Age, Sex, KeyPop, indicator_code, sep = "    ")) %>%
    dplyr::arrange(row_id) %>%
    dplyr::pull(row_id)
  
  if (length(d$tests$duplicates) > 0) {
    warning_msg <-
      paste0(
        "In tab ",
        sheet,
        ": DUPLICATE ROWS. These will be aggregated! -> \n", 
        paste(d$tests$duplicates, collapse = "\n"))
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  # TEST for defunct disaggs ####
  d$tests$defunct <- defunctDisaggs(d)
  
  if (NROW(d$tests$defunct) > 0) {
    defunct_msg <- d$tests$defunct %>%
      dplyr::mutate(
        msg = stringr::str_squish(
          paste(paste0(indicator_code, ":"), Age, Sex, KeyPop)
        )
      ) %>%
      dplyr::pull(msg) %>%
      paste(collapse = ",")
    
    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": INVALID DISAGGS ",
        "(Check MER Guidance for correct alternatives) ->",
        defunct_msg)
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
  }
  
  # Aggregate OVC_HIVSTAT
  # TODO: Fix this in the Data Pack. Not here...
  if (sheet == "OVC") {
    d$data$extract %<>%
      dplyr::mutate(
        Age = dplyr::case_when(
          stringr::str_detect(indicator_code, "OVC_HIVSTAT") ~ NA_character_,
          TRUE ~ Age),
        Sex = dplyr::case_when(
          stringr::str_detect(indicator_code, "OVC_HIVSTAT") ~ NA_character_,
          TRUE ~ Sex)) %>%
      dplyr::group_by(PSNU, psnuid, sheet_name, indicator_code, Age, Sex, KeyPop) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
  }
  
  # Add ages to PMTCT_EID
  # TODO: Fix this in the Data Pack. Not here...
  if (sheet == "PMTCT_EID") {
    d$data$extract %<>%
      dplyr::mutate(
        Age = dplyr::case_when(
          stringr::str_detect(indicator_code, "PMTCT_EID(.)+2to12mo") ~ "02 - 12 months",
          stringr::str_detect(indicator_code, "PMTCT_EID(.)+2mo") ~ "<= 02 months",
          TRUE ~ Age
        )
      )
  }
  
  return(d)
  
}



#' @export
#' @title unPackSiteToolSheet(d, sheet)
#'
#' @description Within a submitted Site Tool (directed to by
#'    \code{d$keychain$submission_path}), extract data from a single sheet specified
#'    in \code{sheet}.
#'
#' @param d Datapackr object.
#' @param sheet Sheet to unpack.
#' 
#' @return d
#' 
unPackSiteToolSheet <- function(d, sheet) {
  
  d$data$extract <-
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet = sheet,
      range = readxl::cell_limits(c(5, 1), c(NA, NA)),
      col_types = "text"
    ) 
  
  # Run structural checks before any filtering
  d <- checkColStructure(d, sheet)
  
  # List Target Columns
  targetCols <- datapackr::site_tool_schema %>%
    dplyr::filter(sheet_name == sheet,
                  col_type == "Target") %>%
    dplyr::pull(indicator_code)
  
  # Handle empty tabs ####
  d$data$extract %<>%
    dplyr::select(-Status) %>%
    dplyr::filter_all(., dplyr::any_cars(!is.na(.)))
  
  if (NROW(d$data$extract) == 0) {
    d$data$extract <- NULL
    return(d)
  }
  
  # Add cols to allow compiling with other sheets
  d$data$extract %<>%
    addcols(c("KeyPop", "Age", "Sex")) %>%
  # Extract Site id & Mech Code
    dplyr::mutate(
      site_uid = stringr::str_extract(Site, "(?<=\\[)([A-Za-z][A-Za-z0-9]{10})(?=\\]$)"),
      Mechanism = stringr::str_replace(Mechanism, "Dedupe", "00000 - Deduplication"),
      mech_code = stringi::stri_extract_first_regex(Mechanism, "^[0-9]{4,6}(?=\\s-)"),
  # Tag sheet name
      sheet_name = sheet
      ) %>%
  # Select only target-related columns
    dplyr::select(Site,
                  site_uid,
                  mech_code,
                  Type,
                  sheet_name,
                  Age,
                  Sex,
                  KeyPop,
                  dplyr::one_of(targetCols)) %>%
  # Gather all indicators in single column for easier processing
    tidyr::gather(key = "indicator_code",
                  value = "value",
                  -Site,
                  -site_uid,
                  -mech_code,
                  -Type,
                  -Age,
                  -Sex,
                  -KeyPop,
                  -sheet_name) %>%
    dplyr::select(Site, site_uid,mech_code,Type, sheet_name, indicator_code,
                  Age, Sex, KeyPop, value) %>%
  # Drop where value is zero, NA, dash, or space-only entry ####
    #TODO Add non-numeric test
    tidyr::drop_na(value) %>%
    dplyr::filter(
      !is.na(suppressWarnings(as.numeric(value)))) %>% 
    dplyr::mutate(value = as.numeric(value))
  
  # Check for non-sites ####
  d$tests$unallocated_data <- greply("NOT YET DISTRIBUTED", d$data$extract$Site)
  
  if (any(d$tests$unallocated_data)) {
    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": Values not allocated to Site level!")
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
  }
  
  # Proceed by removing unallocated rows ####
  d$data$extract %<>%
    dplyr::filter(stringr::str_detect(Site, "NOT YET DISTRIBUTED", negate = TRUE))
  
  # Filter target zeros, allowing for zero-value dedupes ####
  d$data$extract %<>%
    dplyr::filter(value != 0 | stringr::str_detect("00000", mech_code))
  
  # TEST for Negative values in non-dedupe mechanisms ####
  d$tests$has_negative_nondedupes <-
    ( d$data$extract$value < 0 ) &
    stringr::str_detect("00000", d$data$extract$mech_code, negate = TRUE)
  
  if (any(d$tests$has_negative_nondedupes)) {
    d$tests$neg_cols <- d$data$extract %>%
      dplyr::filter( d$tests$has_negative_nondedupes) %>% 
      dplyr::pull(indicator_code) %>%
      unique() %>%
      paste(collapse = ", ")
    
    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": NEGATIVE VALUES found! -> ",
        d$tests$neg_cols,
        "")
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
  }
  
  # TEST for positive values in dedupe mechanisms ####
  d$tests$has_positive_dedupes <-
    (d$data$extract$value > 0) &
    stringr::str_detect("00000", d$data$extract$mech_code)
  
  if ( any( d$tests$has_positive_dedupes ) ) {
    d$tests$pos_cols <- d$data$extract %>%
      dplyr::filter(d$tests$has_positive_dedupes) %>% 
      dplyr::pull(indicator_code) %>%
      unique() %>%
      paste(collapse = ", ")
    
    warning_msg <- paste0("ERROR! In tab ", d$data$sheet,
                  ": POSITIVE DEDUPE VALUES found! -> ",
                  d$tests$pos_cols,
                  "")
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
  }
  
  # TEST for decimals ####
  d$tests$has_decimals <- d$data$extract$value %% 1 != 0
  
  if (any(d$tests$has_decimals)){
    d$tests$decimals_found <- d$data$extract %>%
      dplyr::select(value) %>%
      dplyr::filter(value %% 1 != 0) %>%
      dplyr::distinct %>%
      dplyr::pull(value) %>%
      paste(collapse = ", ")
    
    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": " ,
        sum(d$tests$has_decimals),
        " DECIMAL VALUES found!: ",
        d$tests$decimals_found)
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
  }
  
  
  # TEST for duplicates ####
  d$tests$duplicates <- d$data$extract %>%
    dplyr::select(sheet_name, site_uid, mech_code, Age, Sex, KeyPop, Type, indicator_code) %>%
    dplyr::group_by(sheet_name, site_uid, mech_code, Age, Sex, KeyPop, Type, indicator_code) %>%
    dplyr::summarise(n = (dplyr::n())) %>%
    dplyr::filter(n > 1) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::mutate(row_id = paste(site_uid, mech_code, Age, Sex, KeyPop, Type, indicator_code, sep = "    ")) %>%
    dplyr::arrange(row_id) %>%
    dplyr::pull(row_id)
  
  if (length(d$tests$duplicates) > 0) {
    warning_msg <- 
      paste0(
        "In tab ",
        sheet,
        ":" ,
        length(d$tests$duplicates),
        " DUPLICATE ROWS. These will be aggregated!" ) 
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  # TEST for defunct disaggs ####
  d$tests$defunct <- defunctDisaggs(d)
  
  if (NROW(defunct) > 0) {
    defunct_msg <- d$tests$defunct %>%
      dplyr::mutate(
        msg = stringr::str_squish(
          paste(paste0(indicator_code, ":"), Age, Sex, KeyPop)
          )
        ) %>%
      dplyr::pull(msg) %>%
      paste(collapse = ",")
    
    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": INVALID DISAGGS ",
        "(Check MER Guidance for correct alternatives) ->",
        defunct_msg)
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
  }
  
  # TEST for any missing mechanisms ####
  d$tests$missing_mechs <- d$data$extract %>%
    dplyr::select(sheet_name, PSNU, Age, Sex, KeyPop, indicator_code) %>%
    dplyr::group_by(sheet_name, PSNU, Age, Sex, KeyPop, indicator_code) %>%
    dplyr::summarise(n = (dplyr::n())) %>%
    dplyr::filter(n > 1) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::mutate(row_id = paste(PSNU, Age, Sex, KeyPop, indicator_code, sep = "    ")) %>%
    dplyr::arrange(row_id) %>%
    dplyr::pull(row_id)
    
    dplyr::filter(is.na(mech_code)) %>%
    
  
  if (any(is.na(d$data$extract$mech_code)) ) {
    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": BLANK MECHANISMS found!")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
  }
  
  #TEST for any missing Types ####
  if (any(is.na(d$data$extract$Type)) ) {
    msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": MISSING DSD/TA ATTRIBUTION found!")
    
    d$info$warning_msg <- append(msg,d$info$warning_msg)
    d$info$has_error <- TRUE
  }
  
  return(d)
  
}
