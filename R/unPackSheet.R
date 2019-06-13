#' @importFrom magrittr %>% %<>%
#' @title unPackSheet(d)
#'
#' @description Within a submitted Data Pack or Site Tool (directed to by
#'    \code{d$keychain$submission_path}), extract data from a single sheet specified
#'    in \code{d$data$sheet}.
#'
#' @param d datapackr list object containing at least
#'     \code{d$keychain$submission_path},
#'     \code{d$data$sheet}, & \code{d$info$warningMsg}.
#' @return A datapackr list object, \code{d}, storing a dataframe of extracted
#'    data (\code{d$data$extract}) and a warning message
#'    (\code{d$info$warningMsg}) compiled with errors discovered while
#'    extracting data from sheet specified in \code{d$data$sheet}.
unPackSheet <- function(d) {
  addcols <- function(data, cname) {
    add <- cname[!cname %in% names(data)]
    
    if (length(add) != 0)
      data[add] <- NA_character_
    return(data)
  }
  
  d$data$extract <-
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet = d$data$sheet,
      range = readxl::cell_limits(c(5, 1), c(NA, NA))
    )
  
  # Run structural checks
  
  d <- checkColStructure(d)
  
  # List Target Columns
  targetCols <- datapackr::data_pack_schema %>%
    dplyr::filter(sheet_name == d$data$sheet,
                  col_type == "Target") %>%
    dplyr::pull(indicator_code)
  
  # Add cols to allow compiling with other sheets
  d$data$extract %<>%
    addcols(c("KeyPop", "Age", "Sex")) %>%
    # Extract PSNU uid
    dplyr::mutate(
      psnuid = stringr::str_extract(PSNU, "(?<=\\()([A-Za-z][A-Za-z0-9]{10})(?=\\)$)"),
      # Tag sheet name
      sheet_name = d$data$sheet
    ) %>%
    # Select only target-related columns
    dplyr::select(PSNU,
                  psnuid,
                  sheet_name,
                  Age,
                  Sex,
                  KeyPop,
                  dplyr::one_of(targetCols))
  
  
  if (d$data$sheet == "Prioritization") {
    d$data$extract %<>%
      dplyr::mutate(IMPATT.PRIORITY_SNU.20T = as.numeric(stringr::str_sub(
        IMPATT.PRIORITY_SNU.20T, start = 1, end = 2
      )))
  }
  
  
  d$data$extract %<>%
    tidyr::gather(key = "indicatorCode",
                  value = "value",
                  -PSNU,
                  -psnuid,
                  -Age,
                  -Sex,
                  -KeyPop,
                  -sheet_name) %>%
    dplyr::select(PSNU, psnuid, sheet_name, indicatorCode, Age, Sex, KeyPop, value) %>%
    # Drop zeros & NAs
    tidyr::drop_na(value) %>%
    dplyr::filter(value != 0)
  
  # TEST for non-numeric entries
  nonNumeric <- d$data$extract %>%
    dplyr::filter(!is.numeric(value)) %>%
    dplyr::select(value)
  
  if(NROW(nonNumeric) > 0) {
    msg <- paste0("In tab ", d$data$sheet, ": NON-NUMERIC VALUES found! -> ",
                  nonNumeric)
    d$info$warningMsg <- append(msg, d$info$warningMsg)
  }
  
  
  # TEST for Negative values
  has_negative_numbers <- d$data$extract$value < 0
  
  if (any(has_negative_numbers)) {
    negCols <- d$data$extract %>%
      dplyr::filter(value < 0) %>%
      dplyr::pull(indicatorCode) %>%
      unique() %>%
      paste(collapse = ", ")
    
    msg<-paste0("In tab ", d$data$sheet, ": NEGATIVE VALUES found! -> ", negCols, "")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  # TEST for duplicates
  any_dups <- d$data$extract %>%
    dplyr::select(sheet_name, PSNU, Age, Sex, KeyPop, indicatorCode) %>%
    dplyr::group_by(sheet_name, PSNU, Age, Sex, KeyPop, indicatorCode) %>%
    dplyr::summarise(n = (dplyr::n())) %>%
    dplyr::filter(n > 1) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::mutate(row_id = paste(PSNU, Age, Sex, KeyPop, indicatorCode, sep = "    ")) %>%
    dplyr::arrange(row_id) %>%
    dplyr::pull(row_id)
  
  if (NROW(any_dups) > 0) {
    msg<- paste0("In tab ", d$data$sheet, ": DUPLICATE ROWS -> ", 
                 paste(any_dups, collapse = ","))
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  # TEST for defunct disaggs
  defunct <- defunctDisaggs(d)
  
  if (NROW(defunct) > 0) {
    defunctMsg <- defunct %>%
      dplyr::mutate(msg = stringr::str_squish(paste(
        paste0(indicatorCode, ":"), Age, Sex, KeyPop
      ))) %>%
      dplyr::pull(msg)
    msg <- paste0("In tab ", d$data$sheet, ": DEFUNCT DISAGGS ->",
                  paste(defunctMsg, collapse = ","))
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  # Aggregate OVC_HIVSTAT
  if (d$data$sheet == "OVC") {
    d$data$extract %<>%
      dplyr::mutate(
        Age = dplyr::case_when(
          stringr::str_detect(indicatorCode, "OVC_HIVSTAT") ~ NA_character_,
          TRUE ~ Age),
        Sex = dplyr::case_when(
          stringr::str_detect(indicatorCode, "OVC_HIVSTAT") ~ NA_character_,
          TRUE ~ Sex)) %>%
      dplyr::group_by(PSNU, psnuid, sheet_name, indicatorCode, Age, Sex, KeyPop) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
  }
  
  # Add ages to PMTCT_EID
  if (d$data$sheet == "PMTCT_EID") {
    d$data$extract %<>%
      dplyr::mutate(
        Age = dplyr::case_when(
          stringr::str_detect(indicatorCode, "PMTCT_EID(.)+2to12mo") ~ "02 - 12 months",
          stringr::str_detect(indicatorCode, "PMTCT_EID(.)+2mo") ~ "<= 02 months",
          TRUE ~ Age
        )
      )
  }
  
  d$data$extract %<>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
    tidyr::drop_na(value)
  
  return(d)
  
}



#' @title unPackSiteToolSheet(d)
#'
#' @description Within a submitted Site Tool (directed to by
#'    \code{d$keychain$submission_path}), extract data from a single sheet specified
#'    in \code{d$data$sheet}.
#'
#' @param d datapackr list object containing at least
#'     \code{d$keychain$submission_path},
#'     \code{d$data$sheet}, & \code{d$info$warningMsg}.
#' @return A datapackr list object, \code{d}, storing a dataframe of extracted
#'    data (\code{d$data$extract}) and a warning message
#'    (\code{d$info$warningMsg}) compiled with errors discovered while
#'    extracting data from sheet specified in \code{d$data$sheet}.
unPackSiteToolSheet <- function(d) {
  
  addcols <- function(data, cname) {
    add <- cname[!cname %in% names(data)]
    
    if (length(add) != 0)
      data[add] <- NA_character_
    return(data)
  }
  
  d$data$extract <-
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet = d$data$sheet,
      range = readxl::cell_limits(c(5, 1), c(NA, NA)),
      col_types = "text"
    ) 
  #Check for unallocated values
  has_unallocated<-grepl("NOT A SITE",d$data$extract$Status) & grepl("NOT YET DISTRIBUTED",d$data$extract$Site)
  
  if (any(has_unallocated)) {
    msg<-paste0("ERROR! In tab ", d$data$sheet, ": Unallocated values found!")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
    d$info$has_error<-TRUE
  }
  
  #Proceed by removing unallocated rows
  d$data$extract%<>% 
    #TODO Ugly hack for NOT A SITE ROWS which have no site
    dplyr::filter(Status != "NOT A SITE") 
  
  #No rows
  if (NROW(d$data$extract) ==  0) {
    d$data$extract<-NULL
    return(d)
  }
  
  #Only empty rows
  is_empty_row <- function(x) {
    
    purrr::reduce(purrr::map(x, is.na), `+`) == NCOL(x)
    
  }
  
  empty_rows<-is_empty_row(d$data$extract)
  
  if ( all(empty_rows) ) {
    d$data$extract <-NULL
    return(d)
  }
  
  # Run structural checks before any filtering
  d <- checkSiteToolColStructure(d)
  
  actual_cols <- names(d$data$extract)
  # Static columns
  static_cols<-c("Status","Site",
                 "Mechanism","Type","Age","Sex","KeyPop")
  
  # List Target Columns
  targetCols <- datapackr::site_tool_schema %>%
    dplyr::filter(sheet_name == d$data$sheet,
                  col_type == "Target") %>%
    dplyr::pull(indicator_code) %>%
    append(static_cols,.)
  
  import_cols<-actual_cols[actual_cols %in% targetCols]
  
  # Add cols to allow compiling with other sheets
  d$data$extract %<>%
    #Only process needed columns
    dplyr::select(import_cols) %>%
    dplyr::filter_all(dplyr::any_vars(!is.na(.))) %>%
    #Filter out any unallocated dedupe values
    #TODO Not sure we should really be filtering Dedupe here?? 
    #dplyr::filter( stringr::str_detect(Mechanism,pattern = "Dedupe", negate = TRUE)) %>%
    dplyr::mutate( Mechanism = stringr::str_replace(Mechanism,"Dedupe","00000 - Deduplication") ) %>%
    addcols(c("KeyPop", "Age", "Sex")) %>%
    # Extract Site
    dplyr::mutate(
      site_uid = stringr::str_extract(Site, "(?<=\\[)([A-Za-z][A-Za-z0-9]{10})(?=\\]$)"),
      mech_code = stringi::stri_extract_first_regex(Mechanism, "^[0-9]{4,6}(?=\\s-)"),
      # Tag sheet name
      sheet_name = d$data$sheet
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
    tidyr::gather(key = "indicatorCode",
                  value = "value",
                  -Site,
                  -site_uid,
                  -mech_code,
                  -Type,
                  -Age,
                  -Sex,
                  -KeyPop,
                  -sheet_name) %>%
    dplyr::select(Site, site_uid,mech_code,Type, sheet_name, indicatorCode, Age, Sex, KeyPop, value) %>%
    # Drop zeros, NAs, dashes, and space-only entries
    tidyr::drop_na(value) %>%
    dplyr::filter(
      !is.na(suppressWarnings(as.numeric(value)))) %>% 
    dplyr::mutate(value = as.numeric(value))
  
  #Go ahead and filter any zeros, which are not dedupe
  d$data$extract %<>% dplyr::filter(value != 0 |  stringr::str_detect("00000", mech_code))
  
  # TEST for Negative values in non-dedupe mechanisms
  has_negative_numbers <-
    ( d$data$extract$value < 0 ) &
    stringr::str_detect("00000", d$data$extract$mech_code, negate = TRUE)
  
  if (any(has_negative_numbers)) {
    negCols <- d$data$extract %>%
      dplyr::filter( has_negative_numbers) %>% 
      dplyr::pull(indicatorCode) %>%
      unique() %>%
      paste(collapse = ", ")
    
    msg<-paste0("ERROR! In tab ", d$data$sheet, ": NEGATIVE VALUES found! -> ", negCols, "")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
    d$info$has_error<-TRUE
  }
  
  # TEST for positive values in dedupe mechanisms
  
  has_positive_dedupe <-
    (d$data$extract$value > 0) &
    stringr::str_detect("00000", d$data$extract$mech_code)
  
  if ( any( has_positive_dedupe ) ) {
    
    negCols <- d$data$extract %>%
      dplyr::filter(has_positive_dedupe) %>% 
      dplyr::pull(indicatorCode) %>%
      unique() %>%
      paste(collapse = ", ")
    
    msg<-paste0("ERROR! In tab ", d$data$sheet, ": POSITIVE DEDUPE VALUES found! -> ", negCols, "")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
    d$info$has_error<-TRUE
  }
  
  #Test for decimals
  has_decimals <- d$data$extract$value %% 1 != 0
  
  if (any(has_decimals)){
    msg <-
      paste0(
        "ERROR! In tab ",
        d$data$sheet,
        ":" ,
        sum(has_decimals),
        " DECIMAL VALUES found!")
    d$info$warningMsg <- append(msg, d$info$warningMsg)
    d$info$has_error <- TRUE
  }
  
  
  # TEST for duplicates
  any_dups <- d$data$extract %>%
    dplyr::select(sheet_name, Site, mech_code, Age, Sex, KeyPop, Type, indicatorCode) %>%
    dplyr::group_by(sheet_name, Site, mech_code, Age, Sex, KeyPop, Type, indicatorCode) %>%
    dplyr::summarise(n = (dplyr::n())) %>%
    dplyr::filter(n > 1) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::mutate(row_id = paste( Site, mech_code, Age, Sex, KeyPop, Type, indicatorCode, sep = "    ")) %>%
    dplyr::arrange(row_id) %>%
    dplyr::pull(row_id)
  
  if (length(any_dups) > 0) {
    msg<- paste0("In tab ", d$data$sheet, ":" , length(any_dups)," DUPLICATE ROWS. These will be aggregated!" ) 
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  # TEST for defunct disaggs
  defunct <- defunctDisaggs(d,
                            type = "Site Tool")
  
  if (NROW(defunct) > 0) {
    defunctMsg <- defunct %>%
      dplyr::mutate(msg = stringr::str_squish(paste(
        paste0(indicatorCode, ":"), Age, Sex, KeyPop
      ))) %>%
      dplyr::pull(msg)
    msg <- paste0("ERROR! In tab ", d$data$sheet, ": DEFUNCT DISAGGS ->",
                  paste(defunctMsg, collapse = ","))
    d$info$warningMsg<-append(msg,d$info$warningMsg)
    d$info$has_error<-TRUE
  }
  
  #Test for unallocated values
  
  not_yet_distributed <- stringr::str_detect(d$data$extract$Site,'NOT YET')
  
  if (any(not_yet_distributed)) {
    msg<- paste0("ERROR! In tab ", d$data$sheet, ": UNALLOCATED VALUES FOUND!")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
    d$info$has_error<-TRUE
  }
  
  #Test for any missing mechanisms
  if ( sum(is.na(d$data$extract$mech_code)) ) {
    msg<- paste0("ERROR! In tab ", d$data$sheet, ": blank mechanisms found!")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
    d$info$has_error<-TRUE
  }
  
  #Test for any missing mechanisms
  if ( sum(is.na(d$data$extract$Type)) ) {
    msg<- paste0("ERROR! In tab ", d$data$sheet, ": missing DSD/TA attributtion found!")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
    d$info$has_error<-TRUE
  }
  
  return(d)
  
}
