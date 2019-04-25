#' @title emptySiteToolSheetFrame(d)
#' @return An empty intermediate data frame when all values in a given Site
#' Tool sheet are completely empty. 
emptySiteToolSheetFrame<-function(){
  
  tibble::tribble(
    ~Site, 
    ~site_uid,
    ~mech_code,
    ~Type, 
    ~sheet_name, 
    ~indicatorCode, 
    ~Age, 
    ~Sex, 
    ~KeyPop, 
    ~value
  )
  
}


#' @title unPackSiteToolSheet(d)
#'
#' @description Within a submitted Site Tool (directed to by
#'    \code{d$keychain$submission_path}), extract data from a single sheet specified
#'    in \code{sheet}.
#'
#' @param d Datapackr object
#' @param sheet Sheet to unpack.
#'     
#' @return d
#' 
unPackSiteToolSheet <- function(d, sheet) {
  addcols <- function(data, cname) {
    add <- cname[!cname %in% names(data)]
    
    if (length(add) != 0)
      data[add] <- NA_character_
    return(data)
  }

  d$data$extract <-
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet = sheet,
      range = readxl::cell_limits(c(5, 1), c(NA, NA)),
      col_types = "text"
    ) %>%
    dplyr::rename(indicator_code = indicatorCode)
  
  # Check for unallocated values
  has_unallocated <- grepl("NOT A SITE",d$data$extract$Status) & grepl("NOT YET DISTRIBUTED",d$data$extract$Site)
  
  if (any(has_unallocated)) {
    msg <- paste0("ERROR! In tab ", sheet, ": Unallocated values found!")
    d$info$warning_msg <- append(msg, d$info$warning_msg)
    d$info$has_error <- TRUE
  }
  
  # Proceed by removing unallocated rows
  d$data$extract %<>% 
    #TODO Ugly hack for NOT A SITE ROWS which have no site
    dplyr::filter(Status != "NOT A SITE" &
                    !stringr::str_detect(Site, "NOT YET DISTRIBUTED"))
  
  # No rows
  if (NROW(d$data$extract) ==  0) {
    d$data$extract <- NULL
    return(d)
  }
  
  # Only empty rows
  is_empty_row <- function(x) {
    purrr::reduce(purrr::map(x, is.na), `+`) == NCOL(x)
  }
  
  empty_rows <- is_empty_row(d$data$extract)
  
   if (all(empty_rows)) {
     d$data$extract <- NULL
   return(d)
   }
  
  # Run structural checks before any filtering
  d <- checkColStructure(d, sheet)
  
  actual_cols <- names(d$data$extract)
  # Static columns
  static_cols <- c("Status","Site",
                   "Mechanism","Type","Age","Sex","KeyPop")
  
  # List Target Columns
  targetCols <- datapackr::site_tool_schema %>%
    dplyr::filter(sheet_name == sheet,
                  col_type == "Target") %>%
    dplyr::pull(indicator_code) %>%
    append(static_cols, .)
  
  import_cols <- actual_cols[actual_cols %in% targetCols]
  
  # Add cols to allow compiling with other sheets
  d$data$extract %<>%
    #Only process needed columns
    dplyr::select(import_cols) %>%
    dplyr::filter_all(dplyr::any_vars(!is.na(.))) %>%
    dplyr::mutate(Mechanism = stringr::str_replace(Mechanism,"Dedupe","00000 - Deduplication") ) %>%
    addcols(c("KeyPop", "Age", "Sex")) %>%
    # Extract Site id & Mech Code
    dplyr::mutate(
      site_uid = stringr::str_extract(Site, "(?<=\\[)([A-Za-z][A-Za-z0-9]{10})(?=\\]$)"),
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
    dplyr::pull(indicator_code) %>%
    unique() %>%
    paste(collapse = ", ")
    
    msg <- paste0("ERROR! In tab ", sheet, ": NEGATIVE VALUES found! -> ", negCols, "")
    d$info$warning_msg <- append(msg, d$info$warning_msg)
    d$info$has_error <- TRUE
  }
  
  # TEST for positive values in dedupe mechanisms
  
  has_positive_dedupe <-
    (d$data$extract$value > 0) &
    stringr::str_detect("00000", d$data$extract$mech_code)
  
  if ( any( has_positive_dedupe ) ) {
    
    negCols <- d$data$extract %>%
      dplyr::filter(has_positive_dedupe) %>% 
      dplyr::pull(indicator_code) %>%
      unique() %>%
      paste(collapse = ", ")
    
    msg<-paste0("ERROR! In tab ", sheet, ": POSITIVE DEDUPE VALUES found! -> ", negCols, "")
    d$info$warning_msg <- append(msg, d$info$warning_msg)
    d$info$has_error <- TRUE
  }
  
  #Test for decimals
  has_decimals <- d$data$extract$value %% 1 != 0
  
  if (any(has_decimals)){
    msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ":" ,
        sum(has_decimals),
        " DECIMAL VALUES found!")
    d$info$warning_msg <- append(msg, d$info$warning_msg)
    d$info$has_error <- TRUE
  }
  
  
  # TEST for duplicates
  any_dups <- d$data$extract %>%
    dplyr::select(sheet_name, Site, mech_code, Age, Sex, KeyPop, Type, indicator_code) %>%
    dplyr::group_by(sheet_name, Site, mech_code, Age, Sex, KeyPop, Type, indicator_code) %>%
    dplyr::summarise(n = (dplyr::n())) %>%
    dplyr::filter(n > 1) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::mutate(row_id = paste( Site, mech_code, Age, Sex, KeyPop, Type, indicator_code, sep = "    ")) %>%
    dplyr::arrange(row_id) %>%
    dplyr::pull(row_id)
  
  if (length(any_dups) > 0) {
    msg <- paste0("In tab ", sheet, ": ",
                  length(any_dups),
                  " DUPLICATE ROWS. These will be aggregated!" ) 
    d$info$warning_msg <- append(msg,d$info$warning_msg)
  }
  
  # TEST for defunct disaggs
  defunct <- defunctDisaggs(d,
                            type = "Site Tool")
  
  if (NROW(defunct) > 0) {
    defunctMsg <- defunct %>%
      dplyr::mutate(msg = stringr::str_squish(paste(
        paste0(indicator_code, ":"), Age, Sex, KeyPop
      ))) %>%
      dplyr::pull(msg)
    msg <- paste0("ERROR! In tab ", sheet, ": DEFUNCT DISAGGS ->",
                  paste(defunctMsg, collapse = ","))
    d$info$warning_msg <- append(msg, d$info$warning_msg)
    d$info$has_error <- TRUE
  }

  # Test for any missing mechanisms
  if ( sum(is.na(d$data$extract$mech_code)) ) {
    msg <- paste0("ERROR! In tab ", sheet, ": blank mechanisms found!")
    d$info$warning_msg <- append(msg, d$info$warning_msg)
    d$info$has_error <- TRUE
  }
  
  #Test for any missing Types
  if ( sum(is.na(d$data$extract$Type)) ) {
    msg <- paste0("ERROR! In tab ", sheet, ": missing DSD/TA attributtion found!")
    d$info$warning_msg <- append(msg, d$info$warning_msg)
    d$info$has_error <- TRUE
  }
  
  return(d)
  
}
