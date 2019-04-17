#' @title checkSiteToolOUinfo(d)
#'
#' @description Cross-checks and updates PEPFAR Operating Unit name and id as
#'  read from Data Pack or Site Tool submission file.
#'
#' @param d datapackr list object containing at least d$keychain$submission_path.
#' @return A datapackr list object, \code{d}, storing a unique UID and Name for
#'    the PEPFAR Operating Unit related to the submitted Data Pack or Site Tool.
checkSiteToolOUinfo <- function(d) {
  # Get OU name and uid
  d$info$datapack_uid <-
    names(readxl::read_excel(
      d$keychain$submission_path,
      sheet = "Home",
      range = "B25"
    ))
  d$info$datapack_name <-
    names(readxl::read_excel(
      d$keychain$submission_path,
      sheet = "Home",
      range = "B20"
    ))
  
  d$info$regional_country_name <- 
    names(readxl::read_excel(
      d$keychain$submission_path,
      sheet = "Home",
      range = "B21"
    ))
  
  # Check ou_name and ou_uid match
  datapack_name <- datapackr::configFile %>%
    dplyr::filter(model_uid == d$info$datapack_uid) %>%
    dplyr::select(DataPack_name) %>%
    unique() %>%
    dplyr::pull(DataPack_name)
  
  datapack_uid <- datapackr::configFile %>%
    dplyr::filter(DataPack_name == d$info$datapack_name) %>%
    dplyr::select(model_uid) %>%
    dplyr::pull(model_uid) %>% 
    unique()
  
  # If OU name and UID do not match, force identification via user prompt in Console
  if (d$info$datapack_name != datapack_name |
      d$info$datapack_uid != datapack_uid) {
    msg <-
      "The OU UID and OU name used in this submission don't match up!"
    interactive_print(msg)
    d$info$warningMsg <- append(msg, d$info$warningMsg)
    
    if (interactive()) {
      d$info$datapack_name <- selectOU()
    } else {
      stop(msg)
    }
    
    d$info$datapack_uid <- datapackr::configFile %>%
      dplyr::filter(DataPack_name == d$info$datapack_name) %>%
      dplyr::select(model_uid) %>%
      unique() %>%
      dplyr::pull(model_uid)
  }
  
  return(d)
}

#' @title checkSiteToolStructure(d)
#'
#' @description Checks structural integrity of tabs for SiteTool
#'
#' @param d datapackr list object containing at least d$keychain$submission_path
#'     & d$info$warningMsg.
#' @return A datapackr list object, \code{d}, storing a warning message of all
#'    issues related to Data Pack or Site Tool tab names or order.
checkSiteToolStructure <- function(d) {
  # Check structural integrity of Workbook tabs
  msg <- NULL
  ##Remove this after https://github.com/pepfar-datim/datapackr/issues/25
  site_tool_additional_sheets <-
    tibble::tribble(
      ~sheet_name, ~template_order, ~should_unpack,
      "Home",   1, FALSE,
      "Site List",   2, FALSE,
      "Mechs",   3, FALSE,
      "Validations", 4, FALSE
    )
  
  submission_sheets <-
    readxl::excel_sheets(d$keychain$submission_path) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::select(sheet_name = value) %>%
    dplyr::mutate(submission_order = as.integer(1:(dplyr::n())))
  
  # Check all tabs present and accounted for
  sheets_check <- datapackr::site_tool_schema %>%
    dplyr::select(sheet_name, template_order = sheet_num) %>%
    dplyr::distinct() %>%
    ##Remove this after https://github.com/pepfar-datim/datapackr/issues/25
    dplyr::mutate(template_order = template_order  + 1,
                  should_unpack = TRUE) %>% 
    dplyr::bind_rows(site_tool_additional_sheets,.) %>% 
    dplyr::left_join(submission_sheets, by = c("sheet_name")) %>%
    dplyr::mutate(order_check = template_order == submission_order)
  
  d$info$sheets_info <- sheets_check

  ## Alert to missing Sheets
  info_msg <- "Checking for any missing tabs..."
  interactive_print(info_msg)
  
  if (any(is.na(sheets_check$submission_order))) {
    missing_sheets <- sheets_check %>%
      dplyr::filter(is.na(submission_order)) %>%
      dplyr::pull(sheet_name)
    
    msg <- paste0(
      "MISSING SHEETS: Be advised that while deleting tabs will
      not prohibit data processing.: ",
      paste0(missing_sheets, collapse = ", "), "")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  ## Alert to added/renamed sheets
  interactive_print("Checking for any added or renamed tabs...")
  if (any(is.na(sheets_check$template_order))) {
    added_sheets <- sheets_check %>%
      dplyr::filter(is.na(template_order)) %>%
      dplyr::pull(sheet_name)
    msg <- paste0(
      "ADDED/RENAMED SHEETS: Be advised that while adding tabs for custom 
       purposes will not prohibit data processing, renaming existing tabs will.
      : ",paste(added_sheets, collapse = ", "),"")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  ## Alert to surprises in sheet order
  interactive_print("Checking for any surprises in tab order...")
  if (!all(sheets_check$order_check, na.rm = TRUE)) {
    out_of_order <- sheets_check %>%
      dplyr::filter(order_check == FALSE) %>%
      dplyr::pull(sheet_name)
    msg <- paste0(
      "SHEETS OUT OF ORDER: Be advised that reordering tabs may not prohibit
       data processing, and this issue may be related to missing, 
       added, or renamed sheets. : ", paste(out_of_order, collapse = ","),"")
    
    d$info$warningMsg <- append(msg,d$info$warningMsg)
  }
  
  return(d)
  
}

#' @title unPackSiteToolSheets(d)
#'
#' @description Loops through all critical sheets in a submitted 
#'     Site Tool and executes \code{\link{unPackSheet}} to extract data, then
#'     compiles data into single flat dataframe. Also executes
#'     \code{\link{separateDataSets}} to separate single dataframe into at least
#'     two for \code{MER} and \code{SUBNAT/IMPATT}.
#'
#' @param d datapackr list object containing at least
#'     \code{d$keychain$submission_path}.
#' @return A datapackr list object, \code{d}, storing at least 2 dataframes of
#'    data extracted from submitted Data Pack or Site Tool: a \code{d$data$MER}
#'    dataframe containing all MER data to be distributed to site level, and/or
#'    \code{d$data$SUBNAT_IMPATT} containing data in the SUBNAT and IMPATT
#'    datasets from DATIM that can be imported into DATIM at the PSNU level.
unPackSiteToolSheets <- function(d) {
  # Get sheets list
  sheets <- datapackr::site_tool_schema %>%
    dplyr::select(sheet_name) %>%
    dplyr::distinct() %>%
    dplyr::pull(sheet_name)
  
  actual_sheets <-
    readxl::excel_sheets(d$keychain$submission_path)
  sheets_to_read <- actual_sheets[actual_sheets %in% sheets]
  
  d$data$targets <- NULL
  
  for (i in 1:length(sheets_to_read)) {
    d$data$sheet = sheets_to_read[i]
    interactive_print(d$data$sheet)
    d <- unPackSiteToolSheet(d)
    
    if (!is.null(d$data$extract)) {
      d$data$targets <-
        dplyr::bind_rows(d$data$targets, d$data$extract)
    }
  
}
  
  return(d)
}

#' @title checkSiteToolColStructure(d)
#'
#' @description Checks structural integrity of columns on critical sheets for
#'    submitted  Site Tool.
#'
#' @param d datapackr list object containing at least d$data$extract,
#'     d$data$sheet & d$info$warningMsg.
#' @return A datapackr list object, \code{d}, storing a warning message of all
#'    issues related to Data Pack or Site Tool columns names or order.
checkSiteToolColStructure <- function(d) {
  # Check column structure
  msg <- NULL
  
  submission_cols <- names(d$data$extract) %>%
    tibble::as_tibble() %>%
    dplyr::select(indicatorCode = value) %>%
    dplyr::mutate(submission_order = as.integer(1:(dplyr::n())))
  
  col_check <- datapackr::site_tool_schema %>%
    dplyr::filter(sheet_name == d$data$sheet) %>%
    dplyr::select(indicator_code, template_order = col) %>%
    dplyr::full_join(submission_cols, by = c("indicator_code" = "indicatorCode")) %>%
    dplyr::mutate(order_check = template_order == submission_order)
  
  ## Alert to missing cols
  if (any(is.na(col_check$submission_order))) {
    missing_cols <- col_check %>%
      dplyr::filter(is.na(submission_order)) %>%
      dplyr::pull(indicator_code)
    msg <- paste0("In tab",d$data$sheet, 
                  " MISSING COLUMNS: Note that this may be due to missing/renamed sheets,
                  or added or renamed columns.:  ",
                  paste(missing_cols, collapse = ", "),"")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  ## Alert to added Columns
  if (any(is.na(col_check$template_order))) {
    added_cols <- col_check %>%
      dplyr::filter(is.na(template_order)) %>%
      dplyr::pull(indicator_code)
    msg <- paste0( "In tab ",d$data$sheet, 
                   " ADDED/RENAMED COLUMNS: DO NOT rename columns.
                   Adding columns is ok : ", 
                   paste(added_cols, collapse = ","),"")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  ## Alert to surprises in column order
  if (!all(col_check$order_check, na.rm = TRUE)) {
    out_of_order <- col_check %>%
      dplyr::filter(order_check == FALSE) %>%
      dplyr::pull(indicator_code)
    msg <- paste0("In tab ",d$data$sheet,
                  " COLUMNS OUT OF ORDER: Note that this may be due to missing, 
                  added, or renamed columns: ", 
                  paste(out_of_order, collapse = ","),"")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  return(d)
}

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

#' @title Derive non-Data Pack targets from others in the Data Pack/Site Tool
#' 
#' @description
#' Takes Data Pack or Site Tool data and derives other targets not explicitly
#' set during COP.
#' 
#' @param data Dataframe with either Data Pack or Site Tool data.
#' @param type Type of data, either \code{Data Pack}, or \code{Site Tool}.
#' 
#' @return Dataframe with added, derived targets.
#' 
deriveTargets <- function(data, type) {
  derived <- data %>%
    dplyr::filter(
      stringr::str_detect(
        indicatorCode, 
        paste0(
          "VMMC_CIRC\\.N\\.Age/Sex/HIVStatus\\.20T"
          # If we derive the SUBNAT/IMPATT ones, paste0 here
        )
      )
    )
  
  if(NROW(derived) > 0) {
    derived %<>%
      dplyr::mutate(
        indicatorCode =
          dplyr::case_when(
            stringr::str_detect(
              indicatorCode,
              "VMMC_CIRC\\.N\\.Age/Sex/HIVStatus\\.20T")
              ~ "VMMC_CIRC.N.Age/Sex.20T",
            # If we derive SUBNAT/IMPATT ones, add conditions here
            TRUE ~ indicatorCode
          )
      ) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
    
    combined <- data %>%
      dplyr::bind_rows(derived)
    
    return(combined)
    
  } else {return(data)}
  
}
