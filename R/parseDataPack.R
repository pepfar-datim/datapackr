#' @importFrom magrittr %>% %<>%
#' @title checkOUinfo(d)
#'
#' @description Cross-checks and updates PEPFAR Operating Unit name and id as
#'  read from Data Pack or Site Tool submission file.
#'
#' @param d datapackr list object containing at least d$keychain$submission_path.
#' @return A datapackr list object, \code{d}, storing a unique UID and Name for
#'    the PEPFAR Operating Unit related to the submitted Data Pack or Site Tool.
checkOUinfo <- function(d) {
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
  
  # Check ou_name and ou_uid match
  datapack_name <- datapackr::configFile %>%
    dplyr::filter(model_uid == d$info$datapack_uid) %>%
    dplyr::select(DataPack_name) %>%
    unique() %>%
    dplyr::pull(DataPack_name)
  
  datapack_uid <- datapackr::configFile %>%
    dplyr::filter(DataPack_name == d$info$datapack_name) %>%
    dplyr::select(model_uid) %>%
    unique() %>%
    dplyr::pull(model_uid)
  
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


#' @importFrom magrittr %>% %<>%
#' @title checkWorkbookStructure(d)
#'
#' @description Checks structural integrity of tabs for submitted Data Pack or
#'    Site Tool.
#'
#' @param d datapackr list object containing at least d$keychain$submission_path
#'     & d$info$warningMsg.
#' @return A datapackr list object, \code{d}, storing a warning message of all
#'    issues related to Data Pack or Site Tool tab names or order.
checkWorkbookStructure <- function(d) {
  # Check structural integrity of Workbook tabs
  msg <- NULL
  
  submission_sheets <-
    readxl::excel_sheets(d$keychain$submission_path) %>%
    tibble::as_tibble() %>%
    dplyr::select(sheet_name = value) %>%
    dplyr::filter(!sheet_name %in% c("Home", "Quotes", "Summary", "Spectrum", "Validations")) %>%
    dplyr::mutate(submission_order = as.integer(1:(dplyr::n()) + 4))
  
  # Check all tabs present and accounted for
  sheets_check <- datapackr::template_schema %>%
    dplyr::select(sheet_name, template_order = sheet_num) %>%
    dplyr::distinct() %>%
    dplyr::full_join(submission_sheets, by = c("sheet_name")) %>%
    dplyr::mutate(order_check = template_order == submission_order)
  
  ## Alert to missing Sheets
  info_msg <- "Checking for any missing tabs..."
  interactive_print(info_msg)
  
  if (any(is.na(sheets_check$submission_order))) {
    missing_sheets <- sheets_check %>%
      dplyr::filter(is.na(submission_order)) %>%
      dplyr::pull(sheet_name)
    
    msg <- paste0(
      "MISSING SHEETS: Be advised that while deleting tabs will
      not prohibit data processing,
      it may cause issues in formulas in the SNU x IM tab.
      : ",
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
    
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  return(d)
  
  }


#' @importFrom magrittr %>% %<>%
#' @title checkColStructure(d)
#'
#' @description Checks structural integrity of columns on critical sheets for
#'    submitted Data Pack or Site Tool.
#'
#' @param d datapackr list object containing at least d$data$extract,
#'     d$data$sheet & d$info$warningMsg.
#' @return A datapackr list object, \code{d}, storing a warning message of all
#'    issues related to Data Pack or Site Tool columns names or order.
checkColStructure <- function(d) {
  # Check column structure
  msg <- NULL
  
  submission_cols <- names(d$data$extract) %>%
    tibble::as_tibble() %>%
    dplyr::select(indicatorCode = value) %>%
    dplyr::mutate(submission_order = as.integer(1:(dplyr::n())))
  
  col_check <- datapackr::template_schema %>%
    dplyr::filter(sheet_name == d$data$sheet) %>%
    dplyr::select(indicatorCode, template_order = col) %>%
    dplyr::full_join(submission_cols, by = c("indicatorCode")) %>%
    dplyr::mutate(order_check = template_order == submission_order)
  
  ## Alert to missing cols
  if (any(is.na(col_check$submission_order))) {
    missing_cols <- col_check %>%
      dplyr::filter(is.na(submission_order)) %>%
      dplyr::pull(indicatorCode)
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
      dplyr::pull(indicatorCode)
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
      dplyr::pull(indicatorCode)
    msg <- paste0("In tab ",d$data$sheet,
      " COLUMNS OUT OF ORDER: Note that this may be due to missing, 
        added, or renamed columns: ", 
        paste(out_of_order, collapse = ","),"")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  return(d)
  }


#' @importFrom magrittr %>% %<>%
#' @title defunctDisaggs(d)
#'
#' @description Checks data extracted from a sheet in a submitted Data Pack or
#'    Site Tool to identify cases where invalid Disaggregate combinations have
#'    been used.
#'
#' @param d datapackr list object containing at least d$data$extract of data
#'     from a single sheet in submission file.
#' @return A datapackr list object, \code{d}, storing a dataframe of defunct
#'    disaggs used in a specific sheet.
defunctDisaggs <- function(d) {
  defunct <- d$data$extract %>%
    replace(is.na(.), "") %>%
    dplyr::filter(
      (
        stringr::str_detect(indicatorCode, "Malnutrition|Pediatric") &
          Age != "01-04"
      )
      |
        (
          stringr::str_detect(indicatorCode, "HTS_RECENT|PrEP") &
            Age %in% c("01-04", "05-09", "10-14")
        )
      |
        (
          stringr::str_detect(
            indicatorCode,
            "HTS_TST_PMTCTPostANC1|PMTCT_STAT|PMTCT_ART"
          ) & (Age %in% c("<01", "01-04", "05-09") | Sex == "Male")
        )
      |
        (
          stringr::str_detect(indicatorCode, "HTS_SELF|PP_PREV") &
            Age %in% c("01-04", "05-09")
        )
      |
        (
          stringr::str_detect(
            indicatorCode,
            "(HTS_TST|KP_ESTIMATES|PrEP|TX_NEW)(.)+KeyPop"
          )
          &
            !KeyPop %in% c(
              "FSW",
              "MSM",
              "People in prisons and other enclosed settings",
              "PWID",
              "TG"
            )
        )
      |
        (
          stringr::str_detect(indicatorCode, "KP_MAT") &
            !KeyPop %in% c("Female PWID", "Male PWID")
        )
      | (
        stringr::str_detect(indicatorCode, "KP_PREV")
        &
          !KeyPop %in% c(
            "Female PWID",
            "Male PWID",
            "FSW",
            "MSM not SW",
            "MSM SW",
            "People in prisons and other enclosed settings",
            "TG not SW",
            "TG SW"
          )
      )
      |
        (
          stringr::str_detect(indicatorCode, "PRIORITY_SNU") &
            !value %in% (datapackr::prioritizations %>%
                           dplyr::pull(value))
        )
      |
        (
          stringr::str_detect(indicatorCode, "OVC_HIVSTAT|OVC_SERV") &
            !Age %in% c("<01", "01-04", "05-09", "10-14", "15-17", "18+")
        )
      |
        (
          stringr::str_detect(indicatorCode, "VMMC") &
            (Age %in% c("<01", "01-04", "05-09") | Sex == "Female")
        )
    ) %>%
    dplyr::select(indicatorCode, Age, Sex, KeyPop) %>%
    dplyr::distinct()
  
  return(defunct)
}


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
  
  # List FY20 Target Columns
  targetCols <- datapackr::template_schema %>%
    dplyr::filter(sheet_name == d$data$sheet,
                  colType == "FY20 Target") %>%
    dplyr::pull(indicatorCode)
  
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
    # Drop zeros, NAs, dashes, and space-only entries
        tidyr::drop_na(value) %>%
        dplyr::filter(value != 0
                      & !value %in% c("-"," ")) %>%
        dplyr::mutate(value = as.numeric(value))

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

    return(d)

}

#' @importFrom magrittr %>% %<>%
#' @title separateDataSets(d)
#'
#' @description After data has been extracted from all sheets in a Data Pack or
#'     Site Tool, this function separates datasets by either \code{MER} or
#'     \code{SUBNAT/IMPATT} and removes elements of \code{d} that are no longer
#'     necessary (\code{targets}, \code{extract}, and \code{sheet})
#'
#' @param d datapackr list object containing at least \code{d$data$targets}.
#' @return A datapackr list object, \code{d}, storing at least 2 dataframes of
#'    data extracted from submitted Data Pack or Site Tool: a \code{d$data$MER}
#'    dataframe containing all MER data to be distributed to site level, and/or
#'    \code{d$data$SUBNAT_IMPATT} containing data in the SUBNAT and IMPATT
#'    datasets from DATIM that can be imported into DATIM at the PSNU level.
separateDataSets <- function(d) {
  d$data$MER <- d$data$targets %>%
    dplyr::filter(
      indicatorCode %in% (
        datapackr::template_schema %>%
          dplyr::filter(colType == "FY20 Target",
                        dataset == "MER") %>%
          dplyr::pull(indicatorCode)
      )
    )
  
  d$data$SUBNAT_IMPATT <- d$data$targets %>%
    dplyr::filter(
      indicatorCode %in% (
        datapackr::template_schema %>%
          dplyr::filter(colType == "FY20 Target",
                        dataset %in% c("SUBNAT", "IMPATT")) %>%
          dplyr::pull(indicatorCode)
      )
    )
  d$data <-
    rlist::list.remove(d$data, c("targets", "extract", "sheet"))
  
  return(d)
}


#' @importFrom magrittr %>% %<>%
#' @title unPackSheets(d)
#'
#' @description Loops through all critical sheets in a submitted Data Pack or
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
unPackSheets <- function(d) {
  # Get sheets list
  sheets <- datapackr::template_schema %>%
    dplyr::select(sheet_name) %>%
    dplyr::filter(sheet_name != "SNU x IM") %>%
    dplyr::distinct() %>%
    dplyr::pull(sheet_name)
  actual_sheets <-
    readxl::excel_sheets(d$keychain$submission_path)
  sheets_to_read <- actual_sheets[actual_sheets %in% sheets]
  
  d$data$targets <- NULL
  
  for (i in 1:length(sheets_to_read)) {
    d$data$sheet = sheets_to_read[i]
    interactive_print(d$data$sheet)
    d <- unPackSheet(d)
    d$data$targets <-
      dplyr::bind_rows(d$data$targets, d$data$extract)
  }
  
  d <- separateDataSets(d)
  
  return(d)
}


#' @title unPackSNUxIM(d)
#'
#' @description Looks inside submitted Data Pack to extract SNU x IM data from
#'     \code{SNU x IM} tab and restructure this to be ready for cross-
#'     pollination with PSNU-level MER data coming from
#'     \code{\link{unPackSheets}}. This data is also analyzed to identify
#'     structural or data anomalies and print any issues into running Warning
#'     Message queue.
#'
#' @param d datapackr list object containing at least
#'     \code{d$keychain$submission_path}.
#' @return A datapackr list object, \code{d}, storing at least 1 dataframe of
#'    data extracted from the \code{SNU x IM} tab in a submitted Data Pack or
#'    Site Tool, as well as a running Warning Message queue string,
#'    \code{d$info$warningMsg}
unPackSNUxIM <- function(d) {
  msg <- NULL
  
  col_num <- length(readxl::read_excel(path = d$keychain$submission_path,
                                       sheet = "SNU x IM",
                                       range = readxl::cell_rows(5)))
  ct <- c(rep("text",8),rep("numeric",col_num-8))
  
  d$data$SNUxIM <- readxl::read_excel(path = d$keychain$submission_path,
                                      sheet = "SNU x IM",
                                      range = readxl::cell_limits(c(5,1), c(NA, col_num)),
                                      col_types = ct) %>%
    dplyr::select(
      dplyr::one_of(
        c("PSNU","sheet_name","indicatorCode","CoarseAge","Sex","KeyPop","DataPackTarget","Dedupe")),
      dplyr::matches("(\\d){2,}")) %>%
    dplyr::select_if(~!all(is.na(.))) %>%
    dplyr::mutate(
      sum = rowSums(dplyr::select(.,dplyr::matches("\\d+|Dedupe")), na.rm = TRUE)) %>%
    dplyr::mutate(DataPackTarget = datapackr::round_trunc(DataPackTarget),
                  sum = datapackr::round_trunc(sum))
  
  # TEST where DataPackTarget != sum of mechanism values
  mismatch <- d$data$SNUxIM %>%
    dplyr::filter(DataPackTarget != sum) %>%
    dplyr::select(PSNU, indicatorCode, CoarseAge, Sex, KeyPop, DataPackTarget, mechanisms = sum)
  
  if (NROW(mismatch) > 0) {
    msg <- paste0(
      msg,
      "    ",
      NROW(mismatch),
      " cases where Data Pack Targets are not correctly distributed among mechanisms.
      "
    )
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  d$data$SNUxIM %<>%
    dplyr::mutate(
      # Align PMTCT_EID Age bands with rest of Data Pack
      CoarseAge = dplyr::case_when(
        stringr::str_detect(indicatorCode, "PMTCT_EID(.)+2to12mo") ~ "02 - 12 months",
        stringr::str_detect(indicatorCode, "PMTCT_EID(.)+2mo") ~ "<= 02 months",
        TRUE ~ CoarseAge),
      Sex = dplyr::case_when(
        # Drop Unknown Sex from PMTCT_EID
        stringr::str_detect(indicatorCode, "PMTCT_EID") ~ NA_character_,
        # Fix issues with HTS_SELF (duplicate and split by Male/Female)
        stringr::str_detect(indicatorCode, "HTS_SELF(.)+Unassisted") ~ "Male|Female",
        TRUE ~ Sex)) %>%
    tidyr::separate_rows(Sex, sep = "\\|") %>%
    # Create distribution matrix 
    dplyr::filter(DataPackTarget != 0 & sum != 0) %>%
    dplyr::select(-DataPackTarget, -sum) %>%
    tidyr::gather(
      key = "mechanismCode",
      value = "value",
      -PSNU, -sheet_name, -indicatorCode, -CoarseAge, -Sex, -KeyPop) %>%
    ## Drop all NA values
    tidyr::drop_na(value) %>%
    dplyr::group_by(PSNU, sheet_name, indicatorCode, CoarseAge, Sex, KeyPop) %>%
    dplyr::mutate(distribution = value / sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      psnuid = stringr::str_extract(PSNU,"(?<=\\()([A-Za-z][A-Za-z0-9]{10})(?=\\)$)"),
      mechanismCode = stringr::str_extract(mechanismCode, "(\\d{1,6})|Dedupe")) %>%
    dplyr::select(PSNU, psnuid, sheet_name, indicatorCode, CoarseAge, Sex,
                  KeyPop, mechanismCode, distribution)
  
  return(d)
  }

#' @importFrom magrittr %>% %<>%
#' @title rePackSNUxIM(d)
#'
#' @description Takes the output of the \code{\link{unPackSNUxIM}} and
#'     \code{\link{unPackSheets}} functions and delicatety combines these to create
#'     a single dataframe at the PSNU x IM level.
#'
#' @param d datapackr list object containing at least \code{d$keychain$MER} and
#'     \code{d$data$SNUxIM}
#' @return A datapackr list object, \code{d}, storing at least 1 dataframe of
#'    data at the SNU x IM level, \code{d$data$distributedMER}.
rePackPSNUxIM <- function(d) {
  d$data$distributedMER <- d$data$MER %>%
    dplyr::mutate(
      CoarseAge = dplyr::case_when(
        stringr::str_detect(indicatorCode,"OVC_SERV")
        & Age %in% c("<01","01-04","05-09","10-14","15-17") ~ "<18",
        stringr::str_detect(indicatorCode,"OVC_HIVSTAT") ~ NA_character_,
        stringr::str_detect(indicatorCode,"Malnutrition|Pediatric") ~ "01-04",
        stringr::str_detect(indicatorCode,"HTS_SELF(.)+Unassisted") ~ NA_character_,
        Age %in% c("<01","01-04","05-09","10-14") ~ "<15",
        Age %in% c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+") ~ "15+",
        TRUE ~ Age),
      Sex = dplyr::case_when(
        stringr::str_detect(indicatorCode,"OVC_HIVSTAT") ~ NA_character_,
        stringr::str_detect(indicatorCode,"KP_MAT") ~ stringr::str_replace(KeyPop," PWID",""),
        TRUE ~ Sex),
      KeyPop = dplyr::case_when(
        stringr::str_detect(indicatorCode,"KP_MAT") ~ NA_character_,
        TRUE ~ KeyPop)
    ) %>%
    dplyr::left_join(dplyr::select(d$data$SNUxIM,-PSNU)) %>%
    dplyr::mutate(newValue = value * distribution) %>%
    dplyr::select(PSNU, psnuid, sheet_name, indicatorCode, Age, CoarseAge,
                  Sex, KeyPop, mechanismCode, value = newValue) %>%
    dplyr::filter(value != 0) %>%
    tidyr::drop_na(value)
  
  return(d)
}

#' @importFrom magrittr %>% %<>%
#' @title FASTforward(d)
#'
#' @description Takes the output of the \code{\link{unPackSheets}} function and
#'     recompiles this to contain only data relevant for and in same structure
#'     as the PEPFAR FAST Tool.
#'
#' @param d datapackr list object containing at least
#'     \code{d$data$distributedMER}.
#' @return A datapackr list object, \code{d}, storing at least 1 dataframe of
#'    data structured for ingestion into the PEPFAR FAST Tool,
#'    \code{d$data$FAST}.
FASTforward <- function(d) {
  d$data$FAST <- d$data$distributedMER %>%
    dplyr::filter(
      # Detect HTS_TST & HTS_TST_POS cases
      stringr::str_detect(
        indicatorCode,
        "HTS_TST(.)+Age|(PMTCT|TB)_STAT\\.N(.)+(NewNeg|NewPos)$|VMMC_CIRC\\.(.)+(Negative|Positive)|HTS_INDEX"
      )
      # Detect OVC_SERV, TB_PREV, TX_CURR, TX_NEW, VMMC_CIRC cases
      |
        stringr::str_detect(
          indicatorCode,
          "OVC_SERV|TB_PREV\\.N|TX_CURR\\.|TX_NEW\\.N\\.Age|VMMC_CIRC\\."
        )
    ) %>%
    dplyr::mutate(indicator = NULL) %>%
    dplyr::bind_rows(
      .,
      ## Copy for HTS_TST
      ((.) %>%
         dplyr::filter(
           stringr::str_detect(indicatorCode, "VMMC_CIRC(.)+(Negative|Positive)")
         ) %>%
         dplyr::mutate(indicator = "HTS_TST")
      ),
      ## Copy for HTS_TST_POS
      ((.) %>%
         dplyr::filter(
           stringr::str_detect(
             indicatorCode,
             "(VMMC_CIRC|HTS_TST)(.)+Positive|(HTS_INDEX|PMTCT_STAT|TB_STAT)(.)+NewPos$"
           )
         ) %>%
         dplyr::mutate(indicator = "HTS_TST_POS")
      )
    ) %>%
    dplyr::mutate(
      indicator = dplyr::case_when(
        !is.na(indicator) ~ indicator,
        stringr::str_detect(indicatorCode, "PMTCT_STAT|TB_STAT|HTS_INDEX") ~ "HTS_TST",
        TRUE ~ stringr::str_extract(
          indicatorCode,
          "OVC_SERV|HTS_TST|TB_PREV|TX_CURR|TX_NEW|VMMC_CIRC"
        )
      ),
      disag = dplyr::case_when(
        indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW") &
          CoarseAge == "15+" & Sex == "Male" ~ "Adult Men",
        indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW") &
          CoarseAge == "15+" & Sex == "Female" ~ "Adult Women",
        indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW") &
          CoarseAge %in% c("<15", "01-04") ~ "Peds",
        TRUE ~ ""
      )
    ) %>%
    dplyr::select(mechanismid = mechanismCode, indicator, disag, value) %>%
    dplyr::group_by(mechanismid, indicator, disag) %>%
    dplyr::summarise(fy2020_targets = round_trunc(sum(value))) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na(mechanismid) %>%
    dplyr::arrange(mechanismid, indicator, disag)
  
  return(d)
}


#' @importFrom magrittr %>% %<>%
#' @title packSUBNAT_IMPATT(d)
#'
#' @description Takes the outputs of the \code{\link{unPackSheets}} function and
#'     recompiles the dataframe containing SUBNAT and IMPATT data,
#'     \code{d$data$SUBNAT_IMPATT} into a standard DATIM import file.
#'
#' @param d datapackr list object containing at least
#'     \code{d$data$SUBNAT_IMPATT}.
#' @return A datapackr list object, \code{d}, storing at least 1 dataframe of
#'    SUBNAT & IMPATT data (\code{d$datim$SUBNAT_IMPATT}) ready for DATIM
#'    ingestion.
packSUBNAT_IMPATT <- function(d) {
  d$datim$SUBNAT_IMPATT <- d$data$SUBNAT_IMPATT %>%
    dplyr::left_join((
      datapackr::indicatorMap %>%
        dplyr::filter(dataset %in% c("SUBNAT", "IMPATT")) %>%
        dplyr::select(
          sheet_name,
          indicatorCode,
          Age = validAges,
          Sex = validSexes,
          KeyPop = validKPs,
          dataelementuid,
          categoryoptioncombouid
        )
    )) %>%
    tidyr::drop_na(dataelementuid, categoryoptioncombouid, value) %>%
    dplyr::mutate(
      value = round_trunc(value),
      period = "2019Oct",
      attributeoptioncombo = "HllvX50cXC0"
    ) %>%
    dplyr::filter(value > 0) %>%
    dplyr::select(
      dataelement = dataelementuid,
      period,
      orgunit = psnuid,
      categoryoptioncombo = categoryoptioncombouid,
      attributeoptioncombo,
      value
    ) %>%
    dplyr::group_by(dataelement,
                    period,
                    orgunit,
                    categoryoptioncombo,
                    attributeoptioncombo) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = as.character(value))
  
  return(d)
}
