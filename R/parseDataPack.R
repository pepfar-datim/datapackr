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
  sheets_check <- datapackr::data_pack_schema %>%
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
    
    d$info$warningMsg <- append(msg,d$info$warningMsg)
  }
  
  return(d)
  
  }




#' @importFrom magrittr %>% %<>%
#' @title unPackDataPackSheet(d)
#'
#' @description Within a submitted Data Pack or Site Tool (directed to by
#'    \code{d$keychain$submission_path}), extract data from a single sheet specified
#'    in \code{d$data$sheet}.
#'
#' @param d Datapackr object
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
  
  # Run structural checks
  d <- checkColStructure(d, sheet)
  
  # List Target Columns
  targetCols <- datapackr::data_pack_schema %>%
    dplyr::filter(sheet_name == sheet,
                  col_type == "Target") %>%
    dplyr::pull(indicator_code)
  
  # Add cols to allow compiling with other sheets
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
                  dplyr::one_of(targetCols))
  
  
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
  # Drop zeros & NAs
    tidyr::drop_na(value) %>%
    dplyr::filter(value != 0)
  
  # TEST for non-numeric entries
  non_numeric <- d$data$extract %>%
    dplyr::mutate(value_numeric = as.numeric(value)) %>%
    dplyr::filter(is.na(value_numeric)) %>%
    dplyr::select(value)
  
  if(NROW(non_numeric) > 0) {
    msg <- paste0("In tab ", sheet, ": NON-NUMERIC VALUES found! -> ",
                  non_numeric)
    d$info$warning_msg <- append(msg, d$info$warning_msg)
    
  }
  
  d$data$extract %<>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
    tidyr::drop_na(value)
  
  # TEST for Negative values
  has_negative_numbers <- d$data$extract$value < 0
  
  if (any(has_negative_numbers)) {
    neg_cols <- d$data$extract %>%
      dplyr::filter(value < 0) %>%
      dplyr::pull(indicator_code) %>%
      unique() %>%
      paste(collapse = ", ")
    
    msg <- paste0("ERROR! In tab ", sheet,
                  ": NEGATIVE VALUES found! -> ",
                  neg_cols,
                  "")
    d$info$warning_msg <- append(msg, d$info$warning_msg)
    d$info$has_error <- TRUE
  }

  # TEST for duplicates
  any_dups <- d$data$extract %>%
    dplyr::select(sheet_name, PSNU, Age, Sex, KeyPop, indicator_code) %>%
    dplyr::group_by(sheet_name, PSNU, Age, Sex, KeyPop, indicator_code) %>%
    dplyr::summarise(n = (dplyr::n())) %>%
    dplyr::filter(n > 1) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::mutate(row_id = paste(PSNU, Age, Sex, KeyPop, indicator_code, sep = "    ")) %>%
    dplyr::arrange(row_id) %>%
    dplyr::pull(row_id)
  
  if (length(any_dups) > 0) {
    msg <- paste0("In tab ", sheet, ": ",
                  length(any_dups),
                  " DUPLICATE ROWS. These will be aggregated! -> ", 
                 paste(any_dups, collapse = ","))
    d$info$warning_msg <- append(msg, d$info$warning_msg)
  }

  # TEST for defunct disaggs
  defunct <- defunctDisaggs(d)
  
  if (NROW(defunct) > 0) {
    defunctMsg <- defunct %>%
      dplyr::mutate(
        msg = stringr::str_squish(
          paste(paste0(indicator_code, ":"), Age, Sex, KeyPop)
        )
      ) %>%
      dplyr::pull(msg) %>%
      paste(collapse = ",")
    
    msg <- paste0("ERROR! In tab ", sheet,
                  ": INVALID DISAGGS ",
                  "(Check MER Guidance for correct alternatives) ->",
                  defunctMsg)
    
    d$info$warning_msg <- append(msg, d$info$warning_msg)
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
      indicator_code %in% (
        datapackr::data_pack_schema %>%
          dplyr::filter(col_type == "Target",
                        dataset == "MER") %>%
          dplyr::pull(indicator_code)
      )
    )
  
  d$data$SUBNAT_IMPATT <- d$data$targets %>%
    dplyr::filter(
      indicator_code %in% (
        datapackr::data_pack_schema %>%
          dplyr::filter(col_type == "Target",
                        dataset %in% c("SUBNAT", "IMPATT")) %>%
          dplyr::pull(indicator_code)
      )
    )
  d$data <-
    rlist::list.remove(d$data, c("targets", "extract"))
  
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
  
  col_num <- readxl::read_excel(path = d$keychain$submission_path,
                                sheet = "SNU x IM",
                                range = readxl::cell_rows(5)) %>%
    length()
  
  ct <- c(rep("text",8),rep("numeric",col_num-8))
  
  d$data$SNUxIM <- readxl::read_excel(path = d$keychain$submission_path,
                                      sheet = "SNU x IM",
                                      range = readxl::cell_limits(c(5,1), c(NA, col_num)),
                                      col_types = ct) %>%
    dplyr::rename(indicator_code = indicatorCode) %>%
    dplyr::select(
      dplyr::one_of(
        c("PSNU","sheet_name","indicator_code","CoarseAge","Sex","KeyPop","DataPackTarget","Dedupe")),
      dplyr::matches("(\\d){2,}")) %>%
    dplyr::select_if(~!all(is.na(.))) %>%
    dplyr::mutate(
      sum = rowSums(dplyr::select(.,dplyr::matches("\\d+|Dedupe")), na.rm = TRUE)) %>%
    dplyr::mutate(DataPackTarget = round_trunc(DataPackTarget),
                  sum = round_trunc(sum))
  
  # TEST where DataPackTarget != sum of mechanism values
  mismatch <- d$data$SNUxIM %>%
    dplyr::filter(DataPackTarget != sum) %>%
    dplyr::select(PSNU, indicator_code, CoarseAge, Sex, KeyPop, DataPackTarget, mechanisms = sum)
  
  if (NROW(mismatch) > 0) {
    msg <- paste0(
      msg,
      "    ",
      NROW(mismatch),
      " cases where Data Pack Targets are not correctly distributed among mechanisms. ",
      "To address this, go to your Data Pack's SNU x IM tab and filter the Rollup column for Pink cells. 
      "
    )
    d$info$warning_msg <- append(msg,d$info$warning_msg)
  }
  
  d$data$SNUxIM %<>%
    dplyr::mutate(
      # Align PMTCT_EID Age bands with rest of Data Pack
      CoarseAge = dplyr::case_when(
        stringr::str_detect(indicator_code, "PMTCT_EID(.)+2to12mo") ~ "02 - 12 months",
        stringr::str_detect(indicator_code, "PMTCT_EID(.)+2mo") ~ "<= 02 months",
        TRUE ~ CoarseAge),
      Sex = dplyr::case_when(
        # Drop Unknown Sex from PMTCT_EID
        stringr::str_detect(indicator_code, "PMTCT_EID") ~ NA_character_,
        # Fix issues with HTS_SELF (duplicate and split by Male/Female)
        stringr::str_detect(indicator_code, "HTS_SELF(.)+Unassisted") ~ "Male|Female",
        TRUE ~ Sex)) %>%
    tidyr::separate_rows(Sex, sep = "\\|") %>%
    # Create distribution matrix 
    dplyr::filter(DataPackTarget != 0 & sum != 0) %>%
    dplyr::select(-DataPackTarget, -sum) %>%
    tidyr::gather(
      key = "mechanismCode",
      value = "value",
      -PSNU, -sheet_name, -indicator_code, -CoarseAge, -Sex, -KeyPop) %>%
    ## Coerce to numeric
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
    ## Drop all NA values
    tidyr::drop_na(value) %>%
    dplyr::group_by(PSNU, sheet_name, indicator_code, CoarseAge, Sex, KeyPop) %>%
    dplyr::mutate(distribution = value / sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      psnuid = stringr::str_extract(PSNU, "(?<=\\()([A-Za-z][A-Za-z0-9]{10})(?=\\)$)"),
      mechanismCode = stringr::str_extract(mechanismCode, "(\\d{1,6})|Dedupe")) %>%
    dplyr::select(PSNU, psnuid, sheet_name, indicator_code, CoarseAge, Sex,
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
        stringr::str_detect(indicator_code,"OVC_SERV")
          & Age %in% c("<01","01-04","05-09","10-14","15-17") ~ "<18",
        stringr::str_detect(indicator_code,"OVC_HIVSTAT") ~ NA_character_, #TODO: Fix in Data Pack, not here
        stringr::str_detect(indicator_code,"Malnutrition|Pediatric") ~ "01-04", #TODO: Fix in Data Pack, not here
        stringr::str_detect(indicator_code,"HTS_SELF(.)+Unassisted") ~ NA_character_, #TODO: Fix in Data Pack, not here
        Age %in% c("<01","01-04","05-09","10-14") ~ "<15",
        Age %in% c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+") ~ "15+",
        TRUE ~ Age),
      Sex = dplyr::case_when(
        stringr::str_detect(indicator_code,"OVC_HIVSTAT") ~ NA_character_, #TODO: Fix in Data Pack, not here
        stringr::str_detect(indicator_code,"KP_MAT") ~ stringr::str_replace(KeyPop," PWID",""), #TODO: Fix in Data Pack, not here
        TRUE ~ Sex),
      KeyPop = dplyr::case_when(
        stringr::str_detect(indicator_code,"KP_MAT") ~ NA_character_, #TODO: Fix in Data Pack, not here
        TRUE ~ KeyPop)
    ) %>%
    dplyr::left_join(dplyr::select(d$data$SNUxIM,-PSNU)) %>%
    dplyr::mutate(newValue = value * distribution) %>%
    dplyr::select(PSNU, psnuid, sheet_name, indicator_code, Age, CoarseAge,
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
        indicator_code,
        "HTS_TST(.)+Age|(PMTCT|TB)_STAT\\.N(.)+(NewNeg|NewPos)$|VMMC_CIRC\\.(.)+(Negative|Positive)|HTS_INDEX"
      )
      # Detect OVC_SERV, TB_PREV, TX_CURR, TX_NEW, VMMC_CIRC cases
      |
        stringr::str_detect(
          indicator_code,
          "OVC_SERV|TB_PREV\\.N|TX_CURR\\.|TX_NEW\\.N\\.Age|VMMC_CIRC\\."
        )
    ) %>%
    dplyr::mutate(indicator = NULL) %>%
    dplyr::bind_rows(
      .,
      ## Copy for HTS_TST
      ((.) %>%
         dplyr::filter(
           stringr::str_detect(indicator_code, "VMMC_CIRC(.)+(Negative|Positive)")
         ) %>%
         dplyr::mutate(indicator = "HTS_TST")
      ),
      ## Copy for HTS_TST_POS
      ((.) %>%
         dplyr::filter(
           stringr::str_detect(
             indicator_code,
             "(VMMC_CIRC|HTS_TST)(.)+Positive|(HTS_INDEX|PMTCT_STAT|TB_STAT)(.)+NewPos$"
           )
         ) %>%
         dplyr::mutate(indicator = "HTS_TST_POS")
      )
    ) %>%
    dplyr::mutate(
      indicator = dplyr::case_when(
        !is.na(indicator) ~ indicator,
        stringr::str_detect(indicator_code, "PMTCT_STAT|TB_STAT|HTS_INDEX") ~ "HTS_TST",
        TRUE ~ stringr::str_extract(
          indicator_code,
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
#' @param data SUBNAT/IMPATT dataframe to pack for DATIM.
#' 
#' @return Dataframe of SUBNAT & IMPATT data ready for DATIM ingestion.
#' 
packSUBNAT_IMPATT <- function(data) {
  
  SUBNAT_IMPATT <- data %>%
    dplyr::left_join((
      datapackr::indicatorMap %>%
        dplyr::filter(dataset %in% c("SUBNAT", "IMPATT")) %>%
        dplyr::rename(indicator_code = indicatorCode) %>%
        dplyr::select(
          sheet_name,
          indicator_code,
          Age = validAges,
          Sex = validSexes,
          KeyPop = validKPs,
          dataelementuid,
          categoryoptioncombouid
        )
    )) %>%
    tidyr::drop_na(dataelementuid, categoryoptioncombouid, value) %>%
    dplyr::mutate(
      period = datapackr::periodInfo$iso,
      attributeOptionCombo = datapackr::default_catOptCombo()
    ) %>%
    dplyr::filter(value > 0) %>%
    dplyr::select(
      dataElement = dataelementuid,
      period,
      orgUnit = psnuid,
      categoryOptionCombo = categoryoptioncombouid,
      attributeOptionCombo,
      value
    ) %>%
    dplyr::group_by(dataElement,
                    period,
                    orgUnit,
                    categoryOptionCombo,
                    attributeOptionCombo) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = as.character(round_trunc(value)))
  
  return(SUBNAT_IMPATT)
}
