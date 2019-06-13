
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
    dplyr::mutate(DataPackTarget = round_trunc(DataPackTarget),
                  sum = round_trunc(sum))
  
  # TEST where DataPackTarget != sum of mechanism values
  mismatch <- d$data$SNUxIM %>%
    dplyr::filter(DataPackTarget != sum) %>%
    dplyr::select(PSNU, indicatorCode, CoarseAge, Sex, KeyPop, DataPackTarget, mechanisms = sum)
  
  if (NROW(mismatch) > 0) {
    msg <- paste0(
      msg,
      "    ",
      NROW(mismatch),
      " cases where Data Pack Targets are not correctly distributed among mechanisms. ",
      "To address this, go to your Data Pack's SNU x IM tab and filter the Rollup column for Pink cells. 
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
    ## Coerce to numeric
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
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
