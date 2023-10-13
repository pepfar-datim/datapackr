#' Title
#'
#' @inheritParams datapackr_params
#' @param cols Name of the columns to extract. PSNU is mandatory
#' @description In certain situations in the checkAnalytics function, we need to extract raw
#' data from the DataPack sheets for use in calculations. This
#' utility function will help to extract raw data from the excel sheets
#' given a vector of columns to extract.
#'
#' @return A data frame consisting of psnu, psnu_uid, age, sex, key_population
#' indicator_code and value
#'
extractRawColumnData <- function(d, sheet, cols) {

  if (is.null(sheet) || !(sheet %in% names(d$sheets))) {
    warning(paste("That sheet could not be found: ", sheet))
    return(NULL)
  }

  if (!("PSNU" %in% cols)) {
    cols <- c("PSNU", cols)
  }

  if (is.null(d$sheets[sheet])) {
    d <- loadSheets(d, sheet)
  }

valid_orgunits_local <- getValidOrgUnits(d$info$cop_year) %>%
  dplyr::select(psnu = name,
                psnu_uid = uid)

 extract <-  d$sheets %>%
    purrr::pluck(sheet) %>%
    dplyr::select(tidyselect::all_of(cols)) %>%
    dplyr::mutate(psnu_uid = stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)")) %>%
    dplyr::select(-PSNU) %>%
    dplyr::left_join(valid_orgunits_local, by = "psnu_uid")


 extract <- addcols(extract, c("Age", "Sex", "KeyPop"))

 extract <- extract %>%
   dplyr::rename("age" = "Age",
                 "sex" = "Sex",
                 "key_population" = "KeyPop")

 tidyr::pivot_longer(extract, cols = -tidyselect::all_of(c("psnu", "psnu_uid", "age", "sex", "key_population")),
                                                         names_to = "indicator_code",
                                                         values_to = "value") %>%
   dplyr::select(psnu, psnu_uid, indicator_code, age, sex, key_population, value) %>%
   dplyr::mutate(value = as.numeric(value))

}
