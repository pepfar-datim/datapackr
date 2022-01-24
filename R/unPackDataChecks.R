#' Perform various data quality checks on in-process data from Data Packs.
#'
#' @description
#' A series of functions to check and validate quality & integrity
#' of data encountered in various `unPack...` functions across `datapackr`.
#' 
#' `checkDupeRows` checks for any rows with duplicates across PSNU and other key
#' disaggregates.
#'
#' @name unPackDataChecks
#' @md
#' @importFrom magrittr %>% %<>%
#'
#' @param data Dataset to be validated.
#' @param d Data Pack 
#' @param sheet Name of sheet from Data Pack from which `data` was extracted.
#' @inheritParams datapackr_params
#'
#' @return
#' These functions each return a list object, `t` containing three components:
#'   1. `message`: A message describing any issues encountered in the check.
#'   2. `test_result`: A tibble of data flagged by the check, useful in data
#'   quality forensics.
#'   3. `has_error`: A `TRUE/FALSE` flag indicating whether any fatal errors
#'   have been observed.
#'
NULL


#' @export
#' @rdname unPackDataChecks
#'
checkDupeRows <- function(data, tool, cop_year, sheet) {

  # Check/Fill in parameters ####
  params <- check_params(cop_year = cop_year,
                         tool = tool,
                         schema = NULL)

  for (p in names(params)) {
    assign(p, purrr::pluck(params, p))
  }

  rm(params, p)

  if (!sheet %in% schema$sheet_name) {
    stop("In checkDupeRows, sheet not valid!")
  }
  
  t <- list(
    test_result = NULL,
    warning_msg = NULL,
    has_error = FALSE
  )

  # Get header_cols ####
  header_cols <- schema %>%
    dplyr::filter(
      sheet_name == sheet,
      col_type == "row_header") %>%
    dplyr::pull(indicator_code) %>%
    c(., "mechCode_supportType")

  header_cols <- header_cols[header_cols %in% names(data)]

  # Drop rows/cols with all NAs or 0s ----
    # We don't care if these are duplicates
  names(data) <- data %>%
    names() %>%
    make.names() %>%
    make.unique()

  data %<>%
    #dplyr::select(header_cols, where(~ any(!is.na(.x)))) %>%
    { suppressWarnings(dplyr::mutate(., dplyr::across(-header_cols, #nolint
                                        as.numeric)))
    } %>%
    dplyr::mutate(dplyr::across(-header_cols, ~tidyr::replace_na(.x, 0))) %>%
    dplyr::filter(rowSums(dplyr::across(-header_cols)) != 0)

  data <- 
    dplyr::bind_cols(
      dplyr::select(data, header_cols),
      dplyr::select(data, -header_cols) %>%
        dplyr::select_if(colSums(.) != 0))

  # TEST for duplicates ####
  t$test_result <- data %>%
    #dplyr::select(header_cols) %>%
    dplyr::group_by(dplyr::across(header_cols)) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(-dplyr::any_of(c("n", "ID"))) %>%
    dplyr::arrange(dplyr::across()) %>%
    dplyr::mutate(sheet = sheet) %>%
    dplyr::select(sheet, dplyr::everything())
  
  if (NROW(t$test_result) > 0) {
    
    attr(t$test_result, "test_name") <- "Duplicated rows"
    
    dupes_msg <-
      capture.output(
        print(as.data.frame(t$test_result), row.names = FALSE)
      )
    
    t$warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": DUPLICATE ROWS found. Ensure PSNUs or Age, Sex, KeyPop disaggregates",
        " are not repeated within tabs. This issue may have been caused by inadvertent",
        " or incorrect copying of data from one row to another. -> \n\t",
        paste(dupes_msg, collapse = "\n\t"),
        "\n")
    
    t$has_error <- TRUE
    
  }
  
  t
  
}
