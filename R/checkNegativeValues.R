#' @export
#' @title Check for Negative Values.
#'
#' @description Checks if there are any negative values.
#'
#' @param d Datapackr object.
#' @param sheet Sheet to unpack.
#' @param quiet Logical. Should warning messages be printed? Default is TRUE.
#'
#' @return d
#'

checkNegativeValues <- function(d, sheet, quiet = T) {

  if (!quiet) {
    messages <- MessageQueue()
  }

  # get data ----
  if (sheet %in% c("SNU x IM", "PSNUxIM") &
      d$info$tool == "Data Pack") {
    data <- d$data$SNUxIM
  } else {
    data <- d$data$extract
  }

  # BELOW IS THE TRANSITION to LOAD DATAPACK ONCE THAT IS ADDED TO CREATEKEYCHAININFO
  # if (sheet %in% c("SNU x IM", "PSNUxIM") & d$info$tool == "Data Pack") {
  #
  #   data <- d$sheets[["PSNUxIM"]]
  # } else {
  #   data <- d$sheets[[as.character(sheet)]]
  # }

  # LIFTED OUT OF UNPACK SHEET----
  # List Target Columns ####
  target_cols <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet
                  & (col_type == "target" | (col_type == "result" & dataset == "subnat"))
                  # Filter by what's in submission to avoid unknown column warning messages
                  & indicator_code %in% colnames(data)) %>%
    dplyr::pull(indicator_code)

  if (NROW(target_cols) == 0) {
    data <- NULL
    return(d)
  }

  # Add cols to allow compiling with other sheets ####
  data %<>%
    addcols(c("KeyPop", "Age", "Sex")) %>%
    # Select only target-related columns
    dplyr::select(PSNU, Age, Sex, KeyPop,
                  dplyr::one_of(target_cols)) %>%
    # Drop rows where entire row is NA
    dplyr::filter_all(dplyr::any_vars(!is.na(.))) %>%
    # Extract PSNU uid
    dplyr::mutate(
      psnuid = stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)"),
      # Tag sheet name
      sheet_name = sheet
    ) %>%
    dplyr::select(PSNU, psnuid, sheet_name, Age, Sex, KeyPop,
                  dplyr::everything())

  # If PSNU has been deleted, drop the row ####
  data %<>%
    dplyr::filter(!is.na(PSNU))

  # Gather all indicators as single column for easier processing ####
  data %<>%
    tidyr::gather(key = "indicator_code",
                  value = "value",
                  -PSNU, -psnuid, -Age, -Sex, -KeyPop, -sheet_name) %>%
    dplyr::select(PSNU, psnuid, sheet_name, indicator_code, Age, Sex, KeyPop, value)

  # Drop NAs ####
  data %<>%
    tidyr::drop_na(value)

  # Now that non-numeric cases noted, convert all to numeric & drop non-numeric ####
  data %<>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
    tidyr::drop_na(value) %>%
    # Filter out zeros ####
  dplyr::filter(value != 0)

  # test ----
  negative_values <- data %>%
    dplyr::filter(value < 0)

  if (NROW(negative_values) > 0) {
    lvl <- "ERROR"

    msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": NEGATIVE VALUES found in the following columns! Ensure all values entered",
        " against Targets are whole, positive, numeric values. These will be removed. -> \n\t* ",
        paste(unique(d$tests$negative_values$indicator_code), collapse = "\n\t* "),
        "\n")

    d$tests$negative_values <- dplyr::bind_rows(d$test$negative_values, negative_values)
    attr(d$tests$negative_values, "test_name") <- "Negative values"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)
    d$info$has_error <- TRUE

    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }

  }

  if (!quiet) {
    printMessages(messages)
  }

  return(d)

}
