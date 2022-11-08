checkPSNUxIM_Disaggs <- function(d, sheet) {

  sheet_disaggs <- d$data$SNUxIM %>%
    dplyr::select(indicator_code,Age,Sex,KeyPop)

  schema_disaggs <- getMapDataPack_DATIM_DEs_COCs(d$info$cop_year,
                                                  datasource = d$info$tool) %>%
    dplyr::select(indicator_code,
                  Age = valid_ages.name,
                  Sex = valid_sexes.name,
                  KeyPop = valid_kps.name) %>%
    dplyr::distinct() %>%
    dplyr::mutate(is_valid = TRUE)

  disagg_check <- dplyr::left_join(sheet_disaggs, schema_disaggs) %>%
    dplyr::mutate(rownum = dplyr::row_number())

  disagg_check_idx <- which(is.na(disagg_check$is_valid))

  # Alert to missing cols ####
  if (length(disagg_check_idx > 0)) {

    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ", Invalid disaggregates: You have used an incorrect combination",
        "of disaggregates (Age, Sex, KeyPops).",
        "The following rows are affected: ", formatSetStrings(disagg_check_idx))

    d$tests$bad_disaggs_psnuxim <- disagg_check %>%
      dplyr::filter(is.na(is_valid)) %>%
      dplyr::select(-is_valid)
    attr(d$tests$bad_disaggs_psnuxim, "test_name") <- "Invalid PSNUXIM Disaggs"

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    d$info$has_error <- TRUE
  }

  d

}
checkDisaggs <- function(d, sheet) {

  if (sheet == "PSNUxIM") {

   d <-  checkPSNUxIM_Disaggs(d, sheet)

   }
  d
}
