#' @export
#' @title Check PSNUxIM tab Data
#'
#' @description Tests PSNUxIM data
#'
#' @inheritParams datapackr_params
#'
#' @return d
#'
checkPSNUData  <-  function(d, d2_session = dynGet("d2_default_session",
                                                   inherits = TRUE)) {

  stopifnot("Cannot validate data for this COP year!" =
              d$info$cop_year %in% names(datapackr::cop_validation_rules))

  vr_rules <- datapackr::cop_validation_rules %>%
    purrr::pluck(., as.character(d$info$cop_year))

  vr_data <- d$data$analytics %>%
    dplyr::select(
      dataElement = dataelement_id,
      period = fiscal_year,
      orgUnit = psnu_uid,
      categoryOptionCombo = categoryoptioncombo_id,
      attributeOptionCombo = mechanism_code,
      value = target_value
    ) %>%
    dplyr::mutate(period = paste0((as.numeric(period) - 1), "Oct")) %>%
    dplyr::mutate(attributeOptionCombo = dplyr::case_when(
      is.na(attributeOptionCombo) ~ datapackr::default_catOptCombo(),
      attributeOptionCombo == "default" ~ datapackr::default_catOptCombo(),
      TRUE ~ attributeOptionCombo
    ))

  if (is.null(vr_data) | NROW(vr_data) == 0) {
    return(d)
  }

    vr_data <- datimvalidation::prepDataForValidation(vr_data) %>%
      dplyr::select(-dataElement,-period,-categoryOptionCombo) %>%
      dplyr::group_by(orgUnit,attributeOptionCombo) %>%
      tidyr::nest()


    #Evaluate the indicators in parallel if possible
    if ("parallel" %in% rownames(installed.packages()) == TRUE) {
      vr_data$vr_results <-
        parallel::mclapply(vr_data$data, function(x)
          datimvalidation::evaluateValidation(x$combi,
          x$value, vr = vr_rules,return_violations_only = FALSE),
          mc.cores = parallel::detectCores())
    } else {
      vr_data$vr_results <-
        lapply(vr_data$data, function(x)
          datimvalidation::evaluateValidation(x$combi, x$value, vr = vr_rules ,return_violations_only = FALSE))
    }

  #Unnest the data
  vr_data <- vr_data %>%
    tidyr::unnest(vr_results) %>%
    dplyr::inner_join(valid_PSNUs[,c("psnu","psnu_uid")], by = c("orgUnit" = "psnu_uid")) %>%
    dplyr::ungroup()


  if (sum(!vr_data$result) > 0) {

    diff  <-  gsub(" [<>]= ", "/", vr_data$formula)
    vr_data$diff <-
      sapply(diff, function(x) round((eval(parse(text = x)) - 1) * 100, 2))
    vr_data$diff <-
      ifelse(vr_data$rightSide.expression == 0 |
               vr_data$leftSide.expression == 0,
             NA,
             vr_data$diff)

    diff  <-  gsub(" [<>]= ", "-", vr_data$formula)
    vr_data$abs_diff <-
      sapply(diff, function(x) {
        abs(eval(parse(text = x)))
      })

    d$tests$vr_rules_check  <-  vr_data  %>%
      dplyr::select("Validation rule" = name,
                    "PSNU" = psnu,
                    "Mechanism" = attributeOptionCombo,
                    "Formula" = formula,
                    "Diff (%)" = diff,
                    "Diff (Absolute)" = abs_diff,
                    "Valid" = result)

    attr(d$tests$vr_rules_check, "test_name") <- "Validation rule violations"

    warning_msg <- paste("WARNING: ", sum(!d$tests$vr_rules_check$Valid),
                         "validation rule issues found in",
                         d$info$datapack_name, "DataPack.\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
  }

  d
}