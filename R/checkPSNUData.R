#' @export
#' @title Check PSNUxIM tab Data
#'
#' @description Tests PSNUxIM data
#'
#' @inheritParams datapackr_params
#'
#' @return d
#'
checkPSNUData  <-  function(d) {

  stopifnot("Cannot validate data for this COP year!" =
              d$info$cop_year %in% names(datapackr::cop_validation_rules))

  vr_rules <- datapackr::cop_validation_rules %>%
    purrr::pluck(., as.character(d$info$cop_year))

  vr_data <- d$data$analytics %>%
    #Ignore dedupe in validation rule analysis
    dplyr::filter(!(mechanism_code %in% c("00000", "00001"))) %>%
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

  if (is.null(vr_data) || NROW(vr_data) == 0) {
    return(d)
  }

    vr_data <- datimvalidation::prepDataForValidation(vr_data) %>%
      dplyr::select(-dataElement, -period, -categoryOptionCombo) %>%
      dplyr::group_by(orgUnit, attributeOptionCombo) %>%
      tidyr::nest()


    n_cores <- getMaxCores()
    #Evaluate the indicators in parallel if possible
    if (can_spawn() && n_cores > 1L) {
      vr_data$vr_results <-
        parallel::mclapply(vr_data$data, function(x) {
          datimvalidation::evaluateValidation(x$combi,
                                              x$value,
                                              vr = vr_rules,
                                              return_violations_only = FALSE)
        },
        mc.cores = n_cores)
    } else {
      vr_data$vr_results <-
        lapply(vr_data$data, function(x) {
          datimvalidation::evaluateValidation(x$combi,
                                              x$value,
                                              vr = vr_rules,
                                              return_violations_only = FALSE)
        })
    }

  #Unnest the data
  valid_Orgs <- getValidOrgUnits(d$info$cop_year) %>%
    dplyr::rename(psnu = name, psnu_uid = uid)

  vr_data <- vr_data %>%
    tidyr::unnest(vr_results) %>%
    dplyr::inner_join(valid_Orgs[, c("psnu", "psnu_uid")], by = c("orgUnit" = "psnu_uid")) %>%
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

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING", d$info$tool)
  }

  d
}
