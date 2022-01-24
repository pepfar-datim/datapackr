#' @export
#' @title Check PSNUxIM tab Data
#'
#' @description Tests PSNUxIM data
#'
#' @inheritParams datapackr_params
#'
#' @return d
#'
checkPSNUData  <-  function(d,
                            validation_rules_path =
                              paste0(Sys.getenv("support_files_directory"), "cop_validation_rules.rds"),
                            d2_session = dynGet("d2_default_session",
                                                inherits = TRUE)) {

  # Checks that Validation Rules file can be found and read
  print(validation_rules_path)
  can_read_file <- file.access(validation_rules_path, 4) == 0

  # If Validation Rules file can be found, retrieve it
  if (can_read_file) {
    vr_rules <- readRDS(validation_rules_path) %>%
      purrr::pluck(., as.character(d$info$cop_year))
  } else {
    stop("Cannot find validation rules file!")
  }

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

  # We need ALL mechanisms to be in DATIM before remapping....TODO
  vr_data$attributeOptionCombo  <-
    datimvalidation::remapMechs(vr_data$attributeOptionCombo,
                                d2_session$user_orgunit,
                                "code",
                                "id",
                                d2session = d2_session)

  # TODO Can this be replaced with a call to `getDatasetUids`?
  datasets_uid  <-
    if (d$info$cop_year == "2020") {
      c("Pmc0yYAIi1t", "s1sxJuqXsvV")
    } else if  (d$info$cop_year == "2021") {
      c("YfZot37BbTm", "Pmc0yYAIi1t") #TODO...why do we have last years dataset here?

    } else if (d$info$cop_year == "2022") {
      c("iADcaCD5YXh")
    }

  if (Sys.info()["sysname"] == "Linux") {
    ncores  <-  parallel::detectCores() - 1
    doMC::registerDoMC(cores = ncores)
    is_parallel  <-  TRUE
  } else {
    is_parallel  <-  FALSE
  }

  vr_violations <-
    datimvalidation::validateData(vr_data,
                                  parallel = is_parallel,
                                  return_violations_only = FALSE,
                                  vr = vr_rules,
                                  d2session = d2_session)

  if (NROW(vr_violations) > 0) {

    diff  <-  gsub(" [<>]= ", "/", vr_violations$formula)
    vr_violations$diff <-
      sapply(diff, function(x) round((eval(parse(text = x)) - 1) * 100, 2))
    vr_violations$diff <-
      ifelse(vr_violations$rightSide.expression == 0 |
               vr_violations$leftSide.expression == 0,
             NA,
             vr_violations$diff)

    diff  <-  gsub(" [<>]= ", "-", vr_violations$formula)
    vr_violations$abs_diff <-
      sapply(diff, function(x) {
        abs(eval(parse(text = x)))
      })

    d$tests$vr_rules_check  <-  vr_violations  %>%
      dplyr::select(name, ou_name, mech_code, formula, diff, abs_diff) %>%
      dplyr::rename("Validation rule" = name,
                    "PSNU" = ou_name,
                    "Mechanism" = mech_code,
                    "Formula" = formula,
                    "Diff (%)" = diff,
                    "Diff (Absolute)" = abs_diff)

    attr(d$tests$vr_rules_check, "test_name") <- "Validation rule violations"

    warning_msg <- paste("WARNING: ", NROW(vr_violations),
                         "validation rule issues found in",
                         d$info$datapack_name, "DataPack.\n")
    d$tests$vr_rules_check <-
      d$info$messages <- appendMessage(d$info$messages, warning_msg, "Warning")
    d$info$had_error <- TRUE
  }

  return(d)
}
