#' Standardized Schema Checks
#'
#' @description Standardized package functions,in terms of schema checks.
#' These can be run individually (e.g., `checkSchema_SheetNums`), or in
#' bulk (e.g., `checkSchema(template_path = "my/file/path", cop_year = 2022, tool = "Data Pack")`).
#'
#' @name schema-validations
#' @md
#'
#' @inheritParams datapackr_params
#'
#' @return
#' For lower-level functions, a list of instances of failed tests. For the
#' higher-level `checkSchema`, a list object of lists of failed tests.
#' * `checkSchema_SkippedSheets`:
#' * `checkSchema_SheetNums`:
#' * `checkSchema_SheetNames`:
#' * `checkSchema_InvalidDatasets`:
#' * `checkSchema_InvalidColType`:
#' * `checkSchema_InvalidValueType`:
#' * `checkSchema_DSDSyntax`:
#' * `checkSchema_TASyntax`:
#' * `checkSchema_COsSyntax` :
#' * `checkSchema_ValidAges` :
#' * `checkSchema_ValidSexes` :
#' * `checkSchema_ValidKPs` :
#' * `checkSchema_Formulas` :
#' * `checkSchema` :
#' * `unPackSchema` :
#'
#' @family schema-helpers
NULL


getSkipSheets <- function(schema, tool, cop_year) {
  #Skip sheets which are defined in the code
  package_skip <- skip_tabs(tool = tool, cop_year = cop_year)

  #Skip sheets which are defined in the schema
  schema_skip <- schema %>%
    dplyr::filter(sheet_name %in% package_skip$schema) %>%
    dplyr::select(sheet_name, sheet_num) %>%
    dplyr::distinct()

   list(
    package_skip = package_skip$schema,
    num = schema_skip$sheet_num,
    names = schema_skip$sheet_name)
}

multi_uid_pattern <- function() {
  tag <- "(\\{(EID|KP)\\})"
  delim <- "\\."
  sep <- "/"
  single <- paste0(uid_pattern(), "(", delim, uid_pattern(), ")*")
  multi <- paste0(single, "(", sep, single, ")*")
  tag_block <- paste0(tag, "?", multi)
  paste0("^",
            "(", tag_block, ")+",
         "$")
}



#' @rdname schema-validations
checkSchema_SkippedSheets <- function(schema, tool, cop_year) {

  skip_sheets <- getSkipSheets(schema, tool, cop_year)

  skip_comparison <- waldo::compare(skip_sheets$names, skip_sheets$package_skip,
                                    x_arg = "schema", y_arg = "package")

  if (length(skip_comparison) != 0) {
    skipped_sheets <- list(
      error = length(skip_comparison) != 0,
      data = skip_comparison
    )
  } else {
    skipped_sheets <- list()
  }

  skipped_sheets
}

#' @rdname schema-validations
checkSchema_SheetNums <- function(schema) {
  observed_sheet_nums <- unique(schema$sheet_num)
  expected_sheet_nums <- c(1:max(schema$sheet_num))

  sheet_nums_comparison <- waldo::compare(observed_sheet_nums,
                                          expected_sheet_nums,
                                          x_arg = "observed",
                                          y_arg = "expected")

  if (length(sheet_nums_comparison) != 0) {
    sheet_nums_complete <- list(
      error = length(sheet_nums_comparison) != 0,
      data = sheet_nums_comparison)
  } else {
    sheet_nums_complete <- list()
  }

  sheet_nums_complete
}

#' @rdname schema-validations
#' @param filepath_schema Path to the schema file.
checkSchema_SheetNames <- function(schema, filepath_schema) {
  observed_sheet_names <- unique(schema$sheet_name)
  expected_sheet_names <- unique(filepath_schema$sheet_name)

  sheet_names_comparison <- waldo::compare(observed_sheet_names,
                                           expected_sheet_names,
                                           x_arg = "observed",
                                           y_arg = "expected")

  if (length(sheet_names_comparison) != 0) {
    sheet_names_complete <- list(
      error = length(sheet_names_comparison) != 0,
      data = sheet_names_comparison)
  } else {
    sheet_names_complete <- list()
  }

  sheet_names_complete
}

#' @rdname schema-validations
checkSchema_InvalidDatasets <- function(schema, tool, cop_year) {

  skip_sheets <- getSkipSheets(schema, tool, cop_year)

  datasets_invalid <- schema %>%
    dplyr::mutate(
      invalid_dataset =
        dplyr::case_when(
          col_type %in% c("reference", "assumption", "calculation", "row_header", "allocation")
          ~ !dataset == c("datapack"),
          col_type %in% c("target", "past", "result") ~ !dataset %in% c("mer", "impatt", "subnat"),
          sheet_name %in% skip_sheets$names ~ !is.na(dataset),
          TRUE ~ TRUE)) %>%
    dplyr::filter(invalid_dataset == TRUE) %>%
    dplyr::select(sheet_name, data_structure, col, indicator_code, dataset, col_type)

  datasets_invalid
}

#' @rdname schema-validations
checkSchema_InvalidColType <- function(schema, tool, cop_year) {

  skip_sheets <- getSkipSheets(schema, tool, cop_year)

  col_type_invalid <- schema %>%
    dplyr::filter(!(sheet_name %in% skip_sheets$names)) %>%
    dplyr::filter(!is.na(col_type)) %>%
    dplyr::mutate(
      invalid_col_type =
        (!col_type %in% c("target", "reference", "assumption", "calculation", "past",
                          "row_header", "allocation", "result"))) %>%
    dplyr::filter(invalid_col_type == TRUE) %>%
    dplyr::select(sheet_name, col, indicator_code, data_structure, col_type)

  col_type_invalid
}

#' @rdname schema-validations
checkSchema_InvalidValueType <- function(schema, tool, cop_year) {

  skip_sheets <- getSkipSheets(schema, tool, cop_year)

  value_type_invalid <- schema %>%
    dplyr::filter(!(sheet_name %in% skip_sheets$names)) %>%
    dplyr::filter(!is.na(value_type)) %>%
      dplyr::mutate(
      invalid_value_type = !(value_type %in% c("integer", "percentage", "string"))) %>%
    dplyr::filter(invalid_value_type == TRUE) %>%
    dplyr::select(sheet_name, col, indicator_code, value_type)

  value_type_invalid
}

#' @rdname schema-validations
checkSchema_DataElementSyntax <- function(schema) {

  schema %>%
    dplyr::filter(col_type %in% c("past", "target", "result")) %>%
    dplyr::select(sheet_name, col, indicator_code, dataset, dataelement_dsd, dataelement_ta) %>%
    dplyr::mutate(
      invalid_DSD_DEs =
        dplyr::if_else(
          sheet_name == "PSNUxIM", dataelement_dsd != "NA",
          !stringr::str_detect(dataelement_dsd, multi_uid_pattern())),
      invalid_TA_DEs =
        dplyr::if_else(
          sheet_name == "PSNUxIM", dataelement_ta != "NA",
          !stringr::str_detect(dataelement_ta, multi_uid_pattern()))) %>%
    dplyr::filter(sum(invalid_DSD_DEs, invalid_TA_DEs, na.rm = TRUE) > 0)

}


#' @rdname schema-validations
checkSchema_COsSyntax <- function(schema) {

  schema %>%
    dplyr::filter(col_type %in% c("past", "target", "result")) %>%
    dplyr::select(sheet_name, col, indicator_code, categoryoption_specified) %>%
    dplyr::mutate(
      invalid_COs =
        dplyr::if_else(
          sheet_name == "PSNUxIM", categoryoption_specified != "NA",
          !stringr::str_detect(categoryoption_specified, multi_uid_pattern()))) %>%
    dplyr::filter(invalid_COs == TRUE)

}

#' @rdname schema-validations
checkSchema_ValidAges <- function(schema) {

  valid_age_pattern <- "[0-9]{2}-[0-9]{2}|<01|<1[58]|1[58]+|65\\+|50\\+"

    schema %>%
    dplyr::select(sheet_name, col, indicator_code, valid_ages) %>%
    tidyr::unnest(valid_ages) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(invalid_age_uid = !stringr::str_detect(id, multi_uid_pattern())) %>%
    dplyr::mutate(invalid_age_string = !stringr::str_detect(name, valid_age_pattern)) %>%
    dplyr::filter(invalid_age_string + invalid_age_uid > 0) %>%
    dplyr::select(sheet_name, col, indicator_code, name, id)


}

#' @rdname schema-validations
checkSchema_ValidSexes <- function(schema) {

  valid_sex_pattern <- "^(Male|Female)$"
  #What about this instead?
  # unique(map_DataPack_DATIM_DEs_COCs$valid_sexes.name)

  schema %>%
    dplyr::select(sheet_name, col, indicator_code, valid_sexes) %>%
    tidyr::unnest(valid_sexes) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(invalid_sex_uid = !stringr::str_detect(id, multi_uid_pattern())) %>%
    dplyr::mutate(invalid_sex_string = !stringr::str_detect(name, valid_sex_pattern)) %>%
    dplyr::filter(invalid_sex_string + invalid_sex_uid > 0) %>%
    dplyr::select(sheet_name, col, indicator_code, name, id)
}

#' @rdname schema-validations
checkSchema_ValidKPs <- function(schema) {
  #Instead of this repetitive code.
  valid_KP_names <- unique(map_DataPack_DATIM_DEs_COCs$valid_kps.name)

  schema %>%
    dplyr::select(sheet_name, col, indicator_code, valid_kps) %>%
    tidyr::unnest(valid_kps) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(invalid_kp_uid = !stringr::str_detect(id, multi_uid_pattern())) %>%
    dplyr::mutate(invalid_kp_string = !(name %in% valid_KP_names)) %>%
    dplyr::filter(invalid_kp_string + invalid_kp_uid > 0) %>%
    dplyr::select(sheet_name, col, indicator_code, name, id)

}

#' @rdname schema-validations
checkSchema_Formulas <- function(schema) {
  fxs_ref_error <- schema %>%
    dplyr::mutate(
      ref_error_fxs = stringr::str_detect(formula, "#REF")) %>%
    dplyr::filter(ref_error_fxs == TRUE) %>%
    dplyr::select(sheet_name, col, indicator_code, formula)

  fxs_ref_error
}


#' @export
#' @rdname schema-validations
checkSchema <- function(schema,
                        template_path,
                        cop_year,
                        tool) {

  stopifnot("Package \"waldo\" must be installed to use this function." =
              requireNamespace("waldo", quietly = TRUE))

  # Collect parameters ####
  schema <- schema %missing% NULL
  schema_provided <- !is.null(schema)

  template_path <- template_path %missing% NULL
  template_path_provided <- !is.null(template_path)

  # Validate parameters ####
  params <- check_params(cop_year = cop_year %missing% NULL,
                         tool = tool %missing% NULL,
                         schema = schema %missing% NULL,
                         template_path = template_path %missing% NULL)

  for (p in names(params)) {
    assign(p, purrr::pluck(params, p))
  }

  rm(params, p)

  # Create comparison schema. ####
    filepath_schema <-
      unPackSchema(
        template_path = template_path,
        skip = skip_tabs(tool = tool, cop_year = cop_year),
        tool = tool,
        cop_year = cop_year)

    ## If schema_object is provided, check schema against filepath_schema ####
    if (schema_provided && !identical(schema, filepath_schema)) {
      interactive_message(
        "Provided schema doesn't match the schema at the provided filepath.")
    }

    ## If schema_object not provided, use filepath_schema ####
    schema <- schema %||% filepath_schema

  # Validate schema ####
  # No matter what, we now have a schema to work from.
  # For all the below tests, TRUE = test fail

  tests <- list()

  ## Sheet Names complete ####
  tests$sheet_names_complete <-
    checkSchema_SheetNames(schema, filepath_schema)

  ## All Skipped sheets included  ####
  tests$skipped_sheets <- checkSchema_SkippedSheets(schema, tool, cop_year)

  ## Sheet Numbers don't omit any sheets ####
  tests$sheet_nums_complete <- checkSchema_SheetNums(schema)

  ## PSNUxIM Schema Specific Checks ####
  if (tool != "PSNUxIM") {
    ### dataset ####
    tests$datasets_invalid <- checkSchema_InvalidDatasets(schema, tool, cop_year)

    ### col_type ####
    tests$col_type_invalid <- checkSchema_InvalidColType(schema, tool, cop_year)

    ### value_type ####
    tests$value_type_invalid <- checkSchema_InvalidValueType(schema, tool, cop_year)
  }

  ###Test DEs name and UID syntax
  tests$DEs_syntax_invalid <- checkSchema_DataElementSyntax(schema)

  ###Test COs name and UID syntax
  tests$COs_syntax_invalid <- checkSchema_COsSyntax(schema)
  ###Test KPs name and UID syntax
  tests$KPS_invalid <- checkSchema_ValidKPs(schema)

  ## Test formulas ####
  tests$fxs_ref_error <- checkSchema_Formulas(schema)

  # Filter out any tests with zero rows ####
  tests <-
    tests  %>%
    purrr::keep(~ length(.x) > 0) %>%
    purrr::keep(~ NROW(.x) > 0)

  # Compile test results ####
  if (length(tests) > 0) {
    interactive_message("ERROR! Issues with schema values! See output.")
  } else {
    interactive_message("Schema checks out! Great job!")
  }

  tests
}


#' @export
#' @importFrom methods as
#' @rdname schema-validations
unPackSchema <- function(template_path = NULL,
                         skip = NULL,
                         tool = "Data Pack Template",
                         cop_year = NULL) {

  # Validate parameters ####
  # params <- check_params(cop_year = cop_year %missing% NULL,
  #                        tool = tool %missing% NULL,
  #                        template_path = template_path %missing% NULL)
  #
  # for (p in names(params)) {
  #   assign(p, purrr::pluck(params, p))
  # }
  #
  # rm(params, p)

  if (tool %in% c("PSNUxIM", "PSNUxIM Template")
        && cop_year %in% c(2023, 2024,2025)) {
    include_blank_cells <-  TRUE
  } else {
    include_blank_cells <-  FALSE
  }

  schema <- tidyxl::xlsx_cells(path = template_path,
                               include_blank_cells = include_blank_cells) %>%
    dplyr::select(sheet_name = sheet,
                  col,
                  row,
                  character,
                  formula,
                  numeric,
                  is_array)

  sheet_nums <- data.frame(sheet_name = unique(schema$sheet_name), stringsAsFactors = FALSE)
  sheet_nums$sheet_num <- seq_len(NROW(sheet_nums))

  schema <- schema %>%
    dplyr::inner_join(sheet_nums, by = c("sheet_name"))

  # Skip detail on listed sheets. ####
  if (is.null(skip)) {
    skip <- skip_tabs(tool = tool, cop_year = cop_year)
  }
  sheets <- unique(schema$sheet_name)
  verbose_sheets <- sheets[!sheets %in% skip$schema]

  schema %<>%
    dplyr::filter(sheet_name %in% verbose_sheets,
                  row %in% c(5:(headerRow(tool, cop_year) + 1)))

  # Gather and Spread to get formula, value, and indicator_code in separate cols ####
  schema %<>%
    tidyr::gather(key, value, -sheet_num, -sheet_name, -col, -row) %>%
    tidyr::unite(new.col, c(key, row)) %>%
    tidyr::spread(new.col, value) %>%
    dplyr::select(sheet_num, sheet_name, col,
                  dataset = character_5,
                  col_type = character_6,
                  value_type = character_7,
                  dataelement_dsd = character_8,
                  dataelement_ta = character_9,
                  categoryoption_specified = character_10,
                  valid_ages = character_11,
                  valid_sexes = character_12,
                  valid_kps = character_13,
                  indicator_code = character_14,
                  formula = formula_15,
                  value = numeric_15)

  # When formula is empty, pull from value (Assumed value) ####
  schema %<>%
    dplyr::mutate(formula = dplyr::if_else(is.na(formula), value, formula))

  # For PSNUxIM Data Packs, delete everything in metadata rows/cols
  if (tool %in% c("PSNUxIM", "PSNUxIM Template")) {
    schema %<>%
      dplyr::mutate_at(
        dplyr::vars(
          c("dataelement_dsd", "dataelement_ta", "categoryoption_specified",
            "valid_ages", "valid_sexes", "valid_kps")),
            ~ (. <- NA_character_))
  }

  # Translate valid disaggs ####
  empty <- list(tibble::tribble(
    ~name, ~id,
    NA_character_, NA_character_))

  if (tool %in% c("PSNUxIM", "PSNUxIM Template")) {
    disaggs <- list(tibble::tribble(
      ~name, ~id,
      NA_character_, NA_character_))

    schema %<>%
      dplyr::mutate(
        valid_ages.options = empty,
        valid_sexes.options = empty,
        valid_kps.options = empty
      )
  }

  if (tool %in% c("Data Pack Template", "Data Pack")) {

    cop_year_select <- gsub("^20", "COP", as.character(cop_year))
    map_datapack_cogs <- datapackr::datapack_cogs %>%
      purrr::pluck(cop_year_select)
    stopifnot("Can't find categoryOptionGroups for that cop_year and tool." = is.data.frame(map_datapack_cogs))

    map_datapack_cogs %<>%
      dplyr::mutate(categoryOptions = purrr::map(
        categoryOptions,
        ~ .x %>%
          dplyr::mutate(
            name = stringr::str_replace_all(name, "(?<!\\d)\\d(?!\\d)",
                   function(x) paste0("0", as.character(x)))
          )
      ))

  # Add disagg lists to schema ####
    map_datapack_cogs %<>%
      dplyr::select(-id) %>%
      dplyr::rename(datapack_cog = name) %>%
      tidyr::unnest(cols = categoryOptions) %>%
      dplyr::distinct() %>%
      dplyr::arrange(datapack_cog, name) %>%
      dplyr::group_by(datapack_cog) %>%
      tidyr::nest(options = c(name, id))

    schema %<>%
      dplyr::left_join(
        map_datapack_cogs %>% dplyr::rename(valid_ages.options = options),
        by = c("valid_ages" = "datapack_cog")
      ) %>%
      dplyr::left_join(
        map_datapack_cogs %>% dplyr::rename(valid_sexes.options = options),
        by = c("valid_sexes" = "datapack_cog")
      ) %>%
      dplyr::left_join(
        map_datapack_cogs %>% dplyr::rename(valid_kps.options = options),
        by = c("valid_kps" = "datapack_cog")
      )

    schema %<>%
      dplyr::mutate(
        valid_ages.options = dplyr::case_when(
          !is.na(valid_ages) ~ valid_ages.options,
          TRUE ~ empty),
        valid_sexes.options = dplyr::case_when(
          !is.na(valid_sexes) ~ valid_sexes.options,
          TRUE ~ empty),
        valid_kps.options = dplyr::case_when(
          !is.na(valid_kps) ~ valid_kps.options,
          TRUE ~ empty),
      )
  }



  schema %<>%
    dplyr::select(sheet_num, sheet_name, col, indicator_code,
                  dataset, col_type, value_type,
                  dataelement_dsd, dataelement_ta, categoryoption_specified,
                  valid_ages = valid_ages.options,
                  valid_sexes = valid_sexes.options,
                  valid_kps = valid_kps.options,
                  formula) %>%
    dplyr::arrange(sheet_num, col)

  # Add FY & period to identify targets across years (needed to produce import files)
  schema %<>%
    dplyr::mutate(
      FY = dplyr::case_when(
        stringr::str_detect(indicator_code, "\\.(T|M|C)$") ~ cop_year + 1,

        # # Accommodate OGAC request to place Spectrum IMPATT data in planning FY
        # # instead of projection year. (+1 FY)
        # (stringr::str_detect(indicator_code, "\\.T_1$")
        #   & dataset == "impatt"
        #   & !stringr::str_detect(indicator_code, "PRIORITY_SNU"))
        #  ~ cop_year + 1,

        # PATCH: DP-995: OGAC request to import KP_ESTIMATES into cop_year period
        # unlike other `.T_1` IMPATT estimates.
        stringr::str_detect(indicator_code, "^KP_ESTIMATES(.+)T_1$") ~ cop_year + 1,

        stringr::str_detect(indicator_code, "\\.T_1$") ~ cop_year,
        stringr::str_detect(indicator_code, "\\.R$") ~ cop_year - 1,
        stringr::str_detect(indicator_code, "\\.(T|M)2$") ~ cop_year + 2,
      # Apply default cop_year to blank cols in PSNUxIM tab
        dataset == "mer" & col_type == "target" ~ cop_year + 1,
        TRUE ~ NA_real_
      ),
      period = dplyr::case_when(
        col_type == "target" ~ paste0(FY - 1, "Oct"),
        col_type == "result" ~ paste0(FY, "Q3")
      )
    )

  # Add skipped sheets ####
  skipped_schema <- matrix(nrow = 0, ncol = NCOL(schema)) %>%
    as.data.frame() %>%
    stats::setNames(names(schema))

  skipped_schema[] <- mapply(FUN = as, skipped_schema, sapply(schema, class), SIMPLIFY = FALSE)

  skipped_schema %<>%
    tibble::add_row(sheet_name = skip$schema) %>%
    dplyr::mutate(valid_ages = empty, valid_sexes = empty, valid_kps = empty) %>%
    dplyr::select(-sheet_num) %>%
    dplyr::left_join(sheet_nums, by = "sheet_name")

   #Return the final schema
  schema <- dplyr::bind_rows(skipped_schema, schema) %>%
    dplyr::mutate(
      data_structure =
        dplyr::case_when(sheet_name %in% skip$schema ~ "skip",
                         TRUE ~ "normal")) %>%
    dplyr::select(sheet_num, sheet_name, data_structure, dplyr::everything()) %>%
    dplyr::arrange(sheet_num)

  return(schema)

}
