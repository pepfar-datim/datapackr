#' @export
#' @md
#'
#' @title Validate a `datapackr` Schema object
#'
#' @description
#' Useful in validating a `datapackr` schema object and investigating any issues.
#'
#' @inheritParams datapackr_params
#'
#' @return List object with any failed Schema validation checks.
#'
#' @family schema-helpers
validateSchema <- function(schema,
                           template_path,
                           cop_year,
                           tool,
                           season) {

  stopifnot("Package \"waldo\" must be installed to use this function." =
              requireNamespace("waldo", quietly = TRUE))

  # Collect parameters ####
  schema <- schema %missing% NULL
  schema_provided <- !is.null(schema)

  template_path <- template_path %missing% NULL
  filepath_provided <- !is.null(template_path)

  # Validate parameters ####
  cop_year <- cop_year %missing% NULL
  tool <- tool %missing% NULL
  season <- season %missing% NULL

  params <- check_params(cop_year = cop_year,
                         tool = tool,
                         season = season)

  for (p in names(params)) {
    assign(p, purrr::pluck(params, p))
  }

  # If template_path provided, check it and unpack it to create comparison schema.
  if (filepath_provided) {
    template_path %<>% checkTemplatePath(template_path = .,
                                    cop_year = cop_year,
                                    tool = tool,
                                    season = season)

    filepath_schema <-
      unPackSchema_datapack(
        template_path = template_path,
        skip = skip_tabs(tool = tool, cop_year = cop_year),
        cop_year = cop_year)

  # If schema_object not provided, use filepath_schema
    schema <- schema %||% filepath_schema
  }

  if (schema_provided) {
  # Check if this schema is already one we have archived
    schema %<>% check_schema(schema = .,
                             cop_year = cop_year,
                             tool = tool,
                             season = season)

  # Compare schema against filepath_schema
    if (filepath_provided) {
      if (!identical(schema, filepath_schema)) {
        interactive_message(
          "Provided schema doesn't match the schema at the provided filepath.")
      }
    }
  }

  # Validate schema ####
    # No matter what, we now have a schema to work from.
    # For all the below tests, TRUE = test fail

  tests <- list()

    ## All Skipped sheets included  ####
  skip <- skip_tabs(tool = tool, cop_year = cop_year)
  schema_skip <- schema %>%
    dplyr::filter(sheet_name %in% skip) %>%
    dplyr::select(sheet_name, sheet_num)

  skip_sheets <- list(
    num = unique(schema_skip$sheet_num),
    names = unique(schema_skip$sheet_name))

  skip_comparison <- waldo::compare(skip_sheets$names, skip,
                                    x_arg = "schema", y_arg = "package")

  if (length(skip_comparison) != 0) {
    tests$skipped_sheets <- list(
      error = length(skip_comparison) != 0,
      data = skip_comparison
    )
  }

    ## Sheet Numbers don't omit any sheets ####
  observed_sheet_nums <- unique(schema$sheet_num)
  expected_sheet_nums <- c(min(schema$sheet_num):max(schema$sheet_num))

  sheet_nums_comparison <- waldo::compare(observed_sheet_nums,
                                          expected_sheet_nums,
                                          x_arg = "observed",
                                          y_arg = "expected")

  if (length(sheet_nums_comparison) != 0) {
    tests$sheet_nums_complete <- list(
      error = length(sheet_nums_comparison) != 0,
      data = sheet_nums_comparison)
  }

    ## Sheet Names complete ####
  if (filepath_provided) {
    observed_sheet_names <- unique(schema$sheet_name)
    expected_sheet_names <- unique(filepath_schema$sheet_name)

    sheet_names_comparison <- waldo::compare(observed_sheet_names,
                                             expected_sheet_names,
                                             x_arg = "observed",
                                             y_arg = "expected")

    if (length(sheet_names_comparison) != 0) {
      tests$sheet_names_complete <- list(
        error = length(sheet_names_comparison) != 0,
        data = sheet_names_comparison)
    }
  }

    ## dataset ####
  if (!grepl("OPU Data Pack", tool)) {
    datasets_invalid <- schema %>%
      dplyr::mutate(
        invalid_dataset =
          dplyr::case_when(
            col_type %in% c("reference", "assumption", "calculation", "row_header", "allocation")
            ~ !dataset == c("datapack"),
            col_type %in% c("target", "past", "result") ~ !dataset %in% c("mer", "impatt", "subnat"),
            sheet_num %in% skip_sheets$num ~ !is.na(dataset),
            TRUE ~ TRUE)) %>%
      dplyr::filter(invalid_dataset == TRUE) %>%
      dplyr::select(sheet_name, data_structure, col, indicator_code, dataset, col_type)

    if (NROW(datasets_invalid) != 0) {
      tests$datasets_invalid <- datasets_invalid
    }
  }

    ## col_type ####
  if (!grepl("OPU Data Pack", tool)) {
    col_type_invalid <- schema %>%
      dplyr::mutate(
        invalid_col_type =
          (!col_type %in% c("target", "reference", "assumption", "calculation", "past",
                            "row_header", "allocation", "result"))
          & (sheet_num %in% skip_sheets$num & !is.na(col_type))) %>%
      dplyr::filter(invalid_col_type == TRUE) %>%
      dplyr::select(sheet_name, col, indicator_code, data_structure, col_type)

    if (NROW(col_type_invalid) != 0) {
      tests$col_type_invalid <- col_type_invalid
    }
  }

    ## dataElements ####
      ### UID Syntax
  DEs_schema <- schema %>%
    dplyr::filter(col_type %in% c("past", "target", "result")) %>%
    dplyr::select(sheet_name, col, indicator_code, dataset, dataelement_dsd, dataelement_ta)

  uid_pattern <- "[A-Za-z][A-Za-z0-9]{10}"
  multi_uid_pattern <- paste0("^(", uid_pattern, ")(\\.((", uid_pattern, ")))*$")

  DEs_DSD_syntax_invalid <- DEs_schema %>%
    dplyr::select(-dataelement_ta) %>%
    dplyr::mutate(
      invalid_DSD_DEs =
        dplyr::if_else(
          sheet_name == "PSNUxIM", dataelement_dsd != "NA",
          !stringr::str_detect(dataelement_dsd, multi_uid_pattern))) %>%
    dplyr::filter(invalid_DSD_DEs == TRUE)

  if (NROW(DEs_DSD_syntax_invalid) != 0) {
    tests$DEs_DSD_syntax_invalid <- DEs_DSD_syntax_invalid
  }

  DEs_TA_syntax_invalid <- DEs_schema %>%
    dplyr::select(-dataelement_dsd) %>%
    dplyr::mutate(
      invalid_TA_DEs =
        dplyr::if_else(
          sheet_name == "PSNUxIM", dataelement_ta != "NA",
          !stringr::str_detect(dataelement_ta, multi_uid_pattern))) %>%
    dplyr::filter(invalid_TA_DEs == TRUE)

  if (NROW(DEs_TA_syntax_invalid) != 0) {
    tests$DEs_TA_syntax_invalid <- DEs_TA_syntax_invalid
  }

    ##> Match DATIM (valid UIDs only)
  # DEs_mismatch_DATIM <- DEs_schema %>%
  #   dplyr::filter(!dataelement_dsd %in% DEs_DSD_syntax_invalid$dataelement_dsd,
  #                 !dataelement_ta %in% DEs_TA_syntax_invalid$dataelement_ta) %>%
    # how to check these against DATIM... Cache the data? pull fresh? how to maintain which datasets to pull?

    ## categoryoption_specified ####
      ### UID Syntax
  COs_syntax_invalid <- schema %>%
    dplyr::filter(col_type %in% c("past", "target", "result")) %>%
    dplyr::select(sheet_name, col, indicator_code, categoryoption_specified) %>%
    dplyr::mutate(
      invalid_COs =
        dplyr::if_else(
          sheet_name == "PSNUxIM", categoryoption_specified != "NA",
          !stringr::str_detect(categoryoption_specified, multi_uid_pattern))) %>%
    dplyr::filter(invalid_COs == TRUE)

  if (NROW(COs_syntax_invalid) != 0) {
    tests$COs_syntax_invalid <- COs_syntax_invalid
  }

    ## value_type ####
  if (!grepl("OPU Data Pack", tool)) {
    value_type_invalid <- schema %>%
      dplyr::mutate(
        invalid_value_type =
          (!value_type %in% c("integer", "percentage", "string"))
          & (sheet_num %in% skip_sheets$num & !is.na(value_type))) %>%
      dplyr::filter(invalid_value_type == TRUE) %>%
      dplyr::select(sheet_name, col, indicator_code, value_type)

    if (NROW(value_type_invalid) != 0) {
      tests$value_type_invalid <- value_type_invalid
    }
  }
      # TODO: Update
    ## Test valid_ages ####
      #     valid_ages.test =
      #      !(valid_ages %in% map_datapack_cogs$options | valid_ages %in% empty),
      #
    ## Test valid_sexes ####
      #     valid_sexes.test =
      #       !valid_sexes %in%
      # c(map_datapack_cogs$options[map_datapack_cogs$datapack_cog %in% c("Females", "Males", "M/F")],
      #                           empty),
      #
    ## Test valid_kps ####
      #     valid_kps.test =
      #       !valid_kps %in% c(map_datapack_cogs$options[map_datapack_cogs$datapack_cog == "Coarse KPs"], empty),

    ## Test formulas ####
  fxs_ref_error <- schema %>%
    dplyr::mutate(
      ref_error_fxs = stringr::str_detect(formula, "#REF")) %>%
    dplyr::filter(ref_error_fxs == TRUE) %>%
    dplyr::select(sheet_name, col, indicator_code, formula)

  if (NROW(fxs_ref_error) != 0) {
    tests$fxs_ref_error <- fxs_ref_error
  }

  # TODO: TESTS to add ####
    # * No duplicate indicator_codes on any single sheet
    # * Labels (row 3) for % cols include % at end
    # * Labels include FY at end where applicable
    # * Column categories (row 2) are correctly worded
    # * indicator_codes match style, time period, and label
    # * Subtotal fxs for numeric are correct
    # * Subtotal fxs for % cols reference other cols correctly
    # * Age, Sex, KP groups match DATIM COGS
    # * ID column fx is correct
    # * No invalid comment types
    # * Numeric or % formatting correct

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
#' @title Extract and save schema from Data Pack template.
#'
#' @description
#' Supplied a filepath to a Data Pack template (XLSX), will extract and save a
#' schema based on the template.
#'
#' @param skip Character vector of Sheet Names to label for skipping in schema.
#' @inheritParams datapackr_params
#'
#' @return Data Pack schema.
#'
#' @family schema-helpers
#'
unPackSchema_datapack <- function(template_path = NULL,
                                  skip = NULL,
                                  tool = "Data Pack Template",
                                  cop_year = getCurrentCOPYear()) {

  if ((tool == "Data Pack Template" & !cop_year %in% c(2021, 2022))
      | (tool == "OPU Data Pack Template" & !cop_year %in% 2021:2022)) {
    stop("Sorry, unPackSchema doesn't work for that combination of tool and cop_year.")
  }

  # Check the filepath is valid. If NA, request via window. ####
  filepath <- handshakeFile(path = template_path,
                            tool = tool)

  if (tool == "OPU Data Pack Template" & cop_year %in% c(2021)) {
    schema <- tidyxl::xlsx_cells(path = filepath, include_blank_cells = T) %>%
      dplyr::select(sheet_name = sheet, col, row, character, formula, numeric, is_array)
  } else {
    schema <- tidyxl::xlsx_cells(path = filepath, include_blank_cells = F) %>%
      dplyr::select(sheet_name = sheet, col, row, character, formula, numeric)
  }

  sheets <-data.frame(sheet_name = unique(schema$sheet_name), stringsAsFactors = FALSE)
  sheets$sheet_num <- seq_len(NROW(sheets))
  
  schema <- schema %>% 
    dplyr::inner_join(sheets, by=c("sheet_name"))

  # Skip detail on listed sheets. ####
  if (is.null(skip)) {
    skip <- skip_tabs(tool = tool, cop_year = cop_year)
  }
  sheets <- tidyxl::xlsx_sheet_names(filepath)
  verbose_sheets <- sheets[!sheets %in% skip]

  schema %<>%
    dplyr::filter(sheet_name %in% verbose_sheets,
                  row %in% c(5:(headerRow(tool, cop_year) + 1)))

  # Gather and Spread to get formula, value, and indicator_code in separate cols ####
  schema %<>%
    tidyr::gather(key, value, -sheet_num, -sheet_name, -col, -row) %>%
    tidyr::unite(new.col, c(key, row)) %>%
    tidyr::spread(new.col, value) %>%
    #TODO: How to avoid hardcoding these numbers??
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

  # For OPU Data Packs, delete everything in metadata rows/cols
  if (tool == "OPU Data Pack Template") {
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

  if (tool == "OPU Data Pack Template") {
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

  if (tool == "Data Pack Template") {

    if (cop_year == 2021) {
      map_datapack_cogs <- datapackr::datapack_cogs$COP21
    } else if (cop_year == 2022) {
      map_datapack_cogs <- datapackr::datapack_cogs$COP22
    } else {
      stop("Can't find categoryOptionGroups for that cop_year and tool.")
    }

  # Left-Pad digits with zeros
    # TODO: Move into utilities.R
    pad <- function(digit) {
      padded <- paste0("0", digit)
    }

    map_datapack_cogs %<>%
      dplyr::mutate(
        categoryOptions = purrr::map(
          categoryOptions,
          ~ .x %>%
            dplyr::mutate(
              name = stringr::str_replace_all(name, "(?<!\\d)\\d(?!\\d)", pad))
        )
      )

  # Add disagg lists to schema ####
    map_datapack_cogs %<>%
      dplyr::select(-id) %>%
      dplyr::rename(datapack_cog = name) %>%
      tidyr::unnest(cols = categoryOptions) %>%
      dplyr::distinct() %>%
      dplyr::arrange(datapack_cog, name) %>%
      dplyr::group_by(datapack_cog) %>%
      tidyr::nest(options = c(name, id))

    # TODO: Add test to make sure Data Pack COGs match the above list

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
        stringr::str_detect(indicator_code, "\\.T$") ~ cop_year + 1,
        (stringr::str_detect(indicator_code, "\\.T_1$")
          & dataset == "impatt"
          & !stringr::str_detect(indicator_code, "PRIORITY_SNU"))
         ~ cop_year + 1,
        stringr::str_detect(indicator_code, "\\.T_1$") ~ cop_year,
        stringr::str_detect(indicator_code, "\\.R$") ~ cop_year - 1,
      # Apply default cop_year to blank cols in PSNUxIM tab
        dataset == "mer" & col_type == "target" ~ datapackr::getCurrentCOPYear(),
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
    tibble::add_row(sheet_name = skip, sheet_num = seq_along(skip)) %>%
    dplyr::mutate(valid_ages = empty, valid_sexes = empty, valid_kps = empty)

  schema %<>%
    dplyr::bind_rows(skipped_schema, .) %>%
    dplyr::mutate(
      data_structure =
        dplyr::case_when(sheet_name %in% skip ~ "skip",
                         TRUE ~ "normal")) %>%
    dplyr::select(sheet_num, sheet_name, data_structure, dplyr::everything())

  schema

}
