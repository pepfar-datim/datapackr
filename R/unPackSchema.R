#' @export
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @title Extract and save schema from Data Pack template.
#'
#' @description
#' Supplied a filepath to a Data Pack template (XLSX), will extract and save a
#' schema based on the template.
#'
#' @param filepath Local filepath for a Data Pack template (XLSX).
#' @param skip Character vector of Sheet Names to label for skipping in schema.
#' @inheritParams datapackr_params
#'
#' @return Data Pack schema.
#'
unPackSchema_datapack <- function(filepath = NULL,
                                  skip = NULL,
                                  tool = "Data Pack Template",
                                  cop_year = getCurrentCOPYear()) {
  
  if ((tool == "Data Pack Template" & !cop_year %in% c(2021, 2022))
      | (tool == "OPU Data Pack Template" & !cop_year %in% 2020:2022)) {
    stop("Sorry, unPackSchema doesn't work for that combination of tool and cop_year.")
  }

  # Check the filepath is valid. If NA, request via window. ####
  filepath <- handshakeFile(path = filepath,
                            tool = tool)

  if (tool == "OPU Data Pack Template" & cop_year %in% c(2020, 2021)) {
    schema <- tidyxl::xlsx_cells(path = filepath, include_blank_cells = T) %>%
      dplyr::select(sheet_name = sheet, col, row, character, formula, numeric, is_array)
  } else {
    schema <- tidyxl::xlsx_cells(path = filepath, include_blank_cells = F) %>%
      dplyr::select(sheet_name = sheet, col, row, character, formula, numeric)
  }

  # Add sheet number based on order of occurrence in workbook, rather than A-Z ####
  data.table::setDT(schema)[, sheet_num := .GRP, by = c("sheet_name")]

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
    setNames(names(schema))

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


  #TODO: Convert remaining to separate function
  # TEST schema is valid (TRUE = Test fail) ####
  skip_sheets_num <- schema %>%
    dplyr::filter(sheet_name %in% skip) %>%
    dplyr::select(sheet_num) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  tests <- schema %>%

  ## Test Sheet Numbers ####
    dplyr::left_join(
      data.frame(
        "sheet_name" = tidyxl::xlsx_sheet_names(filepath),
        "sheet_num.test" = seq_along(tidyxl::xlsx_sheet_names(filepath)),
        stringsAsFactors = FALSE),
      by = "sheet_name") %>%
    dplyr::mutate(
      sheet_num.test = sheet_num != sheet_num.test,

  ## Test Sheet Names ####
      sheet_name.test = !sheet_name %in% sheets,

  ## Test Data Elements ####
      dataelement_dsd.test =
        dplyr::if_else(sheet_name == "PSNUxIM", dataelement_dsd != "NA",
                       !stringr::str_detect(dataelement_dsd,
                       "^([A-Za-z][A-Za-z0-9]{10})(\\.(([A-Za-z][A-Za-z0-9]{10})))*$")),
      dataelement_ta.test =
        dplyr::if_else(sheet_name == "PSNUxIM", dataelement_ta != "NA",
                      !stringr::str_detect(dataelement_ta,
                      "^([A-Za-z][A-Za-z0-9]{10})(\\.(([A-Za-z][A-Za-z0-9]{10})))*$")),

  ## Test categoryOptions
      categoryoption.test =
        dplyr::if_else(sheet_name == "PSNUxIM", categoryoption_specified != "NA",
                        !stringr::str_detect(
                          categoryoption_specified,
                          "^([A-Za-z][A-Za-z0-9]{10})(\\.(([A-Za-z][A-Za-z0-9]{10})))*$")),

  ## Test datasets ####
      dataset.test =
        dplyr::case_when(
          col_type %in% c("reference", "assumption", "calculation", "row_header", "allocation")
            ~ !dataset == c("datapack"),
          col_type %in% c("target", "past", "result") ~ !dataset %in% c("mer", "impatt", "subnat"),
          sheet_num %in% skip_sheets_num ~ !is.na(dataset),
          TRUE ~ TRUE),

  ## Test col_type ####
      col_type.test =
        (!col_type %in% c("target", "reference", "assumption", "calculation", "past",
                        "row_header", "allocation", "result"))
        & (sheet_num %in% skip_sheets_num & !is.na(col_type)),

  ## Test value_type ####
      value_type.test =
        (!value_type %in% c("integer", "percentage", "string"))
        & (sheet_num %in% skip_sheets_num & !is.na(value_type)),

  # TODO: Update
  ## Test valid_ages ####
  #     valid_ages.test =
  #      !(valid_ages %in% map_datapack_cogs$options | valid_ages %in% empty),
  #
  # ## Test valid_sexes ####
  #     valid_sexes.test =
  #       !valid_sexes %in%
  # c(map_datapack_cogs$options[map_datapack_cogs$datapack_cog %in% c("Females", "Males", "M/F")],
  #                           empty),
  #
  # ## Test valid_kps
  #     valid_kps.test =
  #       !valid_kps %in% c(map_datapack_cogs$options[map_datapack_cogs$datapack_cog == "Coarse KPs"], empty),

  ## Test formulas
      formula.test = stringr::str_detect(formula, "#REF")
    ) %>%
    dplyr::select(sheet_name, indicator_code, dplyr::matches("test")) %>%
    purrr::when(tool == "OPU Data Pack Template" ~ dplyr::select(., -dataset.test, -col_type.test, -value_type.test),
    ~ .) %>%
    dplyr::filter_at(dplyr::vars(dplyr::matches("test")), dplyr::any_vars(. == TRUE))

  if (NROW(tests) > 0) {

    tests[is.na(tests)] <- ""
    tests[tests == FALSE] <- ""
    tests[tests == TRUE] <- "ERROR!"

    stop_msg <-
      capture.output(
        print(as.data.frame(tests), row.names = FALSE)
      )

    warning(
      paste0(
        "ERROR! Issues with schema values!\n\t",
        paste(stop_msg, collapse = "\n\t"),
        "\n")
      )
  }

  if (cop_year == 2020) {
    schema <- dplyr::select(schema, -FY, -period)
  }

  return(schema)

}
