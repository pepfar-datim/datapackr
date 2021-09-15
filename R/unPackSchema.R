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
#' @param tool Type of tool to unpack.
#' @param cop_year Specifies COP year for dating as well as selection of
#' templates.
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return Data Pack schema.
#'
unPackSchema_datapack <- function(filepath = NULL,
                                  skip = NULL,
                                  tool = "Data Pack Template",
                                  cop_year = getCurrentCOPYear(),
                                  d2_session = dynGet("d2_default_session",
                                                      inherits = TRUE)) {

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
  data.table::setDT(schema)[, sheet_num:=.GRP, by = c("sheet_name")]

  # Skip detail on listed sheets. ####
  if (is.null(skip)) {
    skip <- skip_tabs(tool = tool, cop_year = cop_year)
  }
  sheets <- tidyxl::xlsx_sheet_names(filepath)
  verbose_sheets <- sheets[!sheets %in% skip]

  schema %<>%
    dplyr::filter(sheet_name %in% verbose_sheets,
                  row %in% c(5:(headerRow(tool, cop_year)+1)))

  # # Correctly enter array formulas ####
  #   dplyr::mutate(
  #     formula = dplyr::case_when(
  #       is_array == "TRUE" ~ paste0("{", formula, "}"),
  #       TRUE ~ formula
  #     )
  #   ) %>%

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

  } else {

    if (cop_year == 2020) {
      valid_COs <- getValidCategoryOptions(cop_year = cop_year)

      disaggs <- valid_COs %>%
        dplyr::select(name = datapack_disagg, id, group = datapack_schema_group) %>%
        dplyr::filter(group != "") %>%
        tidyr::separate_rows(group, sep = ",") %>%
        dplyr::arrange(group, name) %>%
        dplyr::group_by(group) %>%
        tidyr::nest(options = c(name, id)) %>%
        tibble::deframe()

      schema %<>%
        dplyr::mutate(
          valid_ages.options = dplyr::case_when(
            valid_ages == "5 yr" ~ list(disaggs$`5yr`),
            valid_ages == "5 yr, 25-49" ~ list(disaggs$`25-49`),
            valid_ages == "15s" ~ list(disaggs$coarse),
            valid_ages == "5 yr, 1+" ~ list(disaggs$`01+`),
            valid_ages == "5 yr, 15+" ~ list(disaggs$`15+`),
            valid_ages == "5 yr, 10+" ~ list(disaggs$`10+`),
            valid_ages == "5 yr, <01-18+" ~ list(disaggs$ovc_serv),
            valid_ages == "5 yr, <01-17" ~ list(disaggs$ovc_hiv_stat),
            valid_ages == "01-04" ~ list(disaggs$`01-04`),
            valid_ages == "<01" ~ list(disaggs$`<01`),
            TRUE ~ empty),
          valid_sexes.options = dplyr::case_when(
            valid_sexes == "M/F" ~ list(disaggs$`M/F`),
            valid_sexes == "M" ~ list(disaggs$`M/F`[disaggs$`M/F`$name == "Male", ]),
            valid_sexes == "F" ~ list(disaggs$`M/F`[disaggs$`M/F`$name == "Female", ]),
            TRUE ~ empty),
          valid_kps.options = dplyr::case_when(
            valid_kps == "coarseKPs" ~ list(disaggs$coarseKPs),
            valid_kps == "fineKPs" ~ list(disaggs$fineKPs),
            valid_kps == "pwidKPs" ~ list(disaggs$pwidKPs),
            TRUE ~ empty)
        )
    } else if (cop_year == 2021) {
      map_datapack_cogs <-
        datimutils::getMetadata(categoryOptionGroups,
                                fields = "id,name,categoryOptions[id,name]", # nolint
                                "groupSets.name:like:COP 21 Data Pack",
                                d2_session = d2_session)

    # Left-Pad digits with zeros
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

    } else {
      stop("Cannot map valid disaggregates for that COP Year")
    }
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
        dataset == "mer" & col_type == "target" ~ datapackr::getCurrentCOPYear(),
        TRUE ~ NA_real_
      ),
      period = dplyr::case_when(
        col_type == "target" ~ paste0(FY-1, "Oct"),
        col_type == "result" ~ paste0(FY, "Q3")
      )
    )

  # Pull list of valid disaggs
  # codeList <- DEs_COCs_COs_Cs()
  #
  # fullCodeList <- codeList %>%
  # # Rename Key Population to match Data Pack namecon
  #   dplyr::select(dataelement, dataelementuid,
  #                 Age, Sex, KeyPop = `Key Population`) %>%
  #
  # # Drop other COs to only summarize Age, Sex, KP
  #   dplyr::distinct() %>%
  #
  # # Map to only those indicator_codes used in Data Pack
  #   dplyr::left_join(
  #     (schema %>%
  #        dplyr::filter(col_type == "target") %>%
  #        dplyr::select(indicator_code, dataelement_dsd, dataelement_ta) %>%
  #        tidyr::gather(key = "support_type", value = "dataelementuid",
  #                      dataelement_dsd, dataelement_ta, na.rm = TRUE) %>%
  #        dplyr::select(-support_type) %>%
  #        dplyr::distinct()),
  #     y = .,
  #     by = "dataelementuid"
  #   ) %>%
  #   dplyr::mutate(
  #     support_type = stringr::str_extract(dataelement, "(?<=(, ))(DSD|TA)(?=(, |\\)))"),
  #     support_type = dplyr::case_when(is.na(support_type) ~ "dsd",
  #                                     TRUE ~ tolower(support_type)))
  #
  # # Group Age, Sex, KP into single list col, and categoryoption name and id
  #   # into another col
  #   dplyr::group_by(dataelement, dataelementuid)
  #
  # match <- list("categoryoptioncombo", "Age|Sex|KeyPop")
  # key <- list("valid_cocs", "valid_cos")
  #
  # fullCodeList <- purrr::map2(.x = match, .y = key,
  #                    ~ fullCodeList %>%
  #                      dplyr::select("dataelement", "dataelementuid", dplyr::matches(.x)) %>%
  #                      tidyr::nest(.key = !! rlang::sym(.y))) %>%
  #   dplyr::bind_cols() %>%
  #   dplyr::select(-dataelement1, -dataelementuid1, -dataelement)
  #
  # # Add valid disaggs to schema
  # schema %<>%
  #   dplyr::left_join(
  #     fullCodeList, by = c("dataelement_dsd" = "dataelementuid")
  #   )

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
  #       !valid_sexes %in% c(map_datapack_cogs$options[map_datapack_cogs$datapack_cog %in% c("Females", "Males", "M/F")],
  #                           empty),
  #
  # ## Test valid_kps
  #     valid_kps.test =
  #       !valid_kps %in% c(map_datapack_cogs$options[map_datapack_cogs$datapack_cog == "Coarse KPs"], empty),

  ## Test formulas
      formula.test = stringr::str_detect(formula, "#REF")
    ) %>%
    dplyr::select(sheet_name, indicator_code, dplyr::matches("test")) %>%
    {
      if (tool == "OPU Data Pack Template")
        dplyr::select(., -dataset.test, -col_type.test, -value_type.test)
      else
        .
    } %>%
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
