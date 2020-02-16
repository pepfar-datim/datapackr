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
#' @param cop_year Specifies COP year for dating as well as selection of
#' templates.
#'
#' @return Data Pack schema.
#'
unPackSchema_datapack <- function(filepath = NULL, skip = NULL, cop_year = getCurrentCOPYear()) {

  # Check the filepath is valid. If NA, request via window. ####
  filepath <- handshakeFile(path = filepath,
                            tool = "Data Pack Template")

  schema <- tidyxl::xlsx_cells(path = filepath, include_blank_cells = FALSE) %>%
    dplyr::select(sheet_name = sheet, col, row, character, formula, numeric)

  # Add sheet number based on order of occurrence in workbook, rather than A-Z ####
  data.table::setDT(schema)[,sheet_num:=.GRP, by = c("sheet_name")]

  # Skip detail on listed sheets. ####
  if (is.null(skip)) {skip = skip_tabs(tool = "Data Pack", cop_year = cop_year)}
  sheets <- tidyxl::xlsx_sheet_names(filepath)
  verbose_sheets <- sheets[!sheets %in% skip]

  schema %<>%
    dplyr::filter(sheet_name %in% verbose_sheets,
                  row %in% c(5:(headerRow("Data Pack Template", cop_year)+1))) %>%

  # Gather and Spread to get formula, value, and indicator_code in separate cols ####
    tidyr::gather(key,value,-sheet_num,-sheet_name,-col,-row) %>%
    tidyr::unite(new.col, c(key,row)) %>%
    tidyr::spread(new.col,value) %>%
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
                  value = numeric_15) %>%

  # When formula is empty, pull from value (Assumed value) ####
    dplyr::mutate(formula = dplyr::if_else(is.na(formula), value, formula))

  # Translate valid disaggs ####
  valid_COs <- validCOs(cop_year = cop_year)

  disaggs <- valid_COs %>%
    dplyr::select(name = datapack_disagg, id, group = datapack_schema_group) %>%
    dplyr::filter(group != "") %>%
    tidyr::separate_rows(group, sep = ",") %>%
    dplyr::arrange(group, name) %>%
    dplyr::group_by(group) %>%
    tidyr::nest(options = c(name,id)) %>%
    tibble::deframe()

  empty <- list(tibble::tribble(
    ~name, ~id,
    NA_character_, NA_character_))

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
        valid_sexes == "M" ~ list(disaggs$`M/F`[disaggs$`M/F`$name == "Male",]),
        valid_sexes == "F" ~ list(disaggs$`M/F`[disaggs$`M/F`$name == "Female",]),
        TRUE ~ empty),
      valid_kps.options = dplyr::case_when(
        valid_kps == "coarseKPs" ~ list(disaggs$coarseKPs),
        valid_kps == "fineKPs" ~ list(disaggs$fineKPs),
        valid_kps == "pwidKPs" ~ list(disaggs$pwidKPs),
        TRUE ~ empty)
      ) %>%
    dplyr::select(sheet_num, sheet_name, col, indicator_code,
                  dataset, col_type, value_type,
                  dataelement_dsd, dataelement_ta, categoryoption_specified,
                  valid_ages = valid_ages.options,
                  valid_sexes = valid_sexes.options,
                  valid_kps = valid_kps.options,
                  formula) %>%
    dplyr::arrange(sheet_num, col)

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
  #                      dplyr::select("dataelement","dataelementuid",dplyr::matches(.x)) %>%
  #                      tidyr::nest(.key = !! rlang::sym(.y))) %>%
  #   dplyr::bind_cols() %>%
  #   dplyr::select(-dataelement1,-dataelementuid1, -dataelement)
  #
  # # Add valid disaggs to schema
  # schema %<>%
  #   dplyr::left_join(
  #     fullCodeList, by = c("dataelement_dsd" = "dataelementuid")
  #   )

  # Add skipped sheets ####
  skipped_schema <- matrix(nrow = 0, ncol = NCOL(schema)) %>%
    as.data.frame() %>%
    setNames(names(schema)) %>%
    tibble::add_row(sheet_name = skip, sheet_num = 1:length(skip))

  skipped_schema[] <- mapply(FUN = as, skipped_schema, sapply(schema, class), SIMPLIFY = FALSE)

  skipped_schema %<>%
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

  #TODO: Add check to make sure T, T_1, and R map correctly to rows 5-13

  tests <- schema %>%

  ## Test Sheet Numbers ####
    dplyr::left_join(
      data.frame(
        "sheet_name" = tidyxl::xlsx_sheet_names(filepath),
        "sheet_num.test" = 1:length(tidyxl::xlsx_sheet_names(filepath)),
        stringsAsFactors = FALSE),
      by = "sheet_name") %>%
    dplyr::mutate(
      sheet_num.test = sheet_num != sheet_num.test,

  ## Test Sheet Names ####
      sheet_name.test = !sheet_name %in% sheets,

  ## Test Data Elements ####
      dataelement_dsd.test =
        !stringr::str_detect(dataelement_dsd, "^([A-Za-z][A-Za-z0-9]{10})$"),
      dataelement_ta.test =
        !stringr::str_detect(dataelement_ta, "^([A-Za-z][A-Za-z0-9]{10})$"),

  ## Test categoryOptions
      categoryoption.test =
        !stringr::str_detect(
          categoryoption_specified,
          "^([A-Za-z][A-Za-z0-9]{10})(\\.(([A-Za-z][A-Za-z0-9]{10})))*$"),

  ## Test datasets ####
      dataset.test =
        dplyr::case_when(
          col_type %in% c("reference","assumption","calculation","row_header","allocation")
            ~ !dataset == c("datapack"),
          col_type %in% c("target","past") ~ !dataset %in% c("mer","impatt","subnat"),
          sheet_num %in% skip_sheets_num ~ !is.na(dataset),
          TRUE ~ TRUE),

  ## Test col_type ####
      col_type.test =
        (!col_type %in% c("target","reference","assumption","calculation", "past",
                        "row_header","allocation"))
        & (sheet_num %in% skip_sheets_num & !is.na(col_type)),

  ## Test value_type ####
      value_type.test =
        (!value_type %in% c("integer","percentage","string"))
        & (sheet_num %in% skip_sheets_num & !is.na(value_type)),

  ## Test valid_ages ####
      valid_ages.test =
       !(valid_ages %in% disaggs | valid_ages %in% empty),

  ## Test valid_sexes ####
      valid_sexes.test =
        !valid_sexes %in% c(list(disaggs$`M/F`),
                            list(disaggs$`M/F`[disaggs$`M/F`$name == "Male",]),
                            list(disaggs$`M/F`[disaggs$`M/F`$name == "Female",]),
                            empty),

  ## Test valid_kps
      valid_kps.test =
        !valid_kps %in% c(list(disaggs$fineKPs),list(disaggs$coarseKPs),
                          list(disaggs$pwidKPs),empty)
    ) %>%
    dplyr::select(sheet_name,indicator_code,dplyr::matches("test")) %>%
    dplyr::filter_at(dplyr::vars(dplyr::matches("test")), dplyr::any_vars(. == TRUE))

  if (NROW(tests) > 0) {

    tests[is.na(tests)] <- ""
    tests[tests == FALSE] <- ""
    tests[tests == TRUE] <- "ERROR!"

    stop_msg <-
      capture.output(
        print(as.data.frame(tests), row.names = FALSE)
      )

    stop(
      paste0(
        "ERROR! Issues with schema values!\n\t",
        paste(stop_msg, collapse = "\n\t"),
        "\n")
      )
  }

  return(schema)
}
