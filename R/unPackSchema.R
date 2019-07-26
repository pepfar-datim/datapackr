#' @export
#' @title Extract and save schema from Data Pack template.
#' 
#' @description
#' Supplied a filepath to a Data Pack template (XLSX), will extract and save a
#' schema based on the template.
#' 
#' @param path Local filepath for a Data Pack template (XLSX).
#' @param skip Character vector of Sheet Names to label for skipping in schema.
#' 
#' @return Data Pack schema.
#'
unPackSchema_datapack <- function(filepath = NA, skip = NA) {
  
  filepath <- handshakeFile(path = filepath,
                            tool = "Data Pack Template")
  
  sheets <- tidyxl::xlsx_sheet_names(filepath)
  verbose_sheets <- sheets[!sheets %in% skip]
  
  schema <- tidyxl::xlsx_cells(path = filepath, include_blank_cells = FALSE) %>%
    dplyr::select(sheet_name = sheet, col, row, character, formula, numeric)
  
  data.table::setDT(schema)[,sheet_num:=.GRP, by = c("sheet_name")]
  
  schema %<>%
    dplyr::filter(sheet_name %in% verbose_sheets,
                  row %in% c(5:12)) %>%
    dplyr::mutate(
      character =
        stringr::str_replace(
          character,
          "dataset|col_type|value_type|dataelement_dsd|dataelement_ta|categoryoption",
          NA_character_)) %>%
    tidyr::gather(key,value,-sheet_num,-sheet_name,-col,-row) %>%
    tidyr::unite(new.col, c(key,row)) %>%
    tidyr::spread(new.col,value) %>%
    dplyr::select(sheet_num, sheet_name, col,
                  dataset = character_5,
                  col_type = character_6,
                  value_type = character_7,
                  dataelement_dsd = character_8,
                  dataelement_ta = character_9,
                  categoryoption = character_10,
                  indicator_code = character_11,
                  formula = formula_12,
                  value = numeric_12) %>%
    dplyr::mutate(
      formula = dplyr::case_when(is.na(formula) ~ value,
                                 TRUE ~ formula),
      col_type =
        dplyr::case_when(
          indicator_code %in% c("PSNU","Age","Sex","ID","AgeCoarse","IDAgeCoarse",
                                "PSNUCheck","KeyPop","sheet_name","indicatorCode",
                                "CoarseAge","sheet_num")
            ~ "row_header",
          TRUE ~ col_type),
      value_type =
        dplyr::case_when(
          indicator_code %in% c("PSNU","Age","Sex","ID","AgeCoarse","IDAgeCoarse",
                                "PSNUCheck","KeyPop","sheet_name","indicatorCode",
                                "CoarseAge","sheet_num")
          ~ "string",
          TRUE ~ value_type),
      dataset =
        dplyr::case_when(
          indicator_code %in% c("PSNU","Age","Sex","ID","AgeCoarse","IDAgeCoarse",
                                "PSNUCheck","KeyPop","sheet_name","indicatorCode",
                                "CoarseAge","sheet_num")
          ~ "datapack",
          TRUE ~ dataset)) %>%
    dplyr::select(sheet_num, sheet_name, col, indicator_code, dataset, col_type,
                  value_type, dataelement_dsd, dataelement_ta, categoryoption,
                  formula) %>%
    dplyr::arrange(sheet_num, col)
  
  # TEST schema is valid
  tests <- schema %>%
    dplyr::mutate(
      dataelement_dsd.test = 
        !stringr::str_detect(dataelement_dsd, "^([A-Za-z][A-Za-z0-9]{10})$"),
      dataelement_ta.test = 
        !stringr::str_detect(dataelement_ta, "^([A-Za-z][A-Za-z0-9]{10})$"),
      categoryoption.test = 
        !stringr::str_detect(categoryoption, "^([A-Za-z][A-Za-z0-9]{10})(\\.(([A-Za-z][A-Za-z0-9]{10})))*$"),
      dataset.test = 
        (col_type %in% c("reference","assumption","calculation","row_header", "allocation") 
         & !dataset %in% c("datapack"))
      | (col_type %in% c("target","past") 
         & !dataset %in% c("mer","impatt","subnat"))
      | (!dataset %in% c("impatt","subnat","mer","datapack")),
      col_type.test = 
        !col_type %in% c("target","reference","assumption","calculation", "past",
                        "row_header","allocation"),
      value_type.test =
        !value_type %in% c("integer","percentage","string")
    ) %>%
    dplyr::select(sheet_name,indicator_code,dplyr::matches("test")) %>%
    dplyr::filter_at(dplyr::vars(dplyr::matches("test")), dplyr::any_vars(. == TRUE))
  
  tests[is.na(tests)] <- ""
  tests[tests == FALSE] <- ""
  tests[tests == TRUE] <- "ERROR!"
  
  if (NROW(tests) > 0) {
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
