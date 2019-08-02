#' @export
#' @importFrom data.table :=
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
  
  # Check the filepath is valid. If NA, request via window. ####
  filepath <- handshakeFile(path = filepath,
                            tool = "Data Pack Template")
  
  schema <- tidyxl::xlsx_cells(path = filepath, include_blank_cells = FALSE) %>%
    dplyr::select(sheet_name = sheet, col, row, character, formula, numeric)
  
  # Add sheet number based on order of occurrence in workbook, rather than A-Z ####
  data.table::setDT(schema)[,sheet_num:=.GRP, by = c("sheet_name")]
  
  # Skip detail on listed sheets. ####
  sheets <- tidyxl::xlsx_sheet_names(filepath)
  verbose_sheets <- sheets[!sheets %in% skip]
  
  schema %<>%
    dplyr::filter(sheet_name %in% verbose_sheets,
                  row %in% c(5:(headerRow("Data Pack Template")+1))) %>%
    
  # Gather and Spread to get formula, value, and indicator_code in separate cols ####
    tidyr::gather(key,value,-sheet_num,-sheet_name,-col,-row) %>%
    tidyr::unite(new.col, c(key,row)) %>%
    tidyr::spread(new.col,value) %>%
    dplyr::select(sheet_num, sheet_name, col,
                  dataset = character_5, # TODO: Find a way to automate these suffixes ####
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
  five_yr_ages <- c("<01","01-04","05-09","10-14","15-19","20-24","25-29",
                    "30-34","35-39","40-44","45-49","50+")
  five_year_ages.25_49 <- five_yr_ages[7:11]
  five_yr_ages.1plus <- five_yr_ages[2:12]
  five_yr_ages.15plus <- five_yr_ages[5:12]
  five_yr_ages.10plus <- five_yr_ages[4:12]
  ovc_ages <- c(five_yr_ages[1:4],"15-17","18+")
  coarse_ages <- c("<15","15+")
  
  coarse_kps <- c("FSW","MSM","PWID","TG",
                  "People in prisons and other enclosed settings")
  finer_kps <- c("Female PWID","Male PWID","MSM not SW",
                 "MSM SW","TG not SW","TG SW",
                 "People in prisons and other enclosed settings")
  pwid_kps <- finer_kps[1:2]
  
  schema %<>%
    dplyr::mutate(
      valid_ages = dplyr::case_when(
        valid_ages == "5 yr" ~ list(five_yr_ages),
        valid_ages == "5 yr, 25-49" ~ list(five_year_ages.25_49),
        valid_ages == "15s" ~ list(coarse_ages),
        valid_ages == "5 yr, 1+" ~ list(five_yr_ages.1plus),
        valid_ages == "5 yr, 15+" ~ list(five_yr_ages.15plus),
        valid_ages == "5 yr, 10+" ~ list(five_yr_ages.10plus),
        valid_ages == "5 yr, <01-18+" ~ list(ovc_ages)),
      valid_sexes = dplyr::case_when(
        valid_sexes == "M/F" ~ list(c("Male", "Female")),
        valid_sexes == "M" ~ list(c("Male")),
        valid_sexes == "F" ~ list(c("Female"))
      ),
      valid_kps = dplyr::case_when(
        valid_kps == "Coarse KP" ~ list(coarse_kps),
        valid_kps == "Finer KP" ~ list(finer_kps),
        valid_kps == "M/F PWID" ~ list(pwid_kps)
      )) %>%
    dplyr::select(sheet_num, sheet_name, col, indicator_code,
                  dataset, col_type, value_type,
                  dataelement_dsd, dataelement_ta, categoryoption_specified,
                  valid_ages, valid_sexes, valid_kps,
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
  
  schema %<>%
    dplyr::bind_rows(skipped_schema, .) %>%
    dplyr::mutate(
      data_structure =
        dplyr::case_when(sheet_name %in% skip ~ "skip",
                         TRUE ~ "normal")) %>%
    dplyr::select(sheet_num, sheet_name, data_structure, dplyr::everything())
  
  schema[schema == "NULL" | schema == "NA"] <- NA_character_
  
  
  # TEST schema is valid ####
  skip_sheets_num <- schema %>%
    dplyr::filter(sheet_name %in% skip) %>%
    dplyr::select(sheet_num) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  tests <- schema %>%
    dplyr::left_join(
      data.frame(
        "sheet_name" = sheets,
        "sheet_num.test" = 1:length(sheets),
        stringsAsFactors = FALSE),
      by = "sheet_name") %>%
    dplyr::mutate(
      sheet_num.test = sheet_num != sheet_num.test,
      sheet_name.test = !sheet_name %in% sheets,
      dataelement_dsd.test = 
        !stringr::str_detect(dataelement_dsd, "^([A-Za-z][A-Za-z0-9]{10})$"),
      dataelement_ta.test = 
        !stringr::str_detect(dataelement_ta, "^([A-Za-z][A-Za-z0-9]{10})$"),
      categoryoption.test = 
        !stringr::str_detect(
          categoryoption_specified,
          "^([A-Za-z][A-Za-z0-9]{10})(\\.(([A-Za-z][A-Za-z0-9]{10})))*$"),
      dataset.test = 
        dplyr::case_when(
          col_type %in% c("reference","assumption","calculation","row_header","allocation") 
            ~ !dataset %in% c("datapack"),
          col_type %in% c("target","past") ~ !dataset %in% c("mer","impatt","subnat"),
          sheet_num %in% skip_sheets_num ~ !is.na(dataset),
          TRUE ~ TRUE),
      col_type.test = 
        (!col_type %in% c("target","reference","assumption","calculation", "past",
                        "row_header","allocation"))
        & (sheet_num %in% skip_sheets_num & !is.na(col_type)),
      value_type.test =
        (!value_type %in% c("integer","percentage","string"))
        & (sheet_num %in% skip_sheets_num & !is.na(value_type)),
      valid_ages.test =
        !valid_ages %in% c(list(five_yr_ages),list(five_year_ages.25_49),
                           list(five_yr_ages.1plus),list(five_yr_ages.15plus),
                           list(five_yr_ages.10plus),list(coarse_ages),
                           list(ovc_ages),NA_character_),
      valid_sexes.test =
        !valid_sexes %in% c(list(c("Male","Female")),list(c("Male")),list(c("Female")),NA_character_),
      valid_kps.test =
        !valid_kps %in% c(list(finer_kps),list(coarse_kps),list(pwid_kps),NA_character_)
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
