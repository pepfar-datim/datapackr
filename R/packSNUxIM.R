#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom stats complete.cases
#' @title packSNUxIM(data)
#'
#' @description Packs SNUxIM data prepared from unPackSNUxIM for import to DATIM.
#'
#' @param d Datapackr object
#' 
#' @return d
#' 
packSNUxIM <- function(d) {

  if (!d$info$cop_year %in% c(2021)) {
    stop(paste0("Packing SNU x IM tabs is not supported for COP ",d$info$cop_year," Data Packs."))
  }
  
  # Check if SNUxIM data already exists ####
  if (NROW(d$data$SNUxIM) == 1 & is.na(d$data$SNUxIM$PSNU[1])) {
    d$info$has_psnuxim <- FALSE
  } else {d$info$has_psnuxim <- TRUE}
  
  # If does exist, extract missing combos ####
  if (d$info$has_psnuxim) {
    d$data$missingCombos <- d$data$MER %>%
      # TODO: Create this here rather than upstream
      dplyr::anti_join(d$data$PSNUxIM_combos)
    
    d$info$missing_psnuxim_combos <- (NROW(d$data$missingCombos) > 0)
  }
    
  # Proceed IFF no PSNU x IM tab exists, or exists but with missing combos ####
  if (d$info$has_psnuxim & !d$info$missing_psnuxim_combos) {
    return(d)
  }
  
  # Prepare SNU x IM model dataset ####
  snuxim_model_data <- readRDS(d$keychain$snuxim_model_data_path)[d$info$country_uids] %>%
    dplyr::bind_rows() %>%
    tidyr::unite(col = mechcode_supporttype, mechanism_code, type) %>%
    dplyr::select(psnu_uid, indicator_code, Age = age_option_name,
                  Sex = sex_option_name, KeyPop = kp_option_name,
                  mechcode_supporttype, percent) %>%
    dplyr::mutate(
      mechcode_supporttype = dplyr::case_when(
        mechcode_supporttype == "00000_DSD" ~ "DSD Dedupe",
        mechcode_supporttype == "00000_TA" ~ "TA Dedupe",
        mechcode_supporttype == "00001_TA" ~ "Crosswalk Dedupe",
        TRUE ~ mechcode_supporttype
      )
    ) %>%
    tidyr::pivot_wider(names_from = mechcode_supporttype,
                       values_from = percent) %>%
    
  # EID: Align model data age bands with Data Pack
    dplyr::mutate(
      Age = dplyr::if_else(
        indicator_code %in% c("PMTCT_EID.N.2.T", "PMTCT_EID.N.12.T"),
        NA_character_,
        Age
      )
    ) %>%  
    
  # Double check that Dedupe cols all exist as expected
    datapackr::addcols(cnames = c("DSD Dedupe",
                                  "TA Dedupe",
                                  "Crosswalk Dedupe"),
                       type = "numeric")
    
  # Create Deduplicated Rollups
  snuxim_model_data %<>%
    dplyr::mutate(
      `Total Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}")), na.rm = TRUE),
      `DSD Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}_DSD")), na.rm = TRUE),
      `TA Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}_TA")), na.rm = TRUE)) %>%
    
  # Create Duplicated Rollups
    dplyr::mutate(
      `Deduplicated DSD Rollup` =
        rowSums(dplyr::select(., tidyselect::all_of(c("DSD Duplicated Rollup","DSD Dedupe"))),
                na.rm = T),
      `Deduplicated TA Rollup` =
        rowSums(dplyr::select(., tidyselect::all_of(c("TA Duplicated Rollup","TA Dedupe"))),
                na.rm = T)) %>%
    dplyr::mutate(
      `Total Deduplicated Rollup` =
        rowSums(
          dplyr::select(.,
                        tidyselect::all_of(c("Deduplicated DSD Rollup",
                                             "Deduplicated TA Rollup",
                                             "Crosswalk Dedupe"))),
          na.rm = TRUE
        )
    )
    
  # Create Max columns
  rowMax <- function(df, cn, regex) {
    df[[cn]] <- df %>%
      dplyr::select(tidyselect::matches(match = regex)) %>%
      purrr::pmap(pmax, na.rm = T) %>%
      as.numeric
    
    return(df)
  }
  
  snuxim_model_data %<>%
    rowMax(cn = "Max_TA.T_1", regex = "\\d{4,}_TA") %>%
    rowMax(cn = "Max_DSD.T_1", regex = "\\d{4,}_DSD") %>%
    dplyr::mutate(
      `Max_Crosswalk.T_1` =
        pmax(`Deduplicated DSD Rollup`, `Deduplicated TA Rollup`, na.rm = T))
  
  # Create Dedupe Resolution columns
  snuxim_model_data %<>%
    dplyr::rowwise() %>%
    dplyr::mutate(ta_im_count = sum(!is.na(dplyr::c_across(tidyselect::matches("\\d{4,}_TA")))),
                  dsd_im_count = sum(!is.na(dplyr::c_across(tidyselect::matches("\\d{4,}_DSD"))))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      `Observed TA Dedupe Resolution (FY21)` = dplyr::case_when(
        `TA Duplicated Rollup` == 0 | ta_im_count <= 1 ~ NA_character_,
        # or where count(TA IMs) == 1
        `Deduplicated TA Rollup` == `TA Duplicated Rollup` ~ "SUM",
        `Deduplicated TA Rollup` == `Max_TA.T_1` ~ "MAX",
        TRUE ~ "CUSTOM"),
      `Observed DSD Dedupe Resolution (FY21)` = dplyr::case_when(
        `DSD Duplicated Rollup` == 0 | dsd_im_count <= 1 ~ NA_character_,
        `Deduplicated DSD Rollup` == `DSD Duplicated Rollup` ~ "SUM",
        `Deduplicated DSD Rollup` == `Max_DSD.T_1` ~ "MAX",
        TRUE ~ "CUSTOM"),
      `Observed Crosswalk Dedupe Resolution (FY21)` = dplyr::case_when(
        `Total Duplicated Rollup` == 0 | `Deduplicated TA Rollup` == 0 | `Deduplicated DSD Rollup` == 0 
          ~ NA_character_,
        `Total Deduplicated Rollup` == `Total Duplicated Rollup` ~ "SUM",
        `Total Deduplicated Rollup` == `Max_Crosswalk.T_1` ~ "MAX",
        TRUE ~ "CUSTOM")
    ) %>%
    dplyr::select(psnu_uid, indicator_code, Age, Sex, KeyPop,
                  tidyselect::matches("\\d{4,}"),
                  `Observed Custom DSD Dedupe Allocation (FY21) (%)` = `DSD Dedupe`,
                  `Observed Custom TA Dedupe Allocation (FY21) (%)` = `TA Dedupe`,
                  `Observed Custom Crosswalk Dedupe Allocation (FY21) (%)` = `Crosswalk Dedupe`,
                  `Observed DSD Dedupe Resolution (FY21)`,
                  `Observed TA Dedupe Resolution (FY21)`,
                  `Observed Crosswalk Dedupe Resolution (FY21)`)
      
  # Filter SNU x IM model dataset to only those data needed in tab ####
  if (d$info$has_psnuxim & d$info$missing_psnuxim_combos) {
    targets_data <- d$data$missingCombos
  } else {
    targets_data <- d$data$MER %>%
      dplyr::left_join(datapackr::valid_PSNUs %>%
                         dplyr::select(psnu_uid, snu1),
                       by = c("psnuid" = "psnu_uid")) %>%
    # Do not include AGYW_PREV -- These are not allocated to IMs
      dplyr::filter(!indicator_code %in% c("AGYW_PREV.N.T", "AGYW_PREV.D.T")) %>%
      dplyr::select(SNU1 = snu1, dplyr::everything())
  }
  
  if (NROW(snuxim_model_data) > 0) {
    snuxim_model_data %<>%
      dplyr::right_join(
        targets_data,
        by = c("psnu_uid" = "psnuid",
               "indicator_code" = "indicator_code",
               "Age" = "Age",
               "Sex" = "Sex",
               "KeyPop" = "KeyPop")) %>%
      dplyr::select(-value)
  } else {
    snuxim_model_data <- targets_data %>%
      datapackr::addcols(cnames = c("Observed Custom DSD Dedupe Allocation (FY21)",
                                    "Observed Custom TA Dedupe Allocation (FY21)",
                                    "Observed Custom Crosswalk Dedupe Allocation (FY21)"),
                         type = "numeric") %>%
      datapackr::addcols(cnames = c("Observed Custom DSD Dedupe Resolution (FY21)",
                                    "Observed Custom TA Dedupe Resolution (FY21)",
                                    "Observed Custom Crosswalk Dedupe Resolution (FY21)"),
                         type = "character")
  }

  # Add DataPackTarget ####
  top_rows <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)
  
  if (d$info$has_psnuxim) {
    SNUxIM_rows <-
      readxl::read_excel(
        path = d$keychain$submission_path,
        sheet = "PSNUxIM",
        range = readxl::cell_limits(c(1,2), c(NA,2)),
        col_names = F
      ) %>%
      NROW()
    
    existing_rows <- SNUxIM_rows
  } else {
    existing_rows <- top_rows
  }
  
  get_ID_col <- function(data) {
    col_letter <- data %>%
      dplyr::filter(indicator_code == "ID")
    
    if (NROW(col_letter) == 0) {
      col_letter <- data %>%
        dplyr::filter(indicator_code == "PSNU")}  
    
    col_letter %<>%
      dplyr::pull(submission_order) %>%
      openxlsx::int2col()
    
    return(col_letter)
  }
        
  id_cols <- lapply(d$info$col_check, get_ID_col) %>%
    dplyr::bind_rows() %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::rename(id_col = V1) %>%
    tibble::rownames_to_column("sheet_name")
  
  target_cols <- datapackr::cop21_data_pack_schema %>%
    dplyr::filter(dataset == "mer" & col_type == "target" & (!sheet_name %in% c("PSNUxIM", "AGYW"))) %>%
    dplyr::mutate(
      target_col = openxlsx::int2col(col)
    ) %>%
    dplyr::select(indicator_code, target_col)
        
  snuxim_model_data %<>%
    dplyr::left_join(
      id_cols, by = c("sheet_name" = "sheet_name")) %>%
    dplyr::left_join(
      target_cols, by = c("indicator_code" = "indicator_code")) %>%
    dplyr::mutate(
      row = as.integer((1:dplyr::n()) + existing_rows),
      id_col = dplyr::case_when(
        indicator_code == "OVC_HIVSTAT.T" ~ "B",
        TRUE ~ id_col),
      DataPackTarget = paste0(
        'SUMIF(',
        sheet_name, '!$', id_col, ':$', id_col,
        ',$G', row,
        ',', sheet_name, '!$', target_col, ':$', target_col, ')')
    ) %>%
    dplyr::select(-id_col, -sheet_name, -target_col, -row)
  
  class(snuxim_model_data[["DataPackTarget"]]) <- c(class(snuxim_model_data[["DataPackTarget"]]), "formula")
        
  # Get formulas & column order from schema ####
  data_structure <- datapackr::cop21_data_pack_schema %>%
    dplyr::filter(sheet_name == "PSNUxIM") %>%
    dplyr::arrange(col) %>%
    dplyr::mutate(
      column_names = dplyr::case_when(
        col > 10 & col < 86 ~ paste0("percent_col_",col),
        col > 112 ~ paste0("target_col_",col),
        TRUE ~ indicator_code)
    ) %>%
    tibble::column_to_rownames(var = "column_names") %>%
    dplyr::select(formula) %>%
    t() %>%
    tibble::as_tibble() %>%
    ## Setup formulas
    dplyr::slice(rep(1:dplyr::n(), times = NROW(snuxim_model_data))) %>%
    dplyr::mutate_if(
      is.character,
      stringr::str_replace_all,
      pattern = paste0("(?<=[:upper:])", headerRow(tool = "Data Pack Template",
                                                   cop_year = d$info$cop_year)
                       +1),
      replacement = as.character(1:NROW(snuxim_model_data)
                                 + headerRow(tool = "Data Pack Template",
                                             cop_year = d$info$cop_year)))
  
  # Classify formula columns as formulas
  ## TODO: Improve approach
  for (i in 1:length(data_structure)) {
    if (!all(any(is.na(data_structure[[i]])))) {
      class(data_structure[[i]]) <- c(class(data_structure[[i]]), "formula")
    }
  }
  
  # Combine schema with SNU x IM model dataset ####
  data_structure <- datapackr::swapColumns(data_structure, snuxim_model_data) %>%
    dplyr::bind_cols(
      snuxim_model_data %>%
        dplyr::select(tidyselect::matches("\\d{4,}"))
      )
  
  header_cols <- datapackr::cop21_data_pack_schema %>%
    dplyr::filter(sheet_name == "PSNUxIM"
                  & col <= 10) %>%
    dplyr::pull(indicator_code)
  
  IM_cols <- data_structure %>%
    dplyr::select(tidyselect::matches("\\d{4,}")) %>%
    names() %>%
    sort()
    
  left_side <- data_structure %>%
    dplyr::select(
      tidyselect::all_of(header_cols),
      tidyselect::all_of(IM_cols)
    )
  
  right_side <- data_structure %>%
    dplyr::select(
      -tidyselect::all_of(names(left_side)),
      -tidyselect::matches("percent_col_\\d{1,3}")
    )
        
  # Write data to sheet ####
  d$tool$wb <- openxlsx::loadWorkbook(d$keychain$submission_path)
  openxlsx::removeFilter(d$tool$wb, names(d$tool$wb))
  
  if (!d$info$has_psnuxim) {
    openxlsx::writeData(wb = d$tool$wb,
                        sheet = "PSNUxIM",
                        x = left_side,
                        xy = c(1, top_rows),
                        colNames = T, rowNames = F, withFilter = FALSE)
    
    openxlsx::writeData(wb = d$tool$wb,
                        sheet = "PSNUxIM",
                        x = right_side,
                        xy = c(86, top_rows + 1),
                        colNames = F, rowNames = F, withFilter = FALSE)
    
    d$info$newSNUxIM <- TRUE
  } else if (d$info$has_psnuxim & d$info$missing_psnuxim_combos) {
    if (d$info$has_psnuxim) {
      # Don't count blank rows at bottom or blank columns to left
      SNUxIM_cols <- 
        readxl::read_excel(
          path = d$keychain$submission_path,
          sheet = "PSNUxIM",
          range = readxl::cell_limits(c(top_rows, 1), c(top_rows, NA))
        )
      
      SNUxIM_rows <-
        readxl::read_excel(
          path = d$keychain$submission_path,
          sheet = "PSNUxIM",
          range = readxl::cell_limits(c(1,2), c(NA,2)),
          col_names = F
        ) %>%
        NROW()
      
      existing_rows <- SNUxIM_rows
      first_new_mech_col <- NCOL(SNUxIM_cols) + 1
      
    } else {
      SNUxIM_cols <- datapackr::cop21_data_pack_schema %>%
        dplyr::filter(sheet_name == "PSNUxIM",
                      !indicator_code %in% c("12345_DSD","12345_TA")) %>%
        dplyr::select(indicator_code) %>%
        `row.names<-`(.[, 1]) %>%
        t() %>%
        tibble::as_tibble()
      
      existing_rows <- top_rows
      first_new_mech_col <- length(header_cols) + 1
    }
    
    new_mech_cols <- names(d$data$SNUxIM_combined)[!names(d$data$SNUxIM_combined) %in% c(names(SNUxIM_cols), "row")]
    non_appended_mech_cols <- names(SNUxIM_cols)[!names(SNUxIM_cols) %in% names(d$data$SNUxIM_combined)]
    
    d$data$SNUxIM_combined %<>%
      {if (length(non_appended_mech_cols) > 0) {
        (.) %>% addcols(non_appended_mech_cols)
      } else {.}} %>%
      dplyr::select(names(SNUxIM_cols), tidyselect::all_of(new_mech_cols))
    
    openxlsx::writeData(wb = d$tool$wb,
                        sheet = "PSNUxIM",
                        x = d$data$SNUxIM_combined,
                        xy = c(1, existing_rows + 1),
                        colNames = F, rowNames = F, withFilter = FALSE)
          
    d$info$newSNUxIM <- TRUE
          
  # Add additional col_names if any
    openxlsx::writeData(wb = d$tool$wb,
                        sheet = "PSNUxIM",
                        x = new_mech_cols %>% as.matrix() %>% t(),
                        xy = c(first_new_mech_col, top_rows),
                        colNames = F, rowNames = F, withFilter = FALSE)
            
  # Add green highlights to appended rows, if any ####
    newRowStyle <- openxlsx::createStyle(fontColour = "#006100", fgFill = "#C6EFCE")
          
    openxlsx::addStyle(
      wb = d$tool$wb,
      sheet = "PSNUxIM",
      newRowStyle,
      rows = (existing_rows + 1):(existing_rows + NROW(d$data$SNUxIM_combined)),
      cols = 1:2,
      gridExpand = TRUE,
      stack = FALSE)
            
  } else {
   stop("Cannot write data where there seems to be no new data needed.")
  }
        
  # Format percent columns ####
  percentCols <- datapackr::cop21_data_pack_schema %>%
    dplyr::filter(sheet_name == "PSNUxIM",
                  value_type == "percentage") %>%
    dplyr::pull(col)
  
  percentStyle = openxlsx::createStyle(numFmt = "0%")
  
  openxlsx::addStyle(wb = d$tool$wb,
                    sheet = "PSNUxIM",
                    percentStyle,
                    rows = (top_rows + 1):(existing_rows + NROW(data_structure)),
                    cols = percentCols,
                    gridExpand = TRUE,
                    stack = FALSE)

  # Format integers ####
  # integerStyle = openxlsx::createStyle(numFmt = "#,##0")
  # 
  # integerCols <- grep("DataPackTarget", final_snuxim_cols)
  # 
  # openxlsx::addStyle(
  #   wb = d$tool$wb,
  #   sheet = "PSNUxIM",
  #   integerStyle,
  #   rows = (top_rows + 1):(existing_rows + NROW(data_structure)),
  #   cols = integerCols,
  #   gridExpand = TRUE,
  #   stack = TRUE)
        
  
  # Consider adding errorStyling here to emphasize where incorrect disaggs entered.
  errorStyle <- openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  warningStyle <- openxlsx::createStyle(fontColour = "#9C5700", bgFill = "#FFEB9C")
  normalStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FFFFFF")
        
  # Hide rows 5-13 ####
  openxlsx::setRowHeights(wb = d$tool$wb,
                          sheet = "PSNUxIM",
                          rows = 4:(top_rows-1),
                          heights = 0)
        
  # Hide columns ####
  hiddenCols <- datapackr::cop21_data_pack_schema %>%
    dplyr::filter(sheet_name == "PSNUxIM",
                  indicator_code %in% c("ID", "sheet_num", "DSD Dedupe",
                                        "TA Dedupe", "Crosswalk Dedupe")) %>%
    dplyr::pull(col)
  
  openxlsx::setColWidths(wb = d$tool$wb,
                         sheet = "PSNUxIM",
                         cols = hiddenCols,
                         hidden = TRUE)
        
  # Tab generation date ####
  openxlsx::writeData(d$tool$wb, "PSNUxIM",
                      paste("Generated on:", Sys.time()),
                      xy = c(1,2),
                      colNames = F)
        
  # Package Version ####
  openxlsx::writeData(d$tool$wb, "PSNUxIM",
                      paste("Package version:",
                            as.character(utils::packageVersion("datapackr"))),
                      xy = c(2,2),
                      colNames = F)
  
  # Warning Messages ####
  warning_msg <- 
    paste0(
      "NOTE: Based on your submission, we have ",
      ifelse(d$info$has_psnuxim,
             paste0("added ", NROW(data_structure), " rows to your PSNUxIM tab.",
                     " These have been highlighted green for your reference."),
             "populated your PSNUxIM tab for the first time."),
      " An updated copy of your Data Pack is available for download from this app.",
      " Please review your PSNUxIM tab and carefully review the Data Pack User Guide
  for detailed guidance on how to use this tab.
  
NOTE: DO NOT delete any columns in this tool, and do not add any new columns between
existing columns.
        
NOTE: Any external references used in cell formulas will now be corrupt and
cause '#N/A' errors. Please review your Data Pack for these cases and correct.
      
If you have any questions, please submit a Help Desk ticket at DATIM.Zendesk.com.",
      "\n")
        
  d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  
  return(d)

}
