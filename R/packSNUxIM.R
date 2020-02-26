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
  if ( d$info$cop_year == 2020 ) {
  # Check if SNUxIM data already exists ####
    if (NROW(d$data$SNUxIM) == 1 & is.na(d$data$SNUxIM[[1,1]])) {
      d$info$has_psnuxim <- FALSE
    } else {d$info$has_psnuxim <- TRUE}
  
  # If does exist, check what combos are missing ####
    if (d$info$has_psnuxim) {
      d$data$missingCombos <- d$data$MER %>%
        dplyr::anti_join(d$data$PSNUxIM_combos)
      
      d$info$missing_psnuxim_combos <- (NROW(d$data$missingCombos) > 0)
    }
    
  # Proceed IFF no PSNU x IM tab exists, or exists but with missing combos ####
    if (!d$info$has_psnuxim | d$info$missing_psnuxim_combos) {
  
      # Prepare SNU x IM model dataset ####
      d$data$snuxim_model_data <- readRDS(d$keychain$snuxim_model_data_path)[d$info$country_uids] %>%
        dplyr::bind_rows()
      
      # Cross with DSD/TA ####
        dsd_ta <- tibble::tribble(
          ~type,
          "DSD",
          "TA"
        )
        
      if (d$info$has_psnuxim & d$info$missing_psnuxim_combos) {
        data <- d$data$missingCombos
      } else {
        data <- d$data$MER
      }
      
        d$data$SNUxIM_combined <- data %>%
          tidyr::crossing(dsd_ta)
      
      # Combine MER data with SNUxIM model data ####
        if (NROW(d$data$snuxim_model_data) > 0) {
          d$data$snuxim_model_data %<>%
            dplyr::select(-value, -age_option_uid, -sex_option_uid, -kp_option_uid)
          
          d$data$SNUxIM_combined %<>%
            dplyr::left_join(
              d$data$snuxim_model_data,
              by = c("psnuid" = "psnu_uid",
                     "indicator_code" = "indicator_code",
                     "Age" = "age_option_name",
                     "Sex" = "sex_option_name",
                     "KeyPop" = "kp_option_name",
                     "type" = "type"))
        } else {
          d$data$SNUxIM_combined %<>%
            dplyr::mutate(
              mechanism_code = NA_character_,
              percent = NA
            )
        }
        
        d$data$SNUxIM_combined %<>%
          dplyr::arrange(mechanism_code, type) %>%
          dplyr::filter(!mechanism_code %in% c('00000','00001')) %>%
          tidyr::pivot_wider(names_from = c(mechanism_code, type),
                             values_from = percent) %>%
          dplyr::select_at(dplyr::vars(-tidyselect::one_of("NA_TA","NA_DSD"),
                                       -psnuid, -sheet_name)) %>%
          
      # Allow DataPackTarget formula to lookup KP_MAT correctly ####
          dplyr::mutate(
            KeyPop = dplyr::case_when(
              indicator_code == "KP_MAT.N.Sex.T" ~ paste0(Sex, " PWID"),
              TRUE ~ KeyPop),
            Sex = dplyr::case_when(indicator_code == "KP_MAT.N.Sex.T" ~ NA_character_,
                                   TRUE ~ Sex)
          )
  
      # Add ID & sheet_num formulas ####
      top_rows <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)
      header_cols <- d$info$schema %>%
        dplyr::filter(sheet_name == "PSNUxIM"
                      & (col_type == "row_header"
                         | indicator_code %in% c("DataPackTarget","Rollup","Dedupe"))) %>%
        dplyr::pull(indicator_code)
      
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
            range = readxl::cell_limits(c(1,1), c(NA,1)),
            col_names = F
          ) %>%
          NROW()
        
        existing_rows <- SNUxIM_rows
        first_new_mech_col <- NCOL(SNUxIM_cols) + 1
          
      } else {
        SNUxIM_cols <- d$info$schema %>%
          dplyr::filter(sheet_name == "PSNUxIM",
                        !indicator_code %in% c("12345_DSD","12345_TA")) %>%
          dplyr::select(indicator_code) %>%
          `row.names<-`(.[, 1]) %>%
          t() %>%
          tibble::as_tibble()
          
        existing_rows <- top_rows
        first_new_mech_col <- length(header_cols) + 1
      }
        
        d$data$SNUxIM_combined %<>%
          dplyr::left_join(
            datapackr::cop20_data_pack_schema %>%
              dplyr::filter(dataset == "mer", col_type == "target") %>%
              dplyr::select(indicator_code, sheet_num, col),
            by = "indicator_code") %>%
          dplyr::arrange(PSNU, sheet_num, col, Age, Sex, KeyPop) %>%
          dplyr::mutate(
            row = (1:dplyr::n()) + existing_rows,
            ID = paste0('$A',row,'&IF($C',row,'<>"","|"&$C',row,',"")&IF($D',row,
                        '<>"","|"&$D',row,',"")&IF($E',row,'<>"","|"&$E',row,',"")'),
            sheet_num = dplyr::case_when(
              indicator_code == "OVC_HIVSTAT.N.total.T" ~ 15,
              TRUE ~ sheet_num - 8))
          
      # Add Data Pack total formula ####
        get_ID_col <- function(data) {
          col_letter <- data %>%
            dplyr::filter(indicator_code == "ID") %>%
            dplyr::pull(submission_order) %>%
            openxlsx::int2col()
          
          if (length(col_letter) == 0) {
            col_letter <- data %>%
              dplyr::filter(indicator_code == "PSNU") %>%
              dplyr::pull(submission_order) %>%
              openxlsx::int2col()
          }
          
          return(col_letter)
        }
        
        col_letters <- lapply(d$info$col_check, get_ID_col)
        
        compile_formula_ref <- function(sheet_name) {
          if (!sheet_name %in% c("Epi Cascade I", "Epi PMTCT", "Prioritization", "PSNUxIM")) {
            paste0(sheet_name, "!$", col_letters[[sheet_name]],
                   ":$", col_letters[[sheet_name]], ",")
          }
        }
        
        OVC_HIVSTAT_col_letter <- d$info$col_check$OVC %>%
          dplyr::filter(indicator_code == "PSNU") %>%
          dplyr::pull(submission_order) %>%
          openxlsx::int2col()
        
        formula_refs_compiled <- lapply(names(d$info$col_check), compile_formula_ref) %>%
          unlist() %>%
          paste(collapse = "") %>%
          paste0(., "OVC!$",OVC_HIVSTAT_col_letter,":$",OVC_HIVSTAT_col_letter)
        
        d$data$SNUxIM_combined %<>%
          dplyr::mutate(
            DataPackTarget = paste0(
             'ROUND(SUMIF(CHOOSE($G',row,
             ',',formula_refs_compiled,'),$F',row,
             ',INDIRECT($B',row,')),0)'),
      
      # Add Dedupe formula ####
            Dedupe = paste0('IF($I',row,'>100%,1-$I',row,',0)'),
      
      # Add Rollup check formula
            Rollup = NA_character_
          ) %>%
          dplyr::select(PSNU, indicator_code, Age, Sex, KeyPop, ID, sheet_num,
                        DataPackTarget, Rollup, Dedupe, dplyr::everything(), -value, -col)
        
      # Add Rollup check formula ####
        new_mech_cols <- names(d$data$SNUxIM_combined)[!names(d$data$SNUxIM_combined) %in% c(names(SNUxIM_cols), "row")]
        non_appended_mech_cols <- names(SNUxIM_cols)[!names(SNUxIM_cols) %in% names(d$data$SNUxIM_combined)]
        
        d$data$SNUxIM_combined %<>%
          dplyr::mutate(
            Rollup = paste0(
              'SUM($', openxlsx::int2col(length(header_cols) + 1), row,
              ':$',
              openxlsx::int2col(
                max(12,
                    first_new_mech_col - 1 + length(new_mech_cols)
                    )
                ),
              row,')')) %>%
          dplyr::select(-row) %>%
          
      # Alter Ages and Sexes as needed
          dplyr::mutate(
            Age = dplyr::case_when(
              indicator_code %in% c("PMTCT_EID.N.Age.T.2mo","PMTCT_EID.N.Age.T.2to12mo") ~ NA_character_,
              TRUE ~ Age)
          ) %>%
          {if (length(non_appended_mech_cols) > 0) {
            (.) %>% addcols(non_appended_mech_cols)
            } else {.}} %>%
          dplyr::select(names(SNUxIM_cols), new_mech_cols)
        
      # Format formula columns ####
        formulaCols <- grep("ID|DataPackTarget|Rollup|Dedupe",
                            colnames(d$data$SNUxIM_combined))
        
        for (i in formulaCols) {
            class(d$data$SNUxIM_combined[[i]]) <- c(class(d$data$SNUxIM_combined[[i]]), "formula")
        }
        
      # Write data to sheet ####
        d$tool$wb <- openxlsx::loadWorkbook(d$keychain$submission_path)
        openxlsx::removeFilter(d$tool$wb, names(d$tool$wb))
        
        if (d$info$has_psnuxim & d$info$missing_psnuxim_combos) {
          # header_data <- d$data$SNUxIM_combined %>%
          #   dplyr::select(header_cols)
          # 
          # mech_data <- d$data$SNUxIM_combined %>%
          #   dplyr::select(-header_cols)
          # 
          # mech_names <- names(mech_data) %>%
          #   as.data.frame() %>%
          #   `row.names<-`(.[, 1]) %>%
          #   t() %>%
          #   tibble::as_tibble()
          # 
          # openxlsx::writeData(wb = d$tool$wb,
          #                     sheet = "PSNUxIM",
          #                     x = header_data,
          #                     xy = c(1, existing_rows + 1),
          #                     colNames = F, rowNames = F, withFilter = FALSE)
          # 
          # openxlsx::writeData(wb = d$tool$wb,
          #                     sheet = "PSNUxIM",
          #                     x = mech_data,
          #                     xy = c(first_new_mech_col, existing_rows + 1),
          #                     colNames = F, rowNames = F, withFilter = FALSE)
          # 
          # openxlsx::writeData(wb = d$tool$wb,
          #                     sheet = "PSNUxIM",
          #                     x = mech_names,
          #                     xy = c(first_new_mech_col, top_rows),
          #                     colNames = F, rowNames = F, withFilter = FALSE)
          
          openxlsx::writeData(wb = d$tool$wb,
                              sheet = "PSNUxIM",
                              x = d$data$SNUxIM_combined,
                              xy = c(1, existing_rows + 1),
                              colNames = F, rowNames = F, withFilter = FALSE)
          
          d$info$newSNUxIM <- TRUE
          
          # Add additional col_names if any
          openxlsx::writeData(wb = d$tool$wb,
                              sheet = "PSNUxIM",
                              x = new_mech_cols,
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
          openxlsx::writeData(wb = d$tool$wb,
                              sheet = "PSNUxIM",
                              x = d$data$SNUxIM_combined,
                              xy = c(1, top_rows),
                              colNames = T, rowNames = F, withFilter = FALSE)
          
          d$info$newSNUxIM <- TRUE
          
        }
        
      # Format percent columns ####
        final_snuxim_cols <- 
          openxlsx::readWorkbook(
            xlsxFile = d$tool$wb,
            sheet = "PSNUxIM",
            rows = top_rows
          ) %>%
          names()
        
        percentCols <- c(grep("Rollup", final_snuxim_cols):length(final_snuxim_cols))
        percentStyle = openxlsx::createStyle(numFmt = "0%")
        
        openxlsx::addStyle(
          wb = d$tool$wb,
          sheet = "PSNUxIM",
          percentStyle,
          rows = (top_rows + 1):(existing_rows + NROW(d$data$SNUxIM_combined)),
          cols = percentCols,
          gridExpand = TRUE,
          stack = FALSE)
      
      # Format integers ####
        integerCols <- grep("DataPackTarget", final_snuxim_cols)
        integerStyle = openxlsx::createStyle(numFmt = "#,##0")
  
        openxlsx::addStyle(
          wb = d$tool$wb,
          sheet = "PSNUxIM",
          integerStyle,
          rows = (top_rows + 1):(existing_rows + NROW(d$data$SNUxIM_combined)),
          cols = integerCols,
          gridExpand = TRUE,
          stack = TRUE)
        
      # Add Conditional Formatting for Ages and Sexes ####
        errorStyle <- openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
        warningStyle <- openxlsx::createStyle(fontColour = "#9C5700", bgFill = "#FFEB9C")
        normalStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FFFFFF")
        
        # Consider adding errorStyling here to emphasize where incorrect disaggs entered.
        
      # Alter conditional formatting for Rollup and Dedupe ####
        openxlsx::conditionalFormatting(wb = d$tool$wb,
                                        sheet = "PSNUxIM",
                                        cols = grep("Rollup", final_snuxim_cols),
                                        rows = (top_rows+1):(existing_rows + NROW(d$data$SNUxIM_combined)),
                                        rule = "AND(ISNUMBER($I15),ROUND($I15,2)<>1)",
                                        style = errorStyle)
        
        openxlsx::conditionalFormatting(wb = d$tool$wb,
                                        sheet = "PSNUxIM",
                                        cols = grep("Dedupe", final_snuxim_cols),
                                        rows = (top_rows+1):(existing_rows + NROW(d$data$SNUxIM_combined)),
                                        rule = "AND(ISNUMBER($J15),ROUND($J15,2)<>0)",
                                        style = warningStyle)
        
        openxlsx::conditionalFormatting(wb = d$tool$wb,
                                        sheet = "PSNUxIM",
                                        cols = grep("Dedupe", final_snuxim_cols),
                                        rows = (top_rows+1):(existing_rows + NROW(d$data$SNUxIM_combined)),
                                        rule = "AND(ISNUMBER($J15),ROUND($J15,2)=0)",
                                        style = normalStyle)
        
      # Format mechanism columns ####
        colCount <- length(final_snuxim_cols)
        
        mechColHeaders <- openxlsx::createStyle(fontSize = 11,
                                                halign = "center",
                                                valign = "center",
                                                textRotation = 90,
                                                fgFill = "#9CBEBD",
                                                textDecoration = "bold")
        
        openxlsx::addStyle(d$tool$wb,
                           sheet = "PSNUxIM",
                           style = mechColHeaders,
                           rows = top_rows,
                           cols = (length(header_cols)+1):colCount,
                           gridExpand = TRUE,
                           stack = FALSE)
        
      # Hide rows 5-13 ####
        openxlsx::setRowHeights(wb = d$tool$wb,
                                sheet = "PSNUxIM",
                                rows = 5:(top_rows-1),
                                heights = 0)
        
      # Hide ID and sheet_num columns ####
        hiddenCols <- grep("ID|sheet_num", final_snuxim_cols)
        
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
        
        warning_msg <- 
          paste0(
            "NOTE: Based on your submission, we have ",
            ifelse(d$info$has_psnuxim,
                   paste0("added ", NROW(d$data$SNUxIM_combined), " rows to your PSNUxIM tab.",
                           " These have been highlighted green for your reference."),
                   "populated your PSNUxIM tab for the first time."),
            " An updated copy of your Data Pack is available for download from this app.",
            " Please review your PSNUxIM tab and note the following:\n\n",
            "\t1) To add new mechanisms to your PSNUxIM tab, either type over an
            existing mechanism name, or add a new column to the right of your existing
            mechanism columns. All mechanism names must be of the format 12345_DSD
            or 12345_TA, and every mechanism must also be entered and up to date
            in FACTS Info. Finally, ensure the formula in the Rollup column
            extends to include all mechanism columns that you add.
            
        2) It is critical that no manual modifications are made to the PSNUxIM
            tab other than:
            
              a. adding additional mechanism columns,
              b. modifying the Rollup column to include additional mechanism columns, and
              c. adding/updating the percent allocations to mechanisms.
              
        3) Any external references used in cell formulas will now be corrupt and
        cause '#N/A' errors. Please review your Data Pack for these cases and correct.
            
If you have any questions, please submit a Help Desk ticket at DATIM.Zendesk.com.",
            "\n")
        
        d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
      
    }
  } else if (d$info$cop_year == 2019) {
    stop("Packing SNU x IM tabs is no longer supported for FY2019 Data Packs.")
  #   data <- data %>%
  #     dplyr::mutate(
  #       period = datapackr::periodInfo$iso) %>% 
  #     dplyr::left_join(datapackr::PSNUxIM_to_DATIM %>% #TODO: Build PSNUxIM_to_DATIM from API call.
  #                        dplyr::filter(dataset == "MER") %>%
  #                        dplyr::select(-sheet_name, -typeOptions, -dataset),
  #                      by = c("indicator_code" = "indicatorCode",
  #                             "Age" = "validAges",
  #                             "Sex" = "validSexes",
  #                             "KeyPop" = "validKPs")) %>%
  #     # Under COP19 requirements, after this join, TX_PVLS N will remain NA for dataelementuid and categoryoptioncombouid
  #     # Select and rename based on DATIM protocol
  #     dplyr::select(
  #       dataElement = dataelementuid,
  #       period,
  #       orgUnit = psnuid,
  #       categoryOptionCombo = categoryoptioncombouid,
  #       mechanism_code,
  #       value) %>%
  #     dplyr::group_by(dataElement, period, orgUnit,categoryOptionCombo,
  #                     mechanism_code) %>% #TODO: Coordinate with self-service on this name change
  #     dplyr::summarise(value = sum(value)) %>%
  #     dplyr::ungroup() %>%
  #   # Coerce decimals to integers now
  #     dplyr::mutate(value = round_trunc(value)) %>%
  #   # Remove anything which is NA here. Under COP19 guidance, this will include only TX_PVLS.N.Age/Sex/Indication/HIVStatus.20T.Routine
  #     dplyr::filter(complete.cases(.))
  }
  
  return(d)

}
