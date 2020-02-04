#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom stats complete.cases
#' @title packSNUxIM(data)
#'
#' @description Packs SNUxIM data prepared from unPackSNUxIM for import to DATIM.
#'
#' @param d Datapackr object
#' @param snuxim_model_data_path Filepath where SNU x IM distribution model is stored.
#' @param output_folder Local folder where you would like your Data Pack to be
#' saved upon export.
#' 
#' @return d
#' 
packSNUxIM <- function(d, snuxim_model_data_path, output_folder) {

  # Check if SNUxIM data already exists ####
  if (NROW(d$data$SNUxIM) > 0) {
  
  # If does exist, check what combos are missing and write these in ####
    
  
  # If doesn't exist, write all combos in ####
  } else {
    # Prepare SNU x IM model dataset ####
      snuxim_model_data <- readRDS(snuxim_model_data_path) %>%
        dplyr::select(-value, -age_option_uid, -sex_option_uid, -kp_option_uid)
    
    # Combine with MER data ####
      dsd_ta <- tibble::tribble(
        ~type,
        "DSD",
        "TA"
      )
      
      d$data$SNUxIM_combined <- d$data$MER %>%
        tidyr::crossing(dsd_ta) %>%
        dplyr::left_join(
          snuxim_model_data,
          by = c("psnuid" = "psnu_uid",
                 "indicator_code" = "indicator_code",
                 "Age" = "age_option_name",
                 "Sex" = "sex_option_name",
                 "KeyPop" = "kp_option_name",
                 "type" = "type")) %>%
        dplyr::arrange(mechanism_code, type) %>%
        dplyr::filter(!mechanism_code %in% c('00000','00001')) %>%
        tidyr::pivot_wider(names_from = c(mechanism_code, type),
                           values_from = percent) %>%
        dplyr::select_at(dplyr::vars(-tidyselect::one_of("NA_TA","NA_DSD"),
                                     -psnuid, -sheet_name))

    # Add ID & sheet_num formulas ####
      d$data$SNUxIM_combined %<>%
        dplyr::left_join(
          datapackr::cop20_data_pack_schema %>%
            dplyr::filter(dataset == "mer", col_type == "target") %>%
            dplyr::select(indicator_code, sheet_num, col),
          by = "indicator_code") %>%
        dplyr::arrange(PSNU, sheet_num, col, Age, Sex, KeyPop) %>%
        dplyr::mutate(
          row = (1:dplyr::n()) + headerRow("Data Pack Template", cop_year),
          ID = paste0('$A',row,'&IF($C',row,'<>"","|"&$C',row,',"")&IF($D',row,
                      '<>"","|"&$D',row,',"")&IF($E',row,'<>"","|"&$E',row,',"")'),
          sheet_num = dplyr::case_when(
            indicator_code == "OVC_HIVSTAT.N.total.T" ~ 15,
            TRUE ~ sheet_num - 8),
        
    # Add Data Pack total formula ####
          DataPackTarget = paste0(
           'ROUND(SUMIF(CHOOSE($G',row,
           ',TX!$D:$D,HTS!$D:$D,TB_STAT_ART!$D:$D,PMTCT_STAT_ART!$D:$D,PMTCT_EID!$A:$A,VMMC!$D:$D,CXCA!$D:$D,HTS_RECENT!$D:$D,TX_TB_PREV!$D:$D,KP!$C:$C,PP!$D:$D,OVC!$D:$D,PrEP!$D:$D,GEND!$A:$A,OVC!$A:$A),$F',row,
           ',INDIRECT($B',row,')),0)'),
    
    # Add Dedupe formula ####
          Dedupe = paste0('IF($I',row,'>100%,1-$I',row,',0)')
        )
      
      # Add Rollup check formula ####
      colCount <- NCOL(d$data$SNUxIM_combined)
      
      d$data$SNUxIM_combined %<>%
        dplyr::mutate(
          Rollup = paste0('SUM($K',row,':$',openxlsx::int2col(colCount),row,')')) %>%
        dplyr::select(
          PSNU, indicator_code, Age, Sex, KeyPop, ID, sheet_num, DataPackTarget,
          Rollup, Dedupe, dplyr::everything(), -row, -value, -col) %>%
        
    # Alter Ages and Sexes as needed
        dplyr::mutate(
          Age = dplyr::case_when(
            indicator_code %in% c("PMTCT_EID.N.Age.T.2mo","PMTCT_EID.N.Age.T.2to12mo") ~ NA_character_,
            TRUE ~ Age)
        )
      
    # Format formula columns ####
      formulaCols <- grep("ID|DataPackTarget|Rollup|Dedupe",
                          colnames(d$data$SNUxIM_combined))
      
      for (i in formulaCols) {
          class(d$data$SNUxIM_combined[[i]]) <- c(class(d$data$SNUxIM_combined[[i]]), "formula")
      }
      
    # Write data to sheet ####
      d$tool$wb <- openxlsx::loadWorkbook(d$keychain$submission_path)
      
      sheets_with_filters <- cop20_data_pack_schema %>%
        dplyr::filter(data_structure == "normal") %>%
        dplyr::pull(sheet_num) %>%
        unique()
      
      openxlsx::removeFilter(d$tool$wb, sheets_with_filters)
      
      openxlsx::writeData(wb = d$tool$wb,
                          sheet = "PSNUxIM",
                          x = d$data$SNUxIM_combined,
                          xy = c(1, headerRow("Data Pack", cop_year)),
                          colNames = T, rowNames = F, withFilter = FALSE)
      
    # Format percent columns ####
      percentCols <- grep("Rollup|Dedupe|_(DSD|TA)$",
                          colnames(d$data$SNUxIM_combined))
      percentStyle = openxlsx::createStyle(numFmt = "0%")
      
      openxlsx::addStyle(
        wb = d$tool$wb,
        sheet = "PSNUxIM",
        percentStyle,
        rows = (1:NROW(d$data$SNUxIM_combined)) + headerRow("Data Pack", cop_year),
        cols = percentCols,
        gridExpand = TRUE,
        stack = FALSE)
    
    # Format integers ####
      integerCols <- grep("DataPackTarget", colnames(d$data$SNUxIM_combined))
      integerStyle = openxlsx::createStyle(numFmt = "#,##0")

      openxlsx::addStyle(
        wb = d$tool$wb,
        sheet = "PSNUxIM",
        integerStyle,
        rows = (1:NROW(d$data$SNUxIM_combined)) + headerRow("Data Pack", cop_year),
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
                                      cols = 9,
                                      rows = (1:NROW(d$data$SNUxIM_combined) + 14),
                                      rule = "AND(ISNUMBER($I15),ROUND($I15,2)<>1)",
                                      style = errorStyle)
      
      openxlsx::conditionalFormatting(wb = d$tool$wb,
                                      sheet = "PSNUxIM",
                                      cols = 10,
                                      rows = (1:NROW(d$data$SNUxIM_combined) + 14),
                                      rule = "AND(ISNUMBER($J15),ROUND($J15,2)<>0)",
                                      style = warningStyle)
      
      openxlsx::conditionalFormatting(wb = d$tool$wb,
                                      sheet = "PSNUxIM",
                                      cols = 10,
                                      rows = (1:NROW(d$data$SNUxIM_combined) + 14),
                                      rule = "AND(ISNUMBER($J15),ROUND($J15,2)=0)",
                                      style = normalStyle)
      
    # Hide rows 5-13 ####
      openxlsx::setRowHeights(wb = d$tool$wb,
                              sheet = "PSNUxIM",
                              rows = 5:13,
                              heights = 0)
      
    # Hide ID and sheet_num columns ####
      hiddenCols <- grep("ID|sheet_num",
                          colnames(d$data$SNUxIM_combined))
      
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
      
    # Export SNU x IM Data Pack ####
      exportPackr(data = d$tool$wb,
                  output_path = output_folder,
                  type = "Data Pack",
                  datapack_name = d$info$datapack_name)
      
  }
  
  
  data <- data %>%
    dplyr::mutate(
      period = datapackr::periodInfo$iso) %>% 
    dplyr::left_join(datapackr::PSNUxIM_to_DATIM %>% #TODO: Build PSNUxIM_to_DATIM from API call.
                       dplyr::filter(dataset == "MER") %>%
                       dplyr::select(-sheet_name, -typeOptions, -dataset),
                     by = c("indicator_code" = "indicatorCode",
                            "Age" = "validAges",
                            "Sex" = "validSexes",
                            "KeyPop" = "validKPs")) %>%
    # Under COP19 requirements, after this join, TX_PVLS N will remain NA for dataelementuid and categoryoptioncombouid
    # Select and rename based on DATIM protocol
    dplyr::select(
      dataElement = dataelementuid,
      period,
      orgUnit = psnuid,
      categoryOptionCombo = categoryoptioncombouid,
      mechanism_code,
      value) %>%
    dplyr::group_by(dataElement, period, orgUnit,categoryOptionCombo,
                    mechanism_code) %>% #TODO: Coordinate with self-service on this name change
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
  # Coerce decimals to integers now
    dplyr::mutate(value = round_trunc(value)) %>%
  # Remove anything which is NA here. Under COP19 guidance, this will include only TX_PVLS.N.Age/Sex/Indication/HIVStatus.20T.Routine
    dplyr::filter(complete.cases(.))
  
  return(d)

}