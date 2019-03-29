#' @export
#' @title Compare Data Pack and Site Tool data
#' 
#' @description 
#' Takes a Data Pack and a Site Tool and unpacks both for easy comparison of
#' target totals.
#' 
#' @param datapack_path Local filepath leading to Data Pack to analyze.
#' @param sitetool_path Local filepath leading to Site Tool to analyze.
#' @param output_path Local folder where you want output written.
#' 
#' @return Log of differences in target totals.
#' 
comparePacks <- function(datapack_path, sitetool_path, output_path) {
  
  dp <- unPackData(submission_path = datapack_path)
  
  st <- unPackSiteToolData(submission_path = sitetool_path)
  
  # Check these are for the same Countries
  if(dp$info$datapack_uid != st$info$datapack_uid) {
    stop("These Countries are apples and oranges. Recheck your fruit.")
  }
  
  # Aggregate Site Tool data
    country_uids <- datapackr::dataPackMap %>%
      dplyr::filter(model_uid == st$info$datapack_uid) %>%
      dplyr::pull(country_uid) %>%
      unique()
    
    siteList <- getSiteList(country_uids, include_mil = TRUE)
    
    sitetool_data <- st$data$targets %>%
      dplyr::filter(indicatorCode != "VMMC_CIRC.N.Age/Sex.20T") %>%
      dplyr::left_join(siteList %>%
                         dplyr::select(id, psnu, psnu_uid, country_name, country_uid),
                       by = c("site_uid" = "id")) %>%
      dplyr::select(-Type, -sheet_name, -Site, -site_uid) %>%
      dplyr::group_by(country_name, country_uid, psnu, psnu_uid, mech_code,
                      indicatorCode, Age, Sex, KeyPop) %>%
      dplyr::summarise(value.sitetool = sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::select(country_name, country_uid, psnu, psnu_uid,
                    indicatorCode, Age, Sex, KeyPop, mech_code,
                    value.sitetool)
    
  # Prepare Data Pack data
    datapack_data <- dp$data$distributedMER %>%
      dplyr::left_join(siteList %>%
                         dplyr::select(psnu, psnu_uid, country_name, country_uid) %>%
                         dplyr::distinct(),
                       by = c("psnuid" = "psnu_uid")) %>%
      dplyr::mutate(valueRounded.datapack = round_trunc(value),
                    Age =
                      dplyr::case_when(
                        stringr::str_detect(indicatorCode, "PMTCT_EID") ~ NA_character_,
                        TRUE ~ Age)) %>%
      dplyr::select(country_name, country_uid, psnu, psnu_uid = psnuid,
                    indicatorCode, Age, Sex, KeyPop, mech_code = mechanismCode,
                    value.datapack = value, valueRounded.datapack)
    
  # Compare
    comparison <- sitetool_data %>%
      dplyr::full_join(datapack_data,
                       by = c("country_name",
                              "country_uid",
                              "psnu",
                              "psnu_uid",
                              "indicatorCode",
                              "Age",
                              "Sex",
                              "KeyPop",
                              "mech_code")) %>%
      dplyr::mutate(
        diff = (value.sitetool - value.datapack)/value.datapack,
        diffRounded = (value.sitetool - valueRounded.datapack)/valueRounded.datapack) %>%
      dplyr::select(country_name, country_uid, psnu, psnu_uid,
                    indicatorCode, Age, Sex, KeyPop, mech_code,
                    value.datapack, valueRounded.datapack, value.sitetool,
                    diff, diffRounded) %>%
      dplyr::arrange(country_name, psnu, indicatorCode, Age, Sex, KeyPop, mech_code)
    
  # Isolate diffs
    diffs <- comparison %>%
      dplyr::filter(abs(diffRounded) >= 0.05 | is.na(diffRounded)) %>%
      dplyr::mutate(
        category =
          dplyr::case_when(
            valueRounded.datapack == 0 & is.na(value.sitetool) ~ "Near-zero target dropped",
            valueRounded.datapack == 0 & value.sitetool == 0 ~ "Near-zero target dropped",
            valueRounded.datapack == 0 & value.sitetool == 1 ~ "Near-zero target rounded to one",
            !is.na(value.sitetool) & is.na(valueRounded.datapack) ~ "Target introduced",
            is.na(value.sitetool) & !is.na(valueRounded.datapack) ~ "Target removed",
            diff > 0 ~ "Target increased",
            diff < 0 ~ "Target decreased"
          )
      )
    
  # Summarize diffs
    summary.categories <- diffs %>%
      dplyr::group_by(category) %>%
      dplyr::summarise(count = dplyr::n()) %>%
      dplyr::ungroup()
    
    summary.indicators <- comparison %>%
      dplyr::select(indicatorCode, value.datapack,
                    valueRounded.datapack, value.sitetool) %>%
      tidyr::replace_na(list(value.datapack = 0,
                             valueRounded.datapack = 0,
                             value.sitetool = 0)
                        )%>%
      dplyr::group_by(indicatorCode) %>%
      dplyr::summarise(value.datapack = sum(value.datapack),
                       valueRounded.datapack = sum(valueRounded.datapack),
                       value.sitetool = sum(value.sitetool)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(diff = (value.sitetool - value.datapack)/value.datapack) %>%
      dplyr::arrange(dplyr::desc(abs(diff)))
     
  # Write to Excel Document
    wb <- openxlsx::createWorkbook()
    
    sheetNames <- c("Data Pack Data", "Site Tool Data", "Comparison", "Diffs",
                    "Category Summary", "Indicator Summary")
    
    invisible(sapply(sheetNames, function(x) openxlsx::addWorksheet(wb, sheetName = x)))
    
    openxlsx::writeData(wb, sheet = 1, x = datapack_data)
    openxlsx::writeData(wb, sheet = 2, x = sitetool_data)
    openxlsx::writeData(wb, sheet = 3, x = comparison)
    openxlsx::writeData(wb, sheet = 4, x = diffs)
    openxlsx::writeData(wb, sheet = 5, x = summary.categories)
    openxlsx::writeData(wb, sheet = 6, x = summary.indicators)
    

    filename = paste0(
      output_path,
      if (is.na(stringr::str_extract(output_path,"/$"))) {"/"} else {},
      "DataPack_SiteTool_Comparison_",
      st$info$datapack_name,
      "_",
      format(Sys.time(), "%Y%m%d%H%M%S"),
      ".xlsx"
    )
        
    openxlsx::saveWorkbook(wb, file = filename)
      
}
