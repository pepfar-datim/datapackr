library(tidyverse)


unPackStructure <- function(filepath) {

  sheets <- tidyxl::xlsx_sheet_names(filepath)
  sheets_to_loop <- sheets[which(!stringr::str_detect(sheets, "Home|Quotes|Summary|Spectrum|Visualizations|Validations"))]

  schema <- tidyxl::xlsx_cells(path = filepath, include_blank_cells = FALSE) %>%
    dplyr::select(sheet_name = sheet, col, row, character, formula, numeric)

  data.table::setDT(schema)[,sheet_num:=.GRP, by = c("sheet_name")]

  schema %<>%
    dplyr::filter(sheet_name %in% sheets_to_loop,
                  row %in% c(3,5,6)) %>%
    tidyr::gather(key,value,-sheet_num,-sheet_name,-col,-row) %>%
    tidyr::unite(new.col, c(key,row)) %>%
    tidyr::spread(new.col,value) %>%
    dplyr::select(sheet_num,sheet_name,col,
                  label = character_3,
                  indicator_code = character_5,
                  formula = formula_6,
                  value = numeric_6) %>%
    dplyr::mutate(
      formula = dplyr::case_when(is.na(formula) ~ value,
                                 TRUE ~ formula),
      col_type = dplyr::case_when(
        stringr::str_detect(indicator_code, "20T")
          & !(sheet_name == "Prioritization" & indicator_code != "IMPATT.PRIORITY_SNU.20T")
          & !(sheet_name == "HTS" & (!stringr::str_detect(indicator_code,"^HTS_") | stringr::str_detect(indicator_code,"HTS_TST_PMTCT")))
          & !(sheet_name == "VMMC" & stringr::str_detect(indicator_code, "POP_EST|coverage"))
          ~ "Target",
        indicator_code %in% c("PSNU","Age","Sex","ID","AgeCoarse","IDAgeCoarse",
                             "PSNUCheck","KeyPop","sheet_name","indicatorCode",
                             "CoarseAge","sheet_num")
          ~ "Row Header"),
      dataset = dplyr::case_when(
        col_type == "Target" & stringr::str_detect(indicator_code,"SUBNAT|VL_SUPPRESSED") ~ "SUBNAT",
        col_type == "Target" & stringr::str_detect(indicator_code,"PLHIV|POP_EST|HIV_PREV|PRIORITY|KP_ESTIMATES") ~ "IMPATT",
        col_type == "Target" ~ "MER")) %>%
    dplyr::select(-value) %>%
    dplyr::arrange(sheet_num, col)

  return(schema)
}

validDPDisaggs <- function() {

    validDisaggs <- list(
        "Epi Cascade I" = list(
            validAges = c("<01","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
            validSexes = c("Female","Male"),
            validKPs = NA_character_),
        "Epi Cascade II" = list(
            validAges = c("<15","15+"),
            validSexes = c("Female","Male"),
            validKPs = NA_character_),
        "Epi PMTCT" = list(
            validAges = NA_character_,
            validSexes = NA_character_,
            validKPs = NA_character_),
        "Prioritization" = list(
            validAges = NA_character_,
            validSexes = NA_character_,
            validKPs = NA_character_),
        "PMTCT_STAT_ART" = list(
            validAges = c("10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
            validSexes = c("Female"),
            validKPs = NA_character_),
        "PMTCT_EID" = list(
            validAges = NA_character_,
            validSexes = NA_character_,
            validKPs = NA_character_),
        "TB_STAT_ART" = list(
            validAges = c("<01","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
            validSexes = c("Female","Male"),
            validKPs = NA_character_),
        "VMMC" = list(
            validAges = c("10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
            validSexes = c("Male"),
            validKPs = NA_character_),
        "TX" = list(
            validAges = c("<01","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
            validSexes = c("Female","Male"),
            validKPs = NA_character_),
        "CXCA" = list(
            validAges = c("25-29","30-34","35-39","40-44","45-49"),
            validSexes = c("Female"),
            validKPs = NA_character_),
        "HTS" = list(
            validAges = c("01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
            validSexes = c("Female","Male"),
            validKPs = NA_character_),
        "TB_TX_PREV" = list(
            validAges = c("<15","15+"),
            validSexes = c("Female","Male"),
            validKPs = NA_character_),
        "OVC" = list(
            validAges = c("<01","01-04","05-09","10-14","15-17","18+"),
            validSexes = c("Female","Male"),
            validKPs = NA_character_),
        "KP" = list(
            validAges = NA_character_,
            validSexes = NA_character_,
            validKPs = c("Female PWID","Male PWID","PWID","FSW","MSM not SW","MSM SW","MSM","People in prisons and other enclosed settings","TG SW","TG not SW","TG")),
        "PP" = list(
            validAges = c("10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
            validSexes = c("Female","Male"),
            validKPs = NA_character_),
        "PrEP" = list(
            validAges = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
            validSexes = c("Female","Male"),
            validKPs = NA_character_),
        "GEND" = list(
            validAges = NA_character_,
            validSexes = NA_character_,
            validKPs = NA_character_)
    )

    return(validDisaggs)
}

prioritizationDict <- function() {
    dict <- tibble::tribble(
        ~value, ~Prioritization,
        1, "1 - Scale-up: Saturation",
        2, "2 - Scale-up: Aggressive",
        4, "4 - Sustained",
        5, "5 - Centrally Supported",
        6, "6 - Sustained: Commodities",
        7, "7 - Attained",
        8, "8 - Not PEPFAR Supported"
    )

    return(dict)
}

mapIndicators <- function() {

    disaggs <- NULL

    for (i in 1:length(validDPDisaggs())) {
        disaggs <- validDPDisaggs() %>%
            magrittr::extract2(i) %>%
            purrr::cross_df() %>%
            mutate(sheet =  (validDPDisaggs() %>%
                                 magrittr::extract(i) %>%
                                 names())) %>%
            select(sheet, everything()) %>%
            rbind(disaggs, .)
    }

    dsdTA <- tibble::tribble(
        ~dataset, ~typeOptions,
        "MER","DSD",
        "MER","TA"
    )

    indicators <- datapackr::template_schema %>%
        dplyr::filter(colType == "Target") %>%
        dplyr::select(-col, -sheet_num, -colType) %>%
        dplyr::left_join(disaggs, by = c("sheet_name" = "sheet")) %>%
        dplyr::left_join(dsdTA, by = c("dataset" = "dataset")) %>%
        readr::write_csv(path = "./data-raw/FY20TargetsUIDMap.csv", na = "")

    # Cross PSNUs x valid Disaggs for complete row setup
    rowStructure <- tidyr::crossing(d$data$PSNUs, disaggs) %>%
        select(org_unit_uid = uid, PSNU = DataPackSiteID, Age = validAges, Sex = validSexes, KeyPop = validKPs)

    # Add AgeCoarse
    if(d$data$sheet == "OVC") {
        rowStructure <- rowStructure %>%
            dplyr::mutate(AgeCoarse = dplyr::case_when(
                Age %in% c("<01","01-04","05-09","10-14","15-17","<18") ~ "<18",
                Age %in% c("18-24","25+","18+") ~ "18+"))
    } else {
        rowStructure <- rowStructure %>%
            dplyr::mutate(AgeCoarse = dplyr::case_when(
                Age %in% c("<01","01-04","05-09","10-14") ~ "<15",
                Age %in% c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+") ~ "15+"))
    }


}

produceConfig <- function() {
  # Load Country List
    configFile <- getCountries()
    
  # Add levels & prioritization details
    impattLevels <- getIMPATTLevels()
    
    configFile %<>%
      dplyr::left_join(impattLevels, by = c("country_name")) %>%
      dplyr::mutate_if(is.integer, as.double)
      
  # Add Mil names & UIDs & metadata
    militaryNodes <- datapackr::getMilitaryNodes() %>%
      dplyr::mutate(mil_in_datim = TRUE) %>%
      dplyr::select(-country_name, mil_level = level)
    
    configFile %<>%
    ## Building based on future state of all military nodes nested under country
      dplyr::left_join(militaryNodes, by= c("country_uid")) %>%
      dplyr::select(data_pack_name, model_uid, country_name, country_uid,
                    mil_psnu = name, mil_psnu_uid = id,
                    level3name, uidlevel3, level4name, uidlevel4,
                    country_in_datim, mil_in_datim, is_region,
                    country, prioritization, planning, community, facility,
                    mil_level) %>%
    ## Add Mil manually where not in DATIM currently
      dplyr::mutate(
        mil_psnu = dplyr::if_else(
          is.na(mil_psnu),
          paste0("_Military ",country_name),
          mil_psnu),
        mil_psnu_uid =
          dplyr::if_else(
            is.na(mil_psnu_uid), 
            paste0(
              "MIL",
              stringr::str_sub(
                stringr::str_replace_all(country_name," ",""),
                1,8
              )
            ),
            mil_psnu_uid
            ),
        mil_psnu_uid = stringr::str_pad(
          mil_psnu_uid,
          width = 11,
          side = "right",
          pad = 0)
        ) %>%
      tidyr::replace_na(list(mil_in_datim = FALSE, mil_level = 5))
    
    return(configFile)
    
}

loadStyleGuide <- function() {
  
  # Home Tab Styles ####
  home <- list(
    ## Home Tab Title
    title = openxlsx::createStyle(fontColour = "#000000",
                                  fontSize = 76,
                                  textDecoration = "bold",
                                  halign = "left",
                                  valign = "center"),
    ## Home Tab OU Name
    datapack_name = openxlsx::createStyle(fontColour = "#9CBEBD",
                                            fontSize = 64,
                                            textDecoration = "bold",
                                            halign = "left",
                                            valign = "center"),
    ## Home Tab PEPFAR banner
    pepfar = openxlsx::createStyle(fontColour = "#7F7F7F",
                                   fontSize = 36,
                                   halign = "left",
                                   valign = "center")
  )
  
  # Site Lists ####
  siteList <- list(
    community = openxlsx::createStyle(fontColour = "#000000",
                                      bgFill = "#EBF1DE"),
    facility = openxlsx::createStyle(fontColour = "#000000",
                                     bgFill = "#DCE6F1"),
    inactive = openxlsx::createStyle(fontColour = "#000000",
                                     bgFill = "#808080"),
    national = openxlsx::createStyle(fontColour = "#000000",
                                     bgFill = "#CCC0DA"),
    military = openxlsx::createStyle(fontColour = "#000000",
                                     bgFill = "#C4BD97")
  )
  
  # Data Tabs ####
  data <- list(
    title = openxlsx::createStyle(fontSize = 18,
                                  textDecoration = "bold",
                                  halign = "left",
                                  valign = "center"),
    header = openxlsx::createStyle(fontSize = 12,
                                   textDecoration = "bold",
                                   halign = "left",
                                   valign = "center",
                                   fgFill = "#E4E0A7"),
    label = openxlsx::createStyle(wrapText = TRUE,
                                  halign = "center",
                                  valign = "center",
                                  fgFill = "#9CBEBD"),
    uid = openxlsx::createStyle(textDecoration = "bold",
                                fgFill = "#C2D8D8",
                                fontColour = "#C2D8D8"),
    rowHeader = openxlsx::createStyle(textDecoration = "bold",
                                      fgFill = "#C2D8D8",
                                      fontColour = "#000000"),
    sumRows = openxlsx::createStyle(textDecoration = "bold"),
    invalidDisagg = openxlsx::createStyle(fontColour = "#C00000",
                                          bgFill = "#000000")
  )
  
  # Compile ####
  styleGuide <- list(home = home,
                     siteList = siteList,
                     data = data)
  
  return(styleGuide)
}

getSiteToolSchema <- function(data_pack_schema) {
  site_sheets <- data_pack_schema %>%
    dplyr::filter(sheet_name %in% c("PMTCT_STAT_ART","PMTCT_EID","TB_STAT_ART",
                                    "VMMC","TX","CXCA","HTS","TB_TX_PREV","OVC",
                                    "KP","PP","PrEP","GEND")) %>%
    dplyr::pull(sheet_name) %>%
    unique()
  
  site_row_headers <- c("PSNU","Age","Sex","KeyPop")
    
  site_schema <- data_pack_schema %>%
  # Select only FY20 MER target columns
    dplyr::filter((col_type == "Target" & dataset == "MER")
                  | (col_type == "Row Header" 
                     & sheet_name %in% site_sheets
                     & indicator_code %in% site_row_headers)) %>%
    dplyr::arrange(sheet_num) %>%
  # Data Pack formulas not relevant for Site Tool
    dplyr::select(-formula) %>%
  # Use indicator_code to construct headers
    dplyr::mutate(indicator_code = stringr::str_replace(indicator_code,
                                                        "^PSNU$",
                                                        "Site"),
                  tech_area =
                    dplyr::case_when(
                      col_type == "Target" ~ stringr::str_extract(indicator_code,"^(.)+\\.(N|D)(?=\\.)")),
                  tech_area = 
                    dplyr::case_when(
                      !is.na(tech_area) ~ paste0(stringr::str_replace(tech_area,"\\."," ("),")"))) %>%
    dplyr::group_by(sheet_name,tech_area) %>%
    dplyr::mutate(header = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(tech_area = dplyr::case_when(header == 1 ~ tech_area),
                  split = dplyr::case_when(indicator_code == "Site" ~ 4, TRUE ~ 1))
  
  # Add Mechanism and type columns to every sheet
  site_schema <- site_schema[rep(seq_len(dim(site_schema)[1]),site_schema$split),] %>%
    dplyr::select(-split,-header) %>%
  # Recalibrate column numbers
    dplyr::group_by(sheet_num) %>%
    dplyr::mutate(column = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
  # Recalibrate sheet number
    dplyr::mutate(
      sheet_num = sheet_num - 5,
  # Rename mechanism and type columns
      indicator_code = dplyr::case_when(
        indicator_code == "Site" & column == 1 ~ "Status",
        indicator_code == "Site" & column == 3 ~ "Mechanism",
        indicator_code == "Site" & column == 4 ~ "Type",
        TRUE ~ indicator_code),
      label = dplyr::if_else(
        stringr::str_detect(label, stringr::regex("^note", ignore_case = TRUE)),
        NA_character_,
        label
      )
    ) %>%
    dplyr::select(sheet_num,sheet_name,col = column,col_type,tech_area,label,indicator_code)
  
  return(site_schema)
}

getPeriodInfo <- function(FY = NA) {
  periodISO <- paste0(FY, "Oct")
  
  url <- paste0(getOption("baseurl"),
                "api/",
                datapackr::api_version(),
                "/sqlViews/TTM90ytCCdY/data.json?filter=iso:eq:",
                periodISO) %>%
    utils::URLencode()
  
    r <- httr::GET(url , httr::timeout(60))
    if (r$status == 200L) {
      r <- httr::content(r, "text")
      r <- jsonlite::fromJSON(r)
      if (length(r$rows) > 0) {
        p <- as.data.frame(r$rows,stringsAsFactors = FALSE)
        names(p) <- r$headers$name
        p$enddate <- as.Date(p$enddate,"%Y-%m-%d")
        p$startdate <- as.Date(p$startdate,"%Y-%m-%d")
      } else {
        stop(paste0("Period with ISO identifier", ISO, "not found"))
      } 
    } else {stop("Could not retrieve period information")}
  
  if (!is.na(FY)) {
    assertthat::assert_that(length(FY) == 1)
    p <- p[p$iso == periodISO,] }
  
  return(p)
  
}


# Procedural logic to generate the actual schemas
    secrets <- "/Users/scott/.secrets/datim.json"
    datapackr::loginToDATIM(secrets)

  ## Config File ####
    config_path = "./data-raw/DataPackConfiguration.csv"
    configFile <- readr::read_csv(config_path)
    save(configFile, file = "./data/configFile.rda")
      
  ## Data Pack Map (i.e., Updated Config File) ####
    dataPackMap <- produceConfig()
    save(dataPackMap, file = "./data/dataPackMap.rda")

  ## Data Pack Schema ####
    # TODO: Completely deprecate this schema
    # template_path <- "./data-raw/COP19_Data_Pack_Template_vFINAL.xlsx"
    # data_pack_schema <- unPackStructure(template_path)
    # save(data_pack_schema, file = "./data/data_pack_schema.rda")
    
  ## Updated Data Pack Schema ####
    datapack_template_filepath <- "./data-raw/COP19_Data_Pack_Template_vFinal.xlsx"
    data_pack_schema <- unPackSchema_datapack(
      filepath = datapack_template_filepath,
      skip = skip_tabs(tool = "Data Pack Template"))
    save(data_pack_schema, file = "./data/data_pack_schema.rda")
      
  ## Site Tool Schema ####
    site_tool_schema <- getSiteToolSchema(data_pack_schema)
    save(site_tool_schema, file = "./data/site_tool_schema.rda")

  ## Valid Data Pack Disaggs ####
    valid_dp_disaggs <- validDPDisaggs()
    save(valid_dp_disaggs, file = "./data/valid_dp_disaggs.rda")

  ## Prioritization Dictionary ####
    prioritizations <- prioritizationDict()
    save(prioritizations, file = "./data/prioritizations.rda")

  ## Data Pack to DATIM Indicator Map ####
    indicatorMap <- readr::read_csv("./data-raw/DataPack to DATIM indicator map.csv")
    save(indicatorMap, file = "./data/indicatorMap.rda")
      
  ## Load Openxlsx Style Guide ####
    styleGuide <- loadStyleGuide()
    save(styleGuide, file = "./data/styleGuide.rda")

  ## Load PSNUxIM to DATIM map ####
    PSNUxIM_to_DATIM <- readr::read_csv("./data-raw/PSNUxIM_to_DATIM.csv")
    save(PSNUxIM_to_DATIM, file = "./data/PSNUxIM_to_DATIM.rda")
  ## Load SiteToDATIM map ####
    SiteToDATIM <- readxl::read_excel(
      path = "./data-raw/DataPack_to_DATIM_indicator_map.xlsx",
      sheet = "Site Tool",
      col_types = "text")
    save(SiteToDATIM, file = "./data/SiteToDATIM.rda")
      
  ## Load Period Info ####
    periodInfo <- getPeriodInfo(datapackr::cop_year())
    save(periodInfo, file = "./data/periodInfo.rda")
    
  ## Load Code List into package data file ####
    fullCodeList <- pullFullCodeList(FY = cop_year()+1)
    save(fullCodeList, file = "./data/fullCodeList.rda")
    
  ## Map of all valid COCs and COs for COP FY ####
    valid_COCs_COs <- map_COCs_to_COs() %>%
      dplyr::filter(id %in% unique(fullCodeList$categoryoptioncombouid))
    save(valid_COCs_COs, file = "./data/valid_COCs_COs.rda")
    
  ## All valid COs for COP FY grouped by category ####
    pad <- function(digit) {padded <- paste0("0", digit)}
    five_year_age_names <- c("<01","01-04","05-09","10-14","15-19","20-24",
                             "25-29","30-34","35-39","40-44","45-49","50+")
    valid_COs <- valid_COCs_COs %>%
      dplyr::select(categoryOptions) %>%
      tidyr::unnest() %>%
      dplyr::distinct() %>%
      dplyr::arrange(name) %>%
      dplyr::left_join(
        map_Cs_to_COs() %>% dplyr::select(-categoryoption),
        by = c("id" = "categoryoptionuid")) %>%
      dplyr::mutate(
        datapack_disagg = dplyr::case_when(
          categoryoptiongroup == "Age"
            ~ stringr::str_replace_all(name, "(?<!\\d)\\d(?!\\d)", pad),
          TRUE ~ name),
        ## five_year_ages
        datapack_schema_group = ifelse(
          test = categoryoptiongroup == "Age"
            & datapack_disagg %in% five_year_age_names,
          yes = "5yr",
          no =  ""),
        ##1plus
        datapack_schema_group = ifelse(
          test = stringr::str_detect(datapack_schema_group,"5yr")
            & datapack_disagg != "<01",
          yes = paste(datapack_schema_group,"01+",sep = ","),
          no =  datapack_schema_group),
        ##10plus
        datapack_schema_group = ifelse(
          test = stringr::str_detect(datapack_schema_group,"5yr")
            & (stringr::str_extract(datapack_disagg,"\\d\\d") %>% as.numeric()) >= 10,
          yes = paste(datapack_schema_group,"10+",sep = ","),
          no =  datapack_schema_group),
        ##15plus
        datapack_schema_group = ifelse(
          test = stringr::str_detect(datapack_schema_group,"5yr")
            & (stringr::str_extract(datapack_disagg,"\\d\\d") %>% as.numeric()) >= 15,
          yes = paste(datapack_schema_group,"15+",sep = ","),
          no =  datapack_schema_group),
        ## 25-49
        datapack_schema_group = ifelse(
          test = stringr::str_detect(datapack_schema_group,"5yr")
            & (stringr::str_extract(datapack_disagg,"\\d\\d") %>% as.numeric) >= 25,
          yes = paste(datapack_schema_group,"25-49",sep = ","),
          no =  datapack_schema_group),
        ##ovcAges
        datapack_schema_group = ifelse(
          test = categoryoptiongroup == "Age"
            & datapack_disagg %in% c(five_year_age_names[1:4],"15-17","18+"),
          yes = paste(datapack_schema_group,"ovc",sep = ","),
          no =  datapack_schema_group),
        ##coarseAges
        datapack_schema_group = ifelse(
          test = categoryoptiongroup == "Age" & datapack_disagg %in% c("<15","15+"),
          yes = paste(datapack_schema_group,"coarse",sep = ","),
          no = datapack_schema_group),
        ##coarseKPs
        datapack_schema_group = ifelse(
          test = categoryoptiongroup == "Key Population"
            & datapack_disagg %in% c("FSW","MSM","PWID","TG",
                                     "People in prisons and other enclosed settings"),
          yes = paste(datapack_schema_group,"coarseKPs",sep = ","),
          no = datapack_schema_group),
        ##fineKPs
        datapack_schema_group = ifelse(
          test = categoryoptiongroup == "Key Population"
          & datapack_disagg %in% c("Female PWID","Male PWID","MSM not SW",
                                   "MSM SW","TG not SW","TG SW",
                                   "People in prisons and other enclosed settings"),
          yes = paste(datapack_schema_group,"fineKPs",sep = ","),
          no = datapack_schema_group),
        ##pwidKPs
        datapack_schema_group = ifelse(
          test = categoryoptiongroup == "Key Population"
          & datapack_disagg %in% c("Female PWID","Male PWID"),
          yes = paste(datapack_schema_group,"pwidKPs",sep = ","),
          no = datapack_schema_group),
        ##M/F
        datapack_schema_group = ifelse(
          test = categoryoptiongroup == "Sex"
          & datapack_disagg %in% c("Female","Male"),
          yes = paste(datapack_schema_group,"M/F",sep = ","),
          no = datapack_schema_group),
        datapack_schema_group = stringr::str_replace(datapack_schema_group, "^,","")
      )
    save(valid_COs, file = "./data/valid_COs.rda")
    
  ## All valid age COs
    
    

    
# Load PSNUs into package from DATIM
    valid_PSNUs <- getPSNUs() %>%
      dplyr::select(-organisationUnitGroups, -ancestors)
    save(valid_PSNUs, file = "./data/valid_PSNUs.rda")
    