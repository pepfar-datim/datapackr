library(tidyverse)


unPackStructure <- function(filepath) {

    sheets <- tidyxl::xlsx_sheet_names(filepath)
    sheets_to_loop <- sheets[which(!stringr::str_detect(sheets, "Home|Quotes|Summary|Spectrum|Visualizations|Validations"))]

    schema <- tidyxl::xlsx_cells(path = filepath, include_blank_cells = FALSE) %>%
        dplyr::select(sheet_name = sheet, col, row, character, formula, numeric)

    data.table::setDT(schema)[,sheet_num:=.GRP, by = c("sheet_name")]

    schema <- schema %>%
        dplyr::filter(sheet_name %in% sheets_to_loop,
                      row == 5) %>%
        tidyr::gather(key,value,-sheet_num,-sheet_name,-col,-row) %>%
        tidyr::unite(new.col, c(key,row)) %>%
        tidyr::spread(new.col,value) %>%
        dplyr::select(sheet_num,sheet_name,col,indicatorCode = character_5) %>%
        dplyr::mutate(
            colType = dplyr::case_when(
                stringr::str_detect(indicatorCode, "20T")
                & !(sheet_name == "Prioritization" & indicatorCode != "IMPATT.PRIORITY_SNU.20T")
                & !(sheet_name == "HTS" & (!stringr::str_detect(indicatorCode,"^HTS_") | stringr::str_detect(indicatorCode,"HTS_TST_PMTCT")))
                & !(sheet_name == "VMMC" & stringr::str_detect(indicatorCode, "POP_EST|coverage"))
                ~ "FY20 Target"),
            dataset = dplyr::case_when(
                colType == "FY20 Target" & stringr::str_detect(indicatorCode,"SUBNAT|VL_SUPPRESSED") ~ "SUBNAT",
                colType == "FY20 Target" & stringr::str_detect(indicatorCode,"PLHIV|POP_EST|HIV_PREV|PRIORITY|KP_ESTIMATES") ~ "IMPATT",
                colType == "FY20 Target" ~ "MER")) %>%
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
        dplyr::filter(colType == "FY20 Target") %>%
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
  datapackr::loginToDATIM(getOption("secrets"))
  
  # Load Country List
    configFile <-
      paste0(getOption("baseurl"),"api/",api_version(),
             "/organisationUnits.json?paging=false",
     ## Filter to just countries
             "&filter=organisationUnitGroups.id:eq:cNzfcPWEGSH",
             "&fields=id,name,level,ancestors[id,name]") %>%
      utils::URLencode() %>%
      httr::GET() %>%
      httr::content(., "text") %>%
      jsonlite::fromJSON(., flatten = TRUE) %>%
      do.call(rbind.data.frame, .) %>%
    ## Remove countries no longer supported
      dplyr::filter(!name %in% 
        c("Antigua & Barbuda","Bahamas","Belize","China","Dominica","Grenada",
          "Saint Kitts & Nevis","Saint Lucia","Saint Vincent & the Grenadines",
          "Turkmenistan","Uzbekistan")) %>%
      
    ## Add new countries
      dplyr::select(country_name = name, country_uid = id, dplyr::everything()) %>%
      dplyr::bind_rows(
        tibble::tribble(
          ~country_name, ~country_uid, ~level, ~ancestors,
          "Nepal", "TBD00000001", 4, list(name = NA_character_, id = NA_character_),
          "Brazil", "TBD00000002", 4, list(name = NA_character_, id = NA_character_),
          "Burkina Faso", "TBD00000003", 4, list(name = NA_character_, id = NA_character_),
          "Liberia", "TBD00000004", 4, list(name = NA_character_, id = NA_character_),
          "Mali", "TBD00000005", 4, list(name = NA_character_, id = NA_character_),
          "Senegal", "TBD00000006", 4, list(name = NA_character_, id = NA_character_),
          "Sierra Leone", "TBD00000007", 4, list(name = NA_character_, id = NA_character_),
          "Togo", "TBD00000008", 4, list(name = NA_character_, id = NA_character_)
        )
      ) %>%
  
  # Add metadata
      dplyr::mutate(is_region = level == 4 |
                      country_name %in% c("Burma","Cambodia","India","Indonesia",
                                         "Papua New Guinea","Ghana"),
                    level3name = purrr::map_chr(ancestors,
                                     function(x) magrittr::use_series(x, name) %>%
                                       magrittr::extract(3)),
                    level3name = dplyr::case_when(level == 3 ~ country_name,
                                                  TRUE ~ level3name),
                    uidlevel3 = purrr::map_chr(ancestors,
                                               function(x) magrittr::use_series(x, id) %>%
                                                 magrittr::extract(3)),
                    uidlevel3 = dplyr::case_when(level == 3 ~ country_uid,
                                                  TRUE ~ uidlevel3),
                    level4name =
                      dplyr::case_when(is_region &
                                       !stringr::str_detect(country_uid, "TBD")
                                          ~ country_name),
                    uidlevel4 =
                      dplyr::case_when(is_region &
                                       !stringr::str_detect(country_uid, "TBD")
                                          ~ country_uid),
                    data_pack_name = dplyr::case_when(
                      country_name %in% c("Burma","Cambodia","India","Indonesia",
                                          "Kazakhstan","Kyrgyzstan","Laos",
                                          "Nepal","Papua New Guinea","Tajikistan",
                                          "Thailand") ~ "Asia Region",
                      country_name %in% c("Barbados","Guyana","Jamaica","Suriname",
                                          "Trinidad & Tobago") ~ "Caribbean Region",
                      country_name %in% c("Brazil","Costa Rica","El Salvador",
                                          "Guatemala","Honduras","Nicaragua",
                                          "Panama") ~ "Central America Region",
                      country_name %in% c("Burkina Faso","Ghana","Liberia","Mali",
                                          "Senegal","Sierra Leone","Togo") 
                                            ~ "West Africa Region",
                      TRUE ~ country_name),
                    model_uid = dplyr::case_when(
                      data_pack_name == "Asia Region" ~ "Asia_Regional_Data_Pack",
                      data_pack_name == "Caribbean Region" ~ "Caribbean_Data_Pack",
                      data_pack_name == "Central America Region" ~ "Central_America_Data_Pack",
                      data_pack_name == "West Africa Region" ~ "Western_Africa_Data_Pack",
                      TRUE ~ country_uid
                      ),
                    country_in_datim = !stringr::str_detect(country_uid,"TBD"))
    
  # Add levels & prioritization details
    impattLevels <- datapackr::getIMPATTLevels()
    
    configFile %<>%
      dplyr::left_join(impattLevels, by = c("country_name")) %>%
      dplyr::mutate_if(is.integer,as.double) %>%
      dplyr::mutate(country = level,
                    prioritization = dplyr::case_when(
                      country_in_datim != TRUE ~ level,
                      TRUE ~ prioritization),
                    operating_unit =
                      dplyr::if_else(is.na(operating_unit),
                                     stringr::str_replace(
                                       data_pack_name,
                                       "Central America|Caribbean",
                                       "Western Hemisphere"),
                                     operating_unit))
      

    
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
        mil_psnu = dplyr::case_when(
          is.na(mil_psnu) ~ paste0("_Military ",country_name),
          TRUE ~ mil_psnu),
        mil_psnu_uid = dplyr::case_when(
          is.na(mil_psnu_uid) ~ "TBD",
          TRUE ~ mil_psnu_uid)) %>%
      tidyr::replace_na(list(mil_in_datim = FALSE, mil_level = 5))
    
    return(configFile)
    
}


# Procedural logic to generate the actual schemas
    secrets <- "/Users/scott/.secrets/datim.json"
    datapackr::loginToDATIM(secrets)

    ## Config File
        config_path = "./data-raw/DataPackConfiguration.csv"
        configFile <- readr::read_csv(config_path)
        save(configFile, file = "./data/configFile.rda")
        
    ## Data Pack Map (i.e., Updated Config File)
        dataPackMap <- produceConfig()
        save(dataPackMap, file = "./data/dataPackMap.rda")

    ## Template Schema
        template_path <- "./data-raw/COP19 Data Pack Template v1.3.xlsx"
        template_schema <- unPackStructure(template_path)
        save(template_schema, file = "./data/template_schema.rda")

    ## Valid Data Pack Disaggs
        valid_dp_disaggs <- validDPDisaggs()
        save(valid_dp_disaggs, file = "./data/valid_dp_disaggs.rda")

    ## Prioritization Dictionary
        prioritizations <- prioritizationDict()
        save(prioritizations, file = "./data/prioritizations.rda")

    ## Data Pack to DATIM Indicator Map
        indicatorMap <- readr::read_csv("./data-raw/DataPack to DATIM indicator map.csv")
        save(indicatorMap, file = "./data/indicatorMap.rda")


