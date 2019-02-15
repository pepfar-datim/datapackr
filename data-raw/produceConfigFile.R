library(tidyverse)
library(datimvalidation)


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

# Procedural logic to generate the actual schemas
    secrets <- "/Users/scott/.secrets/datim.json"
    datimvalidation::loadSecrets(secrets)

    ## Config File
        config_path = "./data-raw/DataPackConfiguration.csv"
        configFile <- readr::read_csv(config_path)
        save(configFile, file = "./data/configFile.rda")

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


