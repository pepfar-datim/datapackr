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

  ## Updated COP19 Data Pack Schema ####
    datapack_template_filepath <- "./data-raw/COP19_Data_Pack_Template_vFinal.xlsx"
    data_pack_schema <- unPackSchema_datapack(
      filepath = datapack_template_filepath,
      skip = skip_tabs(tool = "Data Pack Template"),
      cop_year = 2019)
    save(data_pack_schema, file = "./data/data_pack_schema.rda")

  ## Updated COP20 Data Pack Schema ####
    datapack_template_filepath <- system.file("extdata",
                                              "COP20_Data_Pack_Template_vFINAL.xlsx",
                                              package = "datapackr",
                                              mustWork = TRUE)
    cop20_data_pack_schema <-
      unPackSchema_datapack(
        filepath = datapack_template_filepath,
        skip = skip_tabs(tool = "Data Pack Template", cop_year = 2020),
        cop_year = 2020)
    save(cop20_data_pack_schema,
         file = "./data/cop20_data_pack_schema.rda",
         compress = "xz")

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
    save(styleGuide, file = "./data/styleGuide.rda", compress = "xz")

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
    periodInfo <- getPeriodInfo(datapackr::getCurrentCOPYear())
    save(periodInfo, file = "./data/periodInfo.rda")
