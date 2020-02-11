### CHANGE ME ######

#batch1 <- c("Angola","Botswana","Burundi","Cameroon")
#batch2 <- c("Dominican Republic","Eswatini")
#batch3 <- c("Haiti","Lesotho","Malawi","Namibia","Rwanda","South Sudan")
#batch4 <- c("Ukraine","Vietnam","Zambia","Zimbabwe","Cote d'Ivoire")
#longBatch <- c("Ethiopia","Democratic Republic of the Congo","Kenya","South Africa","Nigeria")
#medBatch <- c("Mozambique","Tanzania","Uganda")
#regionalBatch <- c("Asia Region","Caribbean Region","Central America Region","West-Central Africa Region")
# Asia Region took FOREVER too

#problemBatch <- c("Nigeria","Ethiopia","Democratic Republic of the Congo")


getSNUxIMdata <- function(SNUxIM_path, fv_map_path, mechList) {
  # Read in mapping file
  mapping <- readxl::read_excel(fv_map_path,
                                sheet = 2,
                                na = "NA")
  
  # Read in the frozen "SNU x IM" dataset
  PSNUIM <- readr::read_tsv(SNUxIM_path, col_names = TRUE, col_types = NULL) %>%
    dplyr::select(OperatingUnit, OperatingUnitUID, CountryName, PSNUuid,SNU1Uid,
                  MechanismID,
                  indicator, numeratorDenom, disaggregate,categoryOptionComboName,
                  AgeAsEntered,Sex,resultStatus, otherDisaggregate, modality,
                  FY2019_TARGETS) %>%
    # Drop any Values NA or 0
    tidyr::drop_na(FY2019_TARGETS) %>%
    dplyr::filter(FY2019_TARGETS != 0,
                  (AgeAsEntered != "Unknown Age" | is.na(AgeAsEntered))) %>%
    # Map to Data Pack indicatorCodes
    dplyr::left_join(mapping) %>%
    dplyr::filter(!is.na(dp_de),
                  # Remove MOH Alignment data
                  !MechanismID %in% c("00100","00200")) %>%
    # Combine Dedupe mechanisms
    dplyr::mutate(MechanismID = dplyr::case_when(MechanismID %in% c("00000","00001") ~ "Dedupe",
                                                 TRUE ~ MechanismID),
                  # Summarize under broader age bands
                  DPAge = dplyr::case_when(
                    !is.na(DPAge) ~ DPAge,
                    indicator == "OVC_SERV"
                    & AgeAsEntered %in% c("<01","01-04","05-09","01-09","10-14","15-17") ~ "<18",
                    indicator == "OVC_SERV" & AgeAsEntered %in% c("18-24","25+") ~ "18+",
                    AgeAsEntered %in% c("<02 Months","02 Months - 09 Years","<01","01-04","05-09","01-09","<10","10-14") ~ "<15",
                    AgeAsEntered %in% c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","40-49","50+") ~ "15+",
                    TRUE ~ AgeAsEntered),
                  DPSex = dplyr::case_when(indicator == "PMTCT_EID" ~ "Unknown Sex",
                                           !is.na(DPSex) ~ DPSex,
                                           TRUE ~ Sex),
                  KeyPop = dplyr::case_when(otherDisaggregate %in% c("FSW",
                                                                     "Female PWID","PWID","Male PWID",
                                                                     "MSM","MSM not SW","MSM SW",
                                                                     "Other Key Populations",
                                                                     "People in prisons and other enclosed settings",
                                                                     "TG","TG not SW","TG SW")
                                            ~ otherDisaggregate),
                  DPKP = dplyr::case_when(!is.na(DPKP) ~ DPKP,
                                          TRUE ~ KeyPop)
    ) %>%
    
    # Split Unknown Sex
    dplyr::bind_rows((.) %>%
                       filter(indicator != "PMTCT_EID" & Sex == "Unknown Sex") %>%
                       mutate(DPSex = "Male")
    ) %>%
    dplyr::mutate(DPSex = dplyr::case_when(
      indicator != "PMTCT_EID" & Sex == "Unknown Sex" ~ "Female",
      TRUE ~ DPSex)
    ) %>%
    # Map Mechanisms
    mapLegacyMechs(., mechList) %>%
    
    # Summarize
    dplyr::select(OperatingUnit, OperatingUnitUID, CountryName, SNU1Uid,PSNUuid,
                  sheet_name = dp_sheet, indicatorCode = dp_de,
                  Age = DPAge, Sex = DPSex, KeyPop = DPKP,MechanismID,
                  FY2019_TARGETS) %>%
    dplyr::group_by(OperatingUnit, OperatingUnitUID, CountryName, SNU1Uid, PSNUuid,
                    MechanismID,sheet_name, indicatorCode,
                    Age, Sex, KeyPop) %>%
    dplyr::summarise(FY2019_TARGETS = sum(FY2019_TARGETS)) %>%
    ungroup() %>%
    
    # Create Percent column
    dplyr::group_by(OperatingUnit, OperatingUnitUID, CountryName, SNU1Uid, PSNUuid,
                    sheet_name, indicatorCode,
                    Age, Sex, KeyPop) %>%
    dplyr::mutate(Percent = FY2019_TARGETS / sum(FY2019_TARGETS)) %>%
    dplyr::ungroup() %>%
    # Drop where Percent isn't finite
    dplyr::filter(is.finite(Percent)) %>%
    # Convert percents into formulas with built in percents
    dplyr::mutate(PercentFormulas = paste0("(",Percent,")")) %>%
    dplyr::select(-Percent) %>%
    # Drop value and keep percent
    dplyr::select(-FY2019_TARGETS) %>%
    # Sort
    dplyr::arrange(OperatingUnit, OperatingUnitUID, CountryName, SNU1Uid, PSNUuid,
                   sheet_name, indicatorCode, Age, Sex, KeyPop, MechanismID)
  
  return(PSNUIM)
}

getFACTSMechs <- function(FACTSMechs_path) {
  mechList <- readr::read_csv(file = FACTSMechs_path,
                              na = "NULL",
                              col_types = readr::cols(.default = "c")) %>%
    select(Country,HQID,LegacyID,IM,Agency,StartDate,EndDate) %>%
    mutate(StartDate = lubridate::mdy(StartDate),
           EndDate = lubridate::mdy(EndDate),
           Mechanism = paste0(HQID, " - ", IM),
           LegacyID = dplyr::case_when(LegacyID == "0" ~ NA_character_,
                                       TRUE ~ LegacyID)) %>%
    filter(EndDate > "2019-09-30" | is.na(EndDate),
           !stringr::str_detect(Agency,"State"),
           !Country %in% c("Asia Region"),
           !(Country %in% c("West-Central Africa Region","Western Hemisphere Region")
             & !stringr::str_detect(Mechanism,"Placeholder(.)+DOD"))) %>%
    select(Country,HQID, LegacyID, Mechanism)
}

get_FY20TargetCodes <- function(schema) {
  SNUxIM_targetCodes <- schema %>%
    dplyr::filter(colType == "FY20 Target",
                  !(sheet_name == "Prioritization" & uid != "IMPATT.PRIORITY_SNU.20T"),
                  !(sheet_name == "VMMC" & stringr::str_detect(uid, "POP_EST|SUBNAT(.)+coverage")),
                  !(sheet_name == "HTS" & (!stringr::str_detect(uid,"^HTS_") | stringr::str_detect(uid,"HTS_TST_PMTCT")))) %>%
    dplyr::select(sheet_name, uid) %>%
    dplyr::mutate(dataset = dplyr::case_when(stringr::str_detect(uid,"POP_EST|PLHIV|HIV_PREV|PRIORITY_SNU|KP_ESTIMATES")  ~ "IMPATT",
                                             stringr::str_detect(uid,"SUBNAT|VL_SUPPRESSED") ~ "SUBNAT",
                                             TRUE ~ "MER")) %>%
    unique
  
  return(SNUxIM_targetCodes)
}

getDPinfo <- function(d) {
  
  # Log into DATIM
  datimvalidation::loadSecrets(d$keychain$secrets)
  
  # Is this a Region?
  d$data$isRegion <- d$supportFiles$configFile %>%
    filter(DataPack_name == d$data$ou_name) %>%
    select(isRegion) %>%
    unique() %>%
    pull(isRegion)
  
  # Grab Data Pack UID
  d$data$ou_uid <- d$supportFiles$configFile %>%
    filter(DataPack_name == d$data$ou_name) %>%
    select(model_uid) %>%
    unique() %>%
    pull(model_uid)
  
  # Grab list of Country Names
  d$data$countryNames <- d$supportFiles$configFile %>%
    filter(DataPack_name == d$data$ou_name) %>%
    select(countryName) %>%
    pull(countryName)
  
  # Grab names of Countries currently in DATIM
  d$data$DATIMcountryNames <- d$supportFiles$configFile %>%
    filter(DataPack_name == d$data$ou_name,
           Currently_in_DATIM == "Y") %>%
    select(countryName) %>%
    unique() %>%
    pull(countryName)
  
  # Grab regional countries not prioritizing at Natl or Regional levels
  if (d$data$isRegion == 1) {
    d$data$needsCountryTag <- d$supportFiles$configFile %>%
      filter(DataPack_name == d$data$ou_name,
             Prioritizing_at_Natl_or_SNU == "SNU") %>%
      select(countryName) %>%
      unique() %>%
      pull(countryName)
    
    # Grab list of new new countries
    d$data$newCountries <- d$supportFiles$configFile %>%
      filter(DataPack_name == d$data$ou_name,
             Currently_in_DATIM == "N",
             isMil == "0") %>%
      select(countryName, countryUID)
  }
  
  # Pull list of Mil PSNUs %>%
  d$data$milPSNUs <- d$supportFiles$configFile %>%
    filter(DataPack_name == d$data$ou_name,
           !is.na(milPSNU)) %>%
    select(DataPack_name, milPSNU, milPSNUuid, milPSNU_in_DATIM) %>%
    unique()
  
  return(d)
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

get_SNUxIM_indicators <- function(d) {
  
  # Grab FY20 Targets codes
  SNUxIM_targetCodes <- get_FY20TargetCodes(d$supportFiles$schema) %>%
    dplyr::filter(dataset %in% c("MER"))
  
  # Grab disaggs
  disaggs <- validDPDisaggs()
  
  SNUxIM_des <- NULL
  
  SNUxIM_sheets <- SNUxIM_targetCodes %>%
    select(sheet_name) %>%
    unique() %>%
    pull(sheet_name)
  
  for (i in 1:length(SNUxIM_sheets)) {
    sheet = SNUxIM_sheets[i]
    
    SNUxIM_des <- disaggs %>%
      magrittr::extract2(sheet) %>%
      purrr::cross_df() %>%
      
      # Cross with FY20 Targets codes
      tidyr::crossing((SNUxIM_targetCodes %>%
                         dplyr::filter(sheet_name == sheet) %>%
                         dplyr::select(uid))
                      ,.) %>%
      dplyr::mutate(sheet_name = sheet) %>%
      dplyr::select(sheet_name,everything()) %>%
      dplyr::bind_rows(SNUxIM_des,.)
  }
  
  # Drop invalid Disaggs
  SNUxIM_des <- SNUxIM_des %>%
    dplyr::filter(
      
      ## Clean up KP
      !(sheet_name == "KP"
        & stringr::str_detect(uid,"HTS_TST|TX_NEW|PrEP")
        & stringr::str_detect(validKPs,"Female PWID|Male PWID|MSM not SW|MSM SW|TG not SW|TG SW")),
      !(stringr::str_detect(uid,"KP_PREV") & validKPs %in% c("MSM","PWID","TG")),
      !(stringr::str_detect(uid,"KP_MAT") & !validKPs %in% c("Female PWID","Male PWID")),
      
      ## Clean up HTS
      !(stringr::str_detect(uid,"Malnutrition|Pediatric") & !validAges == "01-04"),
      !(stringr::str_detect(uid,"HTS_RECENT") & validAges %in% c("01-04","05-09","10-14")),
      !(stringr::str_detect(uid,"HTS_SELF") & validAges %in% c("01-04","05-09"))
    ) %>%
    dplyr::mutate(validAges = dplyr::case_when(
      stringr::str_detect(uid,"OVC_SERV")
      & validAges %in% c("<01","01-04","05-09","10-14","15-17")~ "<18",
      stringr::str_detect(uid,"OVC_HIVSTAT") ~ NA_character_,
      stringr::str_detect(uid,"PMTCT_EID(.)+\\.2mo$") ~ "<=02 Months",
      stringr::str_detect(uid,"PMTCT_EID(.)+12mo$") ~ "02 - 12 Months",
      stringr::str_detect(uid,"Malnutrition|Pediatric") ~ "01-04",
      stringr::str_detect(uid,"HTS_SELF(.)+Unassisted") ~ NA_character_,
      validAges %in% c("<01","01-04","05-09","10-14") ~ "<15",
      validAges %in% c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+") ~ "15+",
      TRUE ~ validAges),
      validSexes = dplyr::case_when(stringr::str_detect(uid,"OVC_HIVSTAT") ~ NA_character_,
                                    stringr::str_detect(uid,"PMTCT_EID") ~ "Unknown Sex",
                                    stringr::str_detect(uid,"KP_MAT") ~ stringr::str_replace(validKPs," PWID",""),
                                    stringr::str_detect(uid,"HTS_SELF(.)+Unassisted") ~ NA_character_,
                                    TRUE ~ validSexes),
      validKPs = dplyr::case_when(stringr::str_detect(uid,"KP_MAT") ~ NA_character_,
                                  TRUE ~ validKPs)
    ) %>%
    unique() %>%
    select(sheet_name, indicatorCode = uid, Age = validAges, Sex = validSexes, KeyPop = validKPs)
  
  return(SNUxIM_des)
}

mapLegacyMechs <- function(PSNUIM, mechList) {
  
  OUmechs <- mechList %>%
    dplyr::select(-Mechanism) %>%
    tidyr::drop_na(LegacyID)
  
  PSNUIM_mapped <- PSNUIM %>%
    dplyr::left_join(OUmechs, by = c("MechanismID" = "LegacyID",
                                     "CountryName" = "Country")) %>%
    dplyr::mutate(MechanismID = dplyr::case_when(!is.na(HQID) ~ HQID,
                                                 TRUE ~ MechanismID))
  
  return(PSNUIM_mapped)
}

prepare_SNUxIM_data <- function(wb, d) {
  print("Organizing SNU x IM data...")
  
  # Pull in data filtered to selected OU
  if (d$data$isRegion == 1) {
    SNUxIM_data <- d$supportFiles$PSNUIM %>%
      dplyr::filter(CountryName %in% d$data$DATIMcountryNames)
  } else {
    SNUxIM_data <- d$supportFiles$PSNUIM %>%
      dplyr::filter(OperatingUnitUID == d$data$ou_uid)
  }
  
  # Capture list of mechs in SNUxIM_data
  usedMechs <- SNUxIM_data %>%
    dplyr::select(MechanismID) %>%
    dplyr::filter(MechanismID != "Dedupe") %>%
    unique()
  
  # Spread to splay Mechanisms as Columns
  SNUxIM_data <- SNUxIM_data %>%
    tidyr::spread(key = MechanismID, value = PercentFormulas)
  
  # Make sure there's a Dedupe Column
  if (!"Dedupe" %in% colnames(SNUxIM_data)) {
    SNUxIM_data <- SNUxIM_data %>%
      dplyr::mutate(Dedupe = NA_character_)
  }
  
  # Pull full list of dataelements and disaggs
  SNUxIM_des <- get_SNUxIM_indicators(d)
  
  # Cross dataelement/disagg list with PSNUs
  SNUxIM_combined <- tidyr::crossing((d$data$PSNUs %>%
                                        select(PSNU = DataPackSiteID) %>%
                                        unique),
                                     SNUxIM_des) %>%
    dplyr::mutate(PSNUuid = stringr::str_extract(PSNU,
                                                 "(?<=\\()[a-zA-Z]{1}[a-zA-Z0-9]{10}(?=\\))")) %>%
    dplyr::left_join(SNUxIM_data) %>%
    dplyr::mutate(ID = "",
                  sheet_num = "",
                  DataPackTarget = "",
                  Rollup = "") %>%
    dplyr::select(PSNU, sheet_name, indicatorCode, CoarseAge = Age, Sex, KeyPop, ID, sheet_num,
                  DataPackTarget, Rollup, Dedupe, everything(),
                  -PSNUuid, -OperatingUnit, -OperatingUnitUID, -CountryName, -SNU1Uid)
  
  # Capture dimensions
  rowCount <- NROW(SNUxIM_combined)
  colCount <- NCOL(SNUxIM_combined)
  
  # Setup common formula columns
  print("Adding common formula columns...")
  blueprint <- d$supportFiles$schema %>%
    dplyr::filter(sheet_name == "SNU x IM") %>%
    dplyr::select(uid, formula) %>%
    `row.names<-`(.[, 1]) %>%
    dplyr::select(-1) %>%
    t() %>%
    tibble::as_tibble() %>%
    dplyr::select(-Mechanism1) %>%
    
    dplyr::slice(rep(1:n(), each = rowCount)) %>%
    dplyr::mutate(row = (1:n()) + 5,
                  ID = paste0('$A',row,'&IF($B',row,'="PMTCT_EID","",IF(AND($D',row,'<>"",$E',row,
                              '<>""),"|"&IF(OR(MID($C',row,',9,12)="Malnutrition",MID($C',row,',9,9)="Pediatric"),"<15",$D',row,
                              ')&"|"&E',row,',IF($B',row,'="KP",IF(ISERROR(FIND("KP_MAT",$C',row,')),"|"&$F',row,',"|"&$E',
                              row,'&" PWID"),"")))'),
                  sheet_num = paste0('MATCH($B',row,
                                     ',{"PMTCT_STAT_ART","PMTCT_EID","TB_STAT_ART","VMMC","TX","CXCA","HTS","TB_TX_PREV","OVC","KP","PP","PrEP","GEND"},0)'),
                  DataPackTarget = paste0('ROUND(SUMIF(CHOOSE($H',row,
                                          ',PMTCT_STAT_ART!$F:$F,PMTCT_EID!$A:$A,TB_STAT_ART!$F:$F,VMMC!$F:$F,TX!$F:$F,CXCA!$F:$F,',
                                          'IF(RIGHT($C',row,',10)="Unassisted",HTS!$A:$A,HTS!$F:$F),TB_TX_PREV!$D:$D,',
                                          'IF(LEFT($C',row,',11)="OVC_HIVSTAT",OVC!$A:$A,OVC!$E:$E),KP!$C:$C,PP!$E:$E,PrEP!$E:$E,GEND!$A:$A),$G',
                                          row,',INDEX(CHOOSE($H',
                                          row,',PMTCT_STAT_ART!$A:$Z,PMTCT_EID!$A:$J,TB_STAT_ART!$A:$R,VMMC!$A:$S,TX!$A:$AB,CXCA!$A:$H,HTS!$A:$CC,TB_TX_PREV!$A:$Y,OVC!$A:$L,KP!$A:$T,PP!$A:$H,PrEP!$A:$H,GEND!$A:$F),,MATCH($C',
                                          row,',CHOOSE($H',
                                          row,',PMTCT_STAT_ART!$5:$5,PMTCT_EID!$5:$5,TB_STAT_ART!$5:$5,VMMC!$5:$5,TX!$5:$5,CXCA!$5:$5,HTS!$5:$5,TB_TX_PREV!$5:$5,OVC!$5:$5,KP!$5:$5,PP!$5:$5,PrEP!$5:$5,GEND!$5:$5),0))),0)'),
                  Rollup = paste0('SUM($K',row,':$',openxlsx::int2col(colCount),row,')'))
  
  # Swap Columns where needed
  SNUxIM_combined <- SNUxIM_combined %>%
    swapColumns(., (blueprint %>%
                      select(ID, sheet_num, DataPackTarget, Rollup))) %>%
    as.data.frame(.)
  
  #Make sure formulas referencing correct rows
  print("Adding formulas under every mechanism that has data...")
  SNUxIM_combined <- SNUxIM_combined %>%
    dplyr::mutate(lineNum = (1:n()) + 5) %>%
    dplyr::mutate_at(c(11:colCount),
                     .funs = funs(dplyr::case_when(
                       is.na(.) ~ .,
                       TRUE ~ paste0("=$I",lineNum,"*",.))
                     )
    ) %>%
    dplyr::select(-lineNum)
  
  # Classify formula columns as formulas
  SNUxIM_combined[is.na(SNUxIM_combined)] <- ""
  for (i in 7:colCount) {
    class(SNUxIM_combined[[i]]) <- c(class(SNUxIM_combined[[i]]), "formula")
  }
  
  # Write Data
  print("Writing SNU x IM data into Data Pack")
  openxlsx::writeData(wb, sheet = "SNU x IM",
                      SNUxIM_combined,
                      xy = c(1,5))
  
  # Make sure all active Mechs are represented
  countryList <- d$data$countryNames
  if (d$data$ou_name %in% c("Caribbean Region","Central America Region")) {
    countryList <- c(countryList, "Western Hemisphere Region")
  } else if (d$data$ou_name == "West-Central Africa Region") {
    countryList <- c(countryList, "West-Central Africa Region")
  }
  
  activeMechs <- d$supportFiles$mechList %>%
    dplyr::filter(Country %in% countryList) %>%
    dplyr::select(HQID) %>%
    unique() %>%
    dplyr::arrange(HQID)
  
  missingMechs <- activeMechs %>%
    dplyr::filter(!HQID %in% colnames(SNUxIM_combined)) %>%
    t() %>%
    as.data.frame()
  
  openxlsx::writeData(wb, sheet = "SNU x IM",
                      missingMechs,
                      colNames = FALSE,
                      xy = c(colCount+1,5))
  
  # Make sure Mech Number Header formats are right
  print("Cleaning up SNU x IM tab stlyes")
  mechColHeaders <- openxlsx::createStyle(halign = "center",
                                          valign = "center",
                                          textRotation = 90,
                                          fgFill = "#9CBEBD")
  
  openxlsx::addStyle(wb, sheet = "SNU x IM",
                     style = mechColHeaders,
                     rows = 5,
                     cols = 11:(colCount+length(missingMechs)),
                     gridExpand = TRUE,
                     stack = TRUE)
  
  ## Write roll-up row
  print("Writing the SNU x IM tab roll-up row...")
  rollup_columns <- openxlsx::int2col(11:(colCount+length(missingMechs)))
  
  rollup_formulas <- paste0("SUBTOTAL(109,",
                            rollup_columns,
                            "$6:INDEX(",
                            rollup_columns,
                            ":",
                            rollup_columns,
                            ",COUNTA($A:$A)+5-COUNTA($A$1:$A$5)))") %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate_all(as.character)
  
  ### Classify all roll-up row as formulas
  for (i in 1:length(rollup_formulas)) {
    class(rollup_formulas[[i]]) <- c(class(rollup_formulas[[i]]), "formula")
  }
  
  ### Write into Data Pack
  openxlsx::writeData(wb, sheet = "SNU x IM",
                      x = rollup_formulas,
                      xy = c(11,4),
                      colNames = FALSE)
  
  # Note mechanisms needing attention
  print("Highlighting dying mechanisms in red...")
  attentionMechs <- usedMechs %>%
    dplyr::filter(!MechanismID %in% dplyr::pull(activeMechs,HQID))
  
  attentionCols <- which(colnames(SNUxIM_combined) %in% dplyr::pull(attentionMechs,MechanismID))
  
  attention <- openxlsx::createStyle(fgFill = "#FFC7CE")
  
  openxlsx::addStyle(wb, sheet = "SNU x IM",
                     style = attention,
                     rows = 5,
                     cols = attentionCols,
                     gridExpand = TRUE,
                     stack = TRUE)
  
  return(wb)
}

template_to_schema <- function(template_path) {
  
  sheets <- tidyxl::xlsx_sheet_names(template_path)
  sheets_to_loop <- sheets[which(!stringr::str_detect(sheets, "Home|Quotes|Summary|Spectrum|Visualizations|Validations"))]
  
  schema <- tidyxl::xlsx_cells(path = template_path, include_blank_cells = FALSE) %>%
    dplyr::select(sheet_name = sheet, col, row, character, formula, numeric)
  
  data.table::setDT(schema)[,sheet_num:=.GRP, by = c("sheet_name")]
  
  schema <- schema %>%
    dplyr::filter(sheet_name %in% sheets_to_loop,
                  !row %in% c(4)) %>%
    tidyr::gather(key,value,-sheet_num,-sheet_name,-col,-row) %>%
    tidyr::unite(new.col, c(key,row)) %>%
    tidyr::spread(new.col,value) %>%
    dplyr::mutate(format = dplyr::case_when(stringr::str_detect(character_3,"\\(%\\)$") ~ "Percent",
                                            character_5 %in% c("PSNU","Age","Sex","ID","AgeCoarse",
                                                               "IDAgeCoarse","KeyPop","indicatorCode",
                                                               "sheet_name") ~ "Text",
                                            TRUE ~ "Numeric")) %>%
    dplyr::select(sheet_num,sheet_name,col,indicator = character_1, type = character_2,
                  label = character_3, uid = character_5,
                  formula = formula_6, numeric = numeric_6,
                  format) %>%
    dplyr::mutate(formula = dplyr::case_when(is.na(formula) ~ numeric,
                                             TRUE ~ formula),
                  mode = "Data Pack",
                  colType = dplyr::case_when(stringr::str_detect(uid, "20T") ~ "FY20 Target")
    ) %>%
    dplyr::select(mode, everything(),-numeric) %>%
    dplyr::arrange(sheet_num, col)
  
  return(schema)
}

get_ou_info <- function(ou_uid = NA) {
  
  if ( is.na(ou_uid) ) {
    print(paste("Must supply single OU uid"))
    stop()
  }
  
  if (ou_uid %in% c("Asia_Regional_Data_Pack",
                    "Caribbean_Data_Pack",
                    "Central_America_Data_Pack",
                    "Western_Africa_Data_Pack")) {
    
    ou_name = dplyr::case_when(ou_uid == "Asia_Regional_Data_Pack" ~ "Asia Region",
                               ou_uid == "Caribbean_Data_Pack" ~ "Caribbean Region",
                               ou_uid == "Central_America_Data_Pack" ~ "Central America Region",
                               ou_uid == "Western_Africa_Data_Pack" ~ "West-Central Africa Region")
    
    return(as.character(ou_name))
    stop()
  }
  
  url <- URLencode(paste0(getOption("baseurl"),"api/organisationUnits.json?&filter=id:eq:",ou_uid,"&fields=name&paging=false"))
  sig<-digest::digest(url,algo='md5', serialize = FALSE)
  ou_info<-datimvalidation::getCachedObject(sig)
  if (is.null(ou_info)){
    r<-httr::GET(url,httr::timeout(600))
    if (r$status == 200L ){
      r<- httr::content(r, "text")
      ou_info<-jsonlite::fromJSON(r,flatten=TRUE)[[1]]
      datimvalidation::saveCachedObject(ou_info,sig)
    } else {
      print(paste("Could not retreive OU name",httr::content(r,"text")))
      stop()
    }
  }
  return( as.character(ou_info ) )
}

getPSNULevels <- function(){
  
  psnu_levels <-
    paste0(getOption("baseurl"),
           "api/dataStore/dataSetAssignments/ous") %>%
    httr::GET() %>%
    httr::content(., "text") %>%
    jsonlite::fromJSON(., flatten = TRUE) %>%
    do.call(rbind.data.frame, .) %>%
    dplyr::select(operating_unit = name3, country = name4, country_level = country, psnu_level = prioritization) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(country = dplyr::case_when(country == "" ~ operating_unit,TRUE ~ country))
  
  return(psnu_levels)
}

JamaicaSurinamePatch <- function() {
  
  patchList <- c("Suriname","Jamaica")
  
  JamaicaSuriname = NULL
  
  for (i in 1:length(patchList)) {
    URL <- paste0(getOption("baseurl"),
                  "api/sqlViews/kEtZ2bSQCu2/data.json?fields=level4name,organisationunituid,name&filter=level4name:eq:",
                  patchList[i],
                  dplyr::case_when(patchList[i] == "Jamaica" ~ "&filter=level:eq:6",
                                   patchList[i] == "Suriname" ~ "&filter=level:eq:5"))
    Export <- URL %>%
      URLencode() %>%
      httr::GET() %>%
      httr::content(., "text") %>%
      jsonlite::fromJSON(., flatten = TRUE)
    JamaicaSuriname <- as.data.frame(Export$rows,stringsAsFactors = FALSE) %>%
      setNames(.,Export$headers$name) %>%
      dplyr::select(country = level4name, uid = organisationunituid, name) %>%
      unique() %>%
      dplyr::bind_rows(JamaicaSuriname,.)
  }
  
  return(JamaicaSuriname)
  
}

DRC_PSNU_Patch <- function(d) {
  print("Applying DRC PSNU filtering patch")
  
  d$data$sheet = "Prioritization"
  Prioritizations <- prepare_DP_data(d) %>%
    dplyr::select(PSNU, IMPATT.PRIORITY_SNU.20T)
  
  PSNUs <- d$data$PSNUs %>%
    dplyr::left_join(Prioritizations, by = c("DataPackSiteID" = "PSNU")) %>%
    dplyr::mutate(IMPATT.PRIORITY_SNU.20T = dplyr::case_when(
      stringr::str_detect(DataPackSiteID, "_Military ") ~ IMPATT.PRIORITY_SNU.20T,
      IMPATT.PRIORITY_SNU.20T == "8 - Not PEPFAR Supported" ~ "DROP",
      is.na(IMPATT.PRIORITY_SNU.20T) ~ "DROP",
      TRUE ~ IMPATT.PRIORITY_SNU.20T)) %>%
    dplyr::filter(IMPATT.PRIORITY_SNU.20T != "DROP" | is.na(IMPATT.PRIORITY_SNU.20T)) %>%
    select(-IMPATT.PRIORITY_SNU.20T)
  
  return(PSNUs)
  
}

getPSNUs <- function(d) {
  
  # Pull PSNUs from DATIM SQL view
  DATIMcountryNameString <- d$data$DATIMcountryNames %>%
    paste(collapse = ",") %>%
    stringr::str_replace_all("&","%26") %>%
    stringr::str_replace_all(" ","%20")
  
  PSNUs <- paste0(getOption("baseurl"),
                  "api/sqlViews/PjjAyeXUbBd/data.json?fields=operating_unit,uid,name&filter=name:!like:_Military&paging=false&filter=operating_unit:",
                  dplyr::case_when(d$data$ou_name == "Cote d'Ivoire" ~ "like:Cote",
                                   
                                   ## For Regional OUs, pull PSNUs only where these exist in DATIM
                                   d$data$isRegion == 1 ~ paste0("in:[",DATIMcountryNameString,"]"),
                                   TRUE ~ paste0("eq:",d$data$ou_name))) %>%
    URLencode() %>%
    httr::GET() %>%
    httr::content(., "text") %>%
    jsonlite::fromJSON(., flatten = TRUE) %>% .[["listGrid"]]
  PSNUs <- as.data.frame(PSNUs$rows,stringsAsFactors = FALSE) %>%
    setNames(.,PSNUs$headers$name) %>%
    select(country = operating_unit, uid, name) %>%
    unique()
  
  # Add new new countries as PSNUs
  if (!is.null(d$data$newCountries)) {
    PSNUs <- PSNUs %>%
      dplyr::bind_rows(d$data$newCountries %>%
                         mutate(name = countryName) %>%
                         select(country = countryName, uid = countryUID, name))
  }
  
  # No Mil PSNUs at this point. Add them from Config file
  PSNUs <- PSNUs %>%
    dplyr::bind_rows(d$data$milPSNUs %>%
                       select(country = DataPack_name, uid = milPSNUuid, name = milPSNU))
  
  # Patch for Suriname and Jamaica
  if(d$data$ou_name == "Caribbean Region") {
    PSNUs <- PSNUs %>%
      dplyr::filter(!country %in% c("Jamaica", "Suriname")) %>%
      dplyr::bind_rows(JamaicaSurinamePatch())
  }
  
  # Create Data Pack Site ID & tag with country name breadcrumb where country != PSNU
  PSNUs <- PSNUs %>%
    dplyr::mutate(DataPackSiteID = paste0(
      ## Country Name only where country != PSNU
      dplyr::case_when(d$data$isRegion == 1
                       & country %in% d$data$needsCountryTag
                       ~ paste0(country," > "),
                       TRUE ~ ""),
      ## name & uid
      name, " (", uid,")")
    )
  
  return(PSNUs)
}

getCategoryOptionNames <- function(lookups) {
  suppressWarnings(
    if (is.na(lookups)) {
      categoryOptions <- data.frame(name = NA_character_,id = NA_character_, stringsAsFactors = FALSE)
      
    } else {
      
      categoryOptions <- lookups %>%
        unique() %>%
        paste0(collapse=",") %>%
        paste0("[",.,"]") %>%
        paste0(getOption("baseurl"),
               "api/categoryOptions.json?paging=false&fields=id,name&filter=id:in:",
               .) %>%
        URLencode() %>%
        httr::GET() %>%
        httr::content(., "text") %>%
        jsonlite::fromJSON(., flatten = TRUE) %>%
        magrittr::extract2(1) %>%
        dplyr::mutate(name = dplyr::case_when(name == "<1" ~ "<01",
                                              name == "1-4" ~ "01-04",
                                              name == "5-9" ~ "05-09",
                                              name == "1-9" ~ "01-09",
                                              TRUE ~ name))
    }
  )
  return(categoryOptions)
}

writeDPHome <- function(wb, d) {
  
  # ou_name
  openxlsx::writeData(wb, "Home", d$data$ou_name, xy = c(2,20), colNames = F)
  
  # Country List
  if (d$data$isRegion == 1) {
    d$data$countryNameString <- d$data$countryNames %>%
      paste(collapse = ", ")
    openxlsx::writeData(wb, "Home", d$data$countryNameString, xy = c(2,22), colNames = F)
  }
  
  # ou_uid
  openxlsx::writeData(wb, "Home", d$data$ou_uid, xy = c(2,25), colNames = F)
  
  # Generated:
  openxlsx::writeData(wb, "Home", paste("Generated on:", Sys.time()), xy = c(2, 27), colNames = F)
  
  # Package version
  # openxlsx::writeData(wb,"Home",
  #                     paste("Package version:"), #, as.character(packageVersion("datapackimporter"))),
  #                     xy = c(2, 29))
}

setupRowStructure <- function(d) {
  # What disaggs are valid?
  disaggs <- validDPDisaggs() %>%
    magrittr::extract2(d$data$sheet) %>%
    purrr::cross_df()
  
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
  
  return(rowStructure)
}

swapColumns <- function(to, from) {
  cols = colnames(from)
  for (i in 1:length(cols)) {
    col = cols[i]
    if (col %in% colnames(to)) {
      dots <- setNames(list(lazyeval::interp(~ magrittr::use_series(from,x), x = as.name(col))), col)
      to <- to %>%
        dplyr::mutate_(.dots = dots)
    } else {next}
  }
  return(to)
}

getModelData <- function(d) {
  
  # Pull only results for specified OU & Sheet
  model <- purrr::map(d$supportFiles$model_data[[d$data$ou_uid]][[d$data$sheet]],"results") %>%
    
    # Restructure
    purrr::set_names(names(d$supportFiles$model_data[[d$data$ou_uid]][[d$data$sheet]])) %>%
    tibble::enframe("indicator","results") %>%
    dplyr::mutate(whosThere = lapply(magrittr::use_series(., results), NROW))
  
  if (max(unlist(model$whosThere)) != 0) {
    
    model <- model %>%
      dplyr::filter(whosThere > 0) %>%
      dplyr::select(-whosThere) %>%
      tidyr::unnest()
    
    if (d$data$sheet == "HTS") {
      model <- model %>%
        tidyr::replace_na(list(A = 0.00, B = 0.00)) %>%
        dplyr::mutate(value = dplyr::case_when(indicator == "HTS_INDEX.comPosShare" ~
                                                 A/(A+B),
                                               TRUE ~ value))
    }
    
    # Select only columns needed
    model <- model %>%
      dplyr::select(indicator,org_unit_uid,value,
                    dplyr::matches("age_option_uid"),
                    dplyr::matches("sex_option_uid"),
                    dplyr::matches("kp_option_uid")) %>%
      
      # Transpose to splay indicators as columns
      tidyr::spread(indicator,value,fill=NA,drop=TRUE,convert=TRUE) %>%
      
      # Translate DATIM uids
      dplyr::left_join(d$data$PSNUs, by = c("org_unit_uid" = "uid")) %>%
      dplyr::select(PSNU = DataPackSiteID, everything(), -org_unit_uid, -country, -name)
    
    if ("kp_option_uid" %in% colnames(model)) {
      model <- model %>%
        dplyr::left_join(getCategoryOptionNames(magrittr::extract2(., "kp_option_uid")),
                         by = c("kp_option_uid" = "id")) %>%
        dplyr::select(PSNU, KeyPop = name, everything(), -kp_option_uid)
    }
    
    if ("sex_option_uid" %in% colnames(model)) {
      model <- model %>%
        dplyr::left_join(getCategoryOptionNames(magrittr::extract2(., "sex_option_uid")), by = c("sex_option_uid" = "id")) %>%
        select(PSNU, Sex = name, everything(), -sex_option_uid)
    }
    
    if ("age_option_uid" %in% colnames(model)) {
      model <- model %>%
        dplyr::left_join(getCategoryOptionNames(magrittr::extract2(., "age_option_uid")), by = c("age_option_uid" = "id")) %>%
        select(PSNU, Age = name, everything(), -age_option_uid)
    }
  } else {
    model <- model %>%
      dplyr::select(-whosThere) %>%
      tidyr::unnest()
  }
  
  return(model)
}

prioritizationDict <- function() {
  dict <- tribble(
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

prepare_DP_data <- function(d) {
  # Setup Rows
  model <- setupRowStructure(d) %>%
    arrange(PSNU, Age, Sex, KeyPop)
  
  # Pull Model Data
  if (!stringr::str_detect(d$data$sheet,"Epi|CXCA")) {
    modelData <- getModelData(d)
    if (NROW(modelData) != 0) {
      model <- model %>%
        dplyr::left_join(modelData)
      
      ## Translate Prioritizations
      if (d$data$sheet == "Prioritization") {
        dict <- prioritizationDict()
        model$IMPATT.PRIORITY_SNU.19T <- as.character(
          dict$Prioritization[match(model$IMPATT.PRIORITY_SNU.19T, dict$value)]
        )
        
        model <- model %>%
          dplyr::select(IMPATT.PRIORITY_SNU.20T = IMPATT.PRIORITY_SNU.19T, everything())
      }
      
      ## Fix issue with GEND_GBV name length
      if (d$data$sheet == "GEND") {
        model <- model %>%
          dplyr::mutate(GEND_GBV.N.ViolenceServiceType.19T.postRape = GEND_GBV.N.ViolenceServiceType.19T.Sexual_Violence__Post_Rape_Care) %>%
          select(-GEND_GBV.N.ViolenceServiceType.19T.Sexual_Violence__Post_Rape_Care)
      }
      
      ## Drop Other PITC data to prevent overwriting new catch-all formula in Data Pack
      if (d$data$sheet == "HTS" & "HTS_TST.N.otherShare" %in% colnames(model)) {
        model <- model %>%
          dplyr::select(-HTS_TST.N.otherShare)
      }
    }
  }
  
  # Capture row count
  rowCount <- NROW(model)
  
  # Setup columns based on template schema
  data <- d$supportFiles$schema %>%
    dplyr::filter(sheet_name == d$data$sheet) %>%
    dplyr::select(uid, formula) %>%
    `row.names<-`(.[, 1]) %>%
    dplyr::select(-1) %>%
    t() %>%
    as_tibble() %>%
    
    # Setup formulas
    dplyr::slice(rep(1:n(), each = rowCount)) %>%
    dplyr::mutate_if(is.character,
                     stringr::str_replace_all, pattern = "(?<=[:upper:])6", replacement = as.character(1:rowCount + 5))
  
  # Classify formula columns as formulas
  for (i in 1:length(data)) {
    if (!all(any(is.na(data[[i]])))) {
      class(data[[i]]) <- c(class(data[[i]]), "formula")
    }
  }
  
  
  # Pull row stuctures (PSNUs, Ages, Sexes, KPs)
  data <- data %>%
    swapColumns(., model) %>%
    as.data.frame(.)
  
  #TODO: pay attention here RE: KP
  
  return(data)
  
}

addValidations <- function(wb,d) {
  
  openxlsx::addWorksheet(wb,sheetName = "Validations", visible = FALSE)
  
  # Drop-downs
  ## Rasta table on Summary page
  psnuList <- dplyr::select(d$data$PSNUs,DataPackSiteID)
  
  openxlsx::writeData(wb,
                      "Validations",
                      psnuList,
                      xy = c(1,1),
                      colNames = FALSE,
                      name = "psnuList")
  
  openxlsx::dataValidation(wb,
                           sheet = "Summary",
                           cols = 5,
                           rows = 44,
                           type = "list",
                           value = "=psnuList"
  )
  
  ## VMMC Yes/No
  VMMC.yesNo <- data.frame(options = c("Yes","No"))
  
  openxlsx::writeData(wb,
                      "Validations",
                      VMMC.yesNo,
                      xy = c(2,1),
                      colNames = FALSE,
                      name = "vmmcYesNo")
  
  openxlsx::dataValidation(wb,
                           sheet = "VMMC",
                           cols = 2,
                           rows = 2,
                           type = "list",
                           value = "=vmmcYesNo"
  )
  
  ## Prioritizations
  prioritizationOptions <- prioritizationDict() %>%
    select(Prioritization)
  
  openxlsx::writeData(wb,
                      "Validations",
                      prioritizationOptions,
                      xy = c(3,1),
                      colNames = FALSE,
                      name = "prioritizationOptions")
  
  openxlsx::dataValidation(wb,
                           sheet = "Prioritization",
                           cols = 11,
                           rows = 6:(NROW(d$data$PSNUs)+5),
                           type = "list",
                           value = "=prioritizationOptions"
  )
}

writeDataPack <- function(d) {
  
  # Get Country Info
  d <- getDPinfo(d)
  
  # Grab PSNU list
  d$data$PSNUs <- getPSNUs(d)
  
  ## DRC Patch
  if (d$data$ou_name == "Democratic Republic of the Congo") {
    View(d$data$PSNUs) <- DRC_PSNU_Patch(d)
  }
  
  # Open template
  wb <- openxlsx::loadWorkbook(d$keychain$template_path)
  
  # Write Home sheet info
  writeDPHome(wb, d)
  
  # Write Sheets
  d$data$sheets <- openxlsx::getSheetNames(d$keychain$template_path)
  d$data$sheets_to_loop <- d$data$sheets[which(!stringr::str_detect(d$data$sheets, "Home|Quotes|Summary|Spectrum|SNU x IM|Visualizations"))]
  
  ## Loop Sheets
  percentStyle = openxlsx::createStyle(numFmt = "0%")
  print("Writing Sheets...")
  for (i in 1:(length(d$data$sheets_to_loop))) {
    d$data$sheet = d$data$sheets_to_loop[i]
    print(d$data$sheet)
    d$data$table <- prepare_DP_data(d)
    
    ### Write Data Table into Data Pack Template
    openxlsx::writeData(wb,
                        sheet = d$data$sheet,
                        x = d$data$table,
                        xy = c(1,5),
                        colNames = T, rowNames = F, withFilter = TRUE)
    
    ### Write percentage style in
    percentCols <- which(d$supportFiles$schema$format[d$supportFiles$schema$sheet_name == d$data$sheet] == "Percent")
    if (!is.na(percentCols[1])) {
      openxlsx::addStyle(wb, sheet = d$data$sheet,
                         percentStyle,
                         cols = percentCols,
                         rows = (1:NROW(d$data$table))+5, gridExpand = TRUE, stack = TRUE)
    }
  }
  
  # Compile & write SNUxIM sheet
  print("Writing SNU x IM tab. This can sometimes take a few minutes...")
  wb <- prepare_SNUxIM_data(wb, d)
  
  # Add styles to Summary tab
  print("Cleaning up Styles...")
  summaryStyle = openxlsx::createStyle(fgFill = "#404040")
  openxlsx::addStyle(wb, sheet = "Summary", summaryStyle, cols = 1:2, rows = 1:62, gridExpand = TRUE, stack = TRUE)
  
  # Add styles to Spectrum tab
  spectrumStyle1 = openxlsx::createStyle(fgFill = "#9CBEBD")
  spectrumStyle2 = openxlsx::createStyle(fgFill = "#FFEB84")
  openxlsx::addStyle(wb, sheet = "Spectrum", spectrumStyle1, cols = 1:3, rows = 1:40, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet = "Spectrum", spectrumStyle2, cols = 2, rows = 2, gridExpand = TRUE, stack = TRUE)
  
  # Add validations
  print("Adding Validations...")
  addValidations(wb,d)
  
  # Save & Export
  print("Saving...")
  d$data$output_file_path <- paste0(
    d$keychain$output_path,
    if (is.na(stringr::str_extract(d$keychain$output_path,"/$"))) {"/"} else {},
    "DataPack_",
    d$data$ou_name,
    "_",
    format(Sys.time(), "%Y%m%d%H%M%S"),
    ".xlsx"
  )
  
  openxlsx::saveWorkbook(
    wb = wb,
    file = d$data$output_file_path,
    overwrite = TRUE
  )
  print(paste0("Congratulations! Successfully saved output to ", d$data$output_file_path))
}

produceSoloSNUxIM <- function(d) {
  for (i in 1:length(d$keychain$ou_name_list)) {
    d$data$ou_name = d$keychain$ou_name_list[i]
    print(d$data$ou_name)
    
    # Get Country Info
    d <- getDPinfo(d)
    
    # Grab PSNU list
    d$data$PSNUs <- getPSNUs(d)
    
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, sheetName = "SNU x IM",zoom = 85)
    
    wb <- prepare_SNUxIM_data(wb, d)
    
    print("Saving...")
    d$data$output_file_path <- paste0(
      d$keychain$output_path,
      if (is.na(stringr::str_extract(d$keychain$output_path,"/$"))) {"/"} else {},
      "DataPack_",
      d$data$ou_name,
      "_",
      format(Sys.time(), "%Y%m%d%H%M%S"),
      ".xlsx"
    )
    
    openxlsx::saveWorkbook(
      wb = wb,
      file = d$data$output_file_path,
      overwrite = TRUE
    )
    print(paste0("Congratulations! Successfully saved output to ", d$data$output_file_path))
  }
}

DataPackGeneratR <- function(d) {
  
  #Loop through provided OUs
  for (i in 1:length(d$keychain$ou_name_list)) {
    d$data$ou_name = d$keychain$ou_name_list[i]
    print(d$data$ou_name)
    writeDataPack(d)
  }
}

loadSupportFiles <- function(d) {
  
  # Log into DATIM
  datimvalidation::loadSecrets(d$keychain$secrets)
  
  # Setup Filepaths
  d$keychain$template_path <- paste0(d$keychain$support_files_path,d$keychain$template_filename)
  
  d$keychain$model_data_path <- paste0(d$keychain$support_files_path,d$keychain$model_data_filename)
  
  d$keychain$SNUxIM_path <- paste0(d$keychain$support_files_path,d$keychain$SNUxIM_filename)
  
  d$keychain$fv_map_path <- paste0(d$keychain$support_files_path,d$keychain$fv_indicator_map)
  
  d$keychain$FACTSMechs_path <- paste0(d$keychain$support_files_path,d$keychain$FACTSMechs)
  
  d$keychain$DPconfig_path <- paste0(d$keychain$support_files_path,d$keychain$DPconfig)
  
  # Load Support Files
  d$supportFiles <- list(
    
    ## Prepare Model Data
    model_data = readRDS(d$keychain$model_data_path),
    
    ## Read in FACTS Mechs
    mechList = getFACTSMechs(d$keychain$FACTSMechs_path),
    
    ## Pull Schema from Template on the fly
    schema = template_to_schema(d$keychain$template_path),
    
    ## Regional Groupings
    configFile = readr::read_csv(d$keychain$DPconfig_path)
  )
  
  ## Prepare Mechanism data
  d$supportFiles$PSNUIM <- getSNUxIMdata(d$keychain$SNUxIM_path,
                                         d$keychain$fv_map_path,
                                         d$supportFiles$mechList)
  
  return(d)
}

ou_name_list <- c("Uganda")

support_files_path <- "/Users/sam/Desktop/temp/"

output_path <- "/Users/sam/"

secrets <- "/Users/sam/.secrets/triage.json"

### NO TOUCHY #####
library(tidyverse)
#require(devtools)
#install_github("awalker89/openxlsx")
#install.packages("devtools")
#library(devtools)
#install_github(repo="jason-p-pickering/datim-validation")

d <- list(
    keychain = list(
        ou_name_list = ou_name_list,
        support_files_path = support_files_path,
        output_path = output_path,
        secrets = secrets,
        template_filename = "COP19 Data Pack Template v1.3.xlsx",
        model_data_filename = "model_data_pack_input_cop_20_test.rds",
        SNUxIM_filename = "MER_Structured_Dataset_PSNU_IM_FY17-18_20181221_v2_1.txt",
        fv_indicator_map = "DP FV Indicator Mapping.xlsx",
        FACTSMechs = "FACTSMechanismExtract.csv",
        DPconfig = "DataPackConfiguration.csv"
    )
)

d <- loadSupportFiles(d)

DataPackGeneratR(d)

produceSoloSNUxIM(d)
