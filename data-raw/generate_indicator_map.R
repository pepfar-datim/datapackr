getCodeLists <- function(ds,pd,lvl,rT) {
    datimvalidation::loadSecrets(secrets)
    
    de <-   utils::read.csv(
                url(
                    paste0(
                        getOption("baseurl"),
                        "api/sqlViews/DotdxKrNZxG/data.csv?var=dataSets:",
                        ds
                        )
                    ),
                stringsAsFactors=FALSE,
                header=TRUE) %>%
            dplyr::mutate(period = pd,
                          orgLevel = lvl,
                          resultsTargets = rT
            )
    return(de)
}

expandCodeLists <- function(codeList,TARDIS.filepath){
    ds <- codeList %>%
        dplyr::mutate(
        
        #Break out indicator-level fields
            splitter = stringr::str_replace_all(
                stringr::str_replace_all(dataelement,paste0(indicator," [(]"),"")
                , "\\)(.)*","")
        ) %>%
            tidyr::separate(
                col = splitter,
                into = c("A","B","C"),
                sep = ", ",
                remove = TRUE,
                fill = "right"
            ) %>%
            dplyr::mutate(
                numeratorDenom = dplyr::case_when(A %in% c("N","D") ~ A),
                supportType = dplyr::case_when(resultsTargets == "IMPT" ~ A,
                                               TRUE ~ B)) %>%
        
        #Break out disaggregate-level fields
            dplyr::mutate(
                disaggregate = dplyr::case_when(resultsTargets == "IMPT" & !is.na(B) ~ B,
                                                is.na(C) ~ "No Disagg",
                                                TRUE ~ C),
                ##Remove errant open/close parentheses at start/end of string  
                    categoryoptioncombo = stringr::str_remove_all(categoryoptioncombo,
                                                                  "^\\(|\\)$")
            ) %>%
            dplyr::select(-A,-B,-C) %>%
            tibble::add_column(Modality = NA,
                               Age = NA,
                               AggregatedAge = NA,
                               AggregatedAgeFlag = NA,
                               HIVStatus = NA,
                               KeyPop = NA,
                               KnownNewStatus = NA,
                               NewExistingART = NA,
                               Indication = NA,
                               TBStatus = NA,
                               pregBF = NA,
                               VMMCTechnique = NA,
                               ViolenceType = NA,
                               HIVSelfTest = NA,
                               HIVSelfTestUser = NA,
                               KP_Estimates = NA,
                               ARTStatus = NA,
                               HIVStatusNotes = NA,
                               OVC_ServiceArea = NA,
                               OVC_ProgramStatus = NA,
                               PMTCT_FinalOutcome = NA,
                               TB_TherapyType = NA,
                               TB_SpecimenSent = NA,
                               TB_TestType = NA,
                               PEP_Services = NA,
                               Sex = NA
                               ) %>%
            dplyr::mutate(
            ##Modality    
                Modality = dplyr::case_when(indicator == "HTS_TST" & categoryoptioncombo == "PMTCT" ~ "PMTCT ANC",
                                            indicator == "HTS_TST" & categoryoptioncombo == "Tuberculosis" ~ "TBClinic",
                                            indicator == "HTS_TST" & categoryoptioncombo == "Voluntary Medical Male Circumcision" ~ "VMMC",
                                            indicator == "HTS_TST" & categoryoptioncombo == "Other PITC" ~ "OtherPITC",
                                            indicator == "HTS_TST" & disaggregate == "FacilityDeliveryPoint" ~ stringr::str_extract(categoryoptioncombo,"Index|Inpat|Malnutrition|Outpatient|Pediatric|VCT"),
                                            indicator == "HTS_TST" & disaggregate == "CommunityDeliveryPoint" ~ paste0(stringr::str_extract(categoryoptioncombo,"Home|Index|Mobile|Other(?= Service)"),"Mod"),
                                            indicator == "HTS_TST" ~ stringr::str_extract(dataelement,"Emergency Ward|HomeMod|IndexMod|Index|Inpat|Malnutrition|MobileMod|OtherMod|OtherPITC|Pediatric|PMTCT ANC|STI Clinic|TBClinic|VCTMod|VCT|VMMC")
                                            ),
            ##Age
                Age = dplyr::case_when(stringr::str_detect(disaggregate,"Age") ~ stringr::str_trim(stringr::str_extract(categoryoptioncombo,"(?<!D)(<|<=)?( )?\\d{1,2}( months)?(( )?-( )?\\d{1,2}( months| years)?|\\+)?|Unknown Age")),
                                      stringr::str_detect(dataelement,"HTS_TST(.)+(Malnutrition|Pediatric)") ~ "<5",
                                      indicator == "PMTCT_EID" & stringr::str_detect(categoryoptioncombo,"Infant Test(.)+2 and 12") ~ "2 - 12 months",
                                      indicator == "PMTCT_EID" & stringr::str_detect(categoryoptioncombo,"Infant Test(.)+within 2 months") ~ "<= 2 months",
                                      TRUE ~ NA_character_),
            ##AggregatedAge
                AggregatedAge = dplyr::case_when(stringr::str_detect(Age,"^(< 2 months|<= 2 months|2 - 12 months|<1|<5|1-9|2 months - 9 years|<10|10-14|<15)$") ~ "<15",
                                                 stringr::str_detect(Age,"^(15-17|15-19|18-24|20-24|25-29|15-29|30-34|35-39|40-49|30-49|25-49|15\\+|18\\+|25\\+|30\\+|50\\+)$") ~ "15+",
                                                 stringr::str_detect(Age,"Unknown Age") ~ "Unknown Age"
                                                 ),
            ##Aggregated Age Flag (to distinguish otherwise identical indicators)
                AggregatedAgeFlag = dplyr::case_when(stringr::str_detect(dataelement,"Age Aggregated|(PLHIV|HIV_PREV)(.)+Age") ~ "AgeAggregated"),
            ##HIVStatus
                HIVStatus = dplyr::case_when(
                    ###Implied Positives
                        indicator %in% c("IMPATT.PLHIV","TX_NEW","TX_CURR","TX_RET","TX_PVLS","PMTCT_ART","TB_ART","TX_TB","TB_SCREENDX","TB_PREV") ~ "HIV Positive",
                    ###Test Result Status
                        indicator %in% c("HTS_TST","PMTCT_EID","PMTCT_STAT","PMTCT_STAT_SUBNAT","OVC_HIVSTAT","TB_STAT","VMMC_CIRC") & stringr::str_detect(categoryoptioncombo,"Positive|Negative") ~ paste0("HIV ",stringr::str_extract(categoryoptioncombo,"Positive|Negative")),
                        indicator %in% c("PMTCT_EID","VMMC_CIRC") & stringr::str_detect(categoryoptioncombo,"Unknown") ~ "HIV Status Unknown",
                    ###Already Identified Status
                        indicator %in% c("KP_PREV","PP_PREV") & stringr::str_detect(categoryoptioncombo,"Declined Testing|Newly Tested") ~ "HIV Status Unknown",
                        indicator %in% c("KP_PREV","PP_PREV") & stringr::str_detect(categoryoptioncombo,"Known at Entry Positive") ~ "HIV Positive",
                        indicator == "PMTCT_FO" & stringr::str_detect(categoryoptioncombo,"HIV-infected") ~ "HIV Positive",
                        indicator == "PMTCT_FO" & stringr::str_detect(categoryoptioncombo,"HIV-uninfected") ~ "HIV Negative",
                        indicator == "PMTCT_FO" & stringr::str_detect(categoryoptioncombo,"unknown") ~ "HIV Status Unknown",
                        indicator == "OVC_HIVSTAT" & categoryoptioncombo %in% c("No HIV Status","Test Not Indicated","Other Reasons") ~ "HIV Status Unknown",
                        indicator == "OVC_HIVSTAT" & categoryoptioncombo %in% c("Not Receiving ART","Receiving ART") ~ "HIV Positive"
                        ),
            ##KeyPop
                KeyPop = case_when(indicator == "KP_MAT" & stringr::str_detect(categoryoptioncombo,"Unknown Sex|default") ~ "PWID",
                                   indicator == "KP_MAT" ~ paste0(stringr::str_extract(categoryoptioncombo,"Female|Male")," PWID"),
                                   TRUE ~ stringr::str_extract(categoryoptioncombo,"Female PWID|Male PWID|PWID|FSW|MSM not SW|MSM SW|MSM|Other Key Populations|People in prisons and other enclosed settings|TG not SW|TG SW|TG")),
            ##KnownNewStatus
                KnownNewStatus = stringr::str_extract(categoryoptioncombo,"Known at Entry|Newly Identified|Newly Tested or Testing Referred|Declined Testing Or Testing Referral"),
            ##NewExistingART
                NewExistingART = dplyr::case_when(stringr::str_detect(categoryoptioncombo,"Life-long ART, Already") ~ "Already on ART",
                                                  stringr::str_detect(categoryoptioncombo,"Life-long ART, New") ~ "New on ART",
                                                  indicator == "TX_NEW" ~ "New on ART",
                                                  indicator == "PMTCT_ART_SUBNAT" ~ stringr::str_extract(categoryoptioncombo,"Other - ART")),
            ##Indication
                Indication = dplyr::case_when(indicator %in% c("TX_PVLS","VL_SUPPRESSION_SUBNAT") ~ stringr::str_extract(categoryoptioncombo,"Routine|Targeted|Undocumented")),
            ##TB Status
                TBStatus = dplyr::case_when(
                    ###Implied TB Status
                        indicator %in% c("TB_ART","TB_STAT") ~ "TB Positive",
                        indicator == "TX_TB" & numeratorDenom == "N" ~ "TB Positive",
                        indicator == "TX_NEW" & stringr::str_detect(disaggregate,"TB Diagnosis") ~ "TB Positive",
                        #What about HTS_TST TB Clinic screening?
                    ###TB Screen Result Status
                        indicator == "TX_TB" & numeratorDenom == "D" & stringr::str_detect(disaggregate,"PositiveScreen") ~ paste0("TB ",stringr::str_extract(categoryoptioncombo,"Positive|Negative")),
                        indicator == "TX_TB" & numeratorDenom == "D" & stringr::str_detect(disaggregate,"TBScreen") ~ paste0("TB ",stringr::str_extract(categoryoptioncombo,"(?<=, TB Screen - )Positive|Negative")),
                        indicator == "TB_SCREENDX" & stringr::str_detect(disaggregate,"Screened Positive|TB Test Type|Specimen Sent") ~ "TB Positive"
                    ),
            ##Pregnant or Breastfeeding
                pregBF = dplyr::case_when(
                    ###Implied Pregnant
                        indicator == "HTS_TST" & Modality == "PMTCT ANC" ~ "Pregnant",
                        indicator %in% c("PMTCT_STAT","PMTCT_ART","PMTCT_STAT_SUBNAT","PMTCT_ART_SUBNAT") ~ "Pregnant",
                    ###Designated Pregnant/Breastfeeding
                        indicator %in% c("TX_NEW","TX_RET","TX_PVLS") ~ stringr::str_extract(categoryoptioncombo,"Pregnant|Breastfeeding")
                    ),
            ##VMMC Technique
                VMMCTechnique = dplyr::case_when(
                        indicator == "VMMC_CIRC" ~ stringr::str_extract(categoryoptioncombo,"Surgical|Device")
                        ),
            ##Gender-based Violence Service Type
                ViolenceType = dplyr::case_when(
                        indicator == "GEND_GBV" & stringr::str_detect(disaggregate,"PEP") ~ "Sexual Violence (Post-Rape Care",
                        indicator == "GEND_GBV" ~ stringr::str_extract(categoryoptioncombo,"Physical and\\/or Emotional Violence|Sexual Violence \\(Post-Rape Care")
                        ),
            ##HIV Self Test Designations
                HIVSelfTest = dplyr::case_when(
                        indicator == "HTS_SELF" ~ stringr::str_extract(categoryoptioncombo,"Directly-Assisted|Unassisted")
                        ),
                HIVSelfTestUser = dplyr::case_when(
                        disaggregate == "HIVSelfTestUser" ~ stringr::str_extract(categoryoptioncombo,"Self|Sex Partner|Other")
                        ),
            ##KP Estimates
                KP_Estimates = dplyr::case_when(
                        indicator == "KP_ESTIMATES" ~ stringr::str_extract(disaggregate,"PositiveEstimate|Prevalence|TotalSizeEstimate")
                        ),
            ##ART Status
                ARTStatus = dplyr::case_when(
                    ###Implied ART Status
                        indicator %in% c("TX_CURR","TX_NEW","TX_PVLS","TX_RET","TB_PREV","TX_TB") ~ "Receiving ART",
                        indicator %in% c("TB_ART","PMTCT_ART","PMTCT_ART_SUBNAT") & numeratorDenom == "N" ~ "Receiving ART",
                    ###Desginated ART Status
                        indicator %in% c("PMTCT_HEI_POS","OVC_HIVSTAT") ~ stringr::str_extract(categoryoptioncombo,"Not Receiving ART|Receiving ART"),
                        NewExistingART %in% c("New on ART","Already on ART") ~ "Receiving ART"
                    ),
            ##HIV Status Notes (to capture OVC_HIVSTAT disaggs)
                HIVStatusNotes = dplyr::case_when(
                        disaggregate == "StatusNotRep" ~ stringr::str_extract(categoryoptioncombo,"Other Reasons|Test Not Indicated")
                    ),
            ##OVC Service Area
                OVC_ServiceArea = dplyr::case_when(
                        indicator == "OVC_SERV" ~ stringr::str_extract(categoryoptioncombo,"Economic Strengthening|Education Support|Other Service Areas|Parenting\\/Caregiver Programs|Social Protection")
                    ),
            ##OVC Program Status
                ###May need to come back to parse out ProgramStatus and TransferExit disaggs -- only relevant for 2018 Results.
                OVC_ProgramStatus = dplyr::case_when(
                        indicator == "OVC_SERV" ~ stringr::str_extract(categoryoptioncombo,"Active|Exited without Graduation|Graduated|Transferred out - non PEPFAR Support Partner|Transferred out - PEPFAR Support Partner|Transferred")
                    ),
            ##PMTCT_FinalOutcome
                PMTCT_FinalOutcome = dplyr::case_when(
                        indicator == "PMTCT_FO" ~ stringr::str_extract(categoryoptioncombo,"HIV-final status unknown|HIV-infected|HIV-uninfected|Other Outcomes: Died")
                    ),
            ##TB_TherapyType
                TB_TherapyType = dplyr::case_when(
                        indicator == "TB_PREV" & stringr::str_detect(disaggregate,"TherapyType") ~ stringr::str_extract(categoryoptioncombo,"IPT|Alternative")
                    ),
            ##TB_SpecimenSent
                TB_SpecimenSent = dplyr::case_when(
                        indicator == "TB_SCREENDX" & stringr::str_detect(disaggregate,"Specimen Sent|TB Test Type") ~ "Specimen Sent"
                    ),
            ##TB_TestType
                TB_TestType = dplyr::case_when(
                        indicator %in% c("TB_SCREENDX","TX_TB") & stringr::str_detect(disaggregate,"TB Test Type") ~ stringr::str_extract(categoryoptioncombo,"Other|Smear|Xpert")
                    ),
            ##GEND_GBV PEP Services
                PEP_Services = dplyr::case_when(
                        indicator == "GEND_GBV" & stringr::str_detect(disaggregate,"PEP") ~ "Receiving PEP"
                    ),
            ##Sex
                Sex = dplyr::case_when(
                    ###Implied Female
                        stringr::str_detect(dataelement, "PMTCT(?!_(FO|EID|HEI))") ~ "Female",
                        Modality == "PMTCT ANC" ~ "Female",
                        stringr::str_detect(KeyPop,"FSW") ~ "Female",
                        pregBF %in% c("Breastfeeding","Pregnant") ~ "Female",
                    ###Implied Male
                        stringr::str_detect(dataelement,"VMMC") ~ "Male",
                        Modality == "VMMC" ~ "Male",
                        stringr::str_detect(KeyPop,"MSM") ~ "Male",
                    ###Implied Unknown
                        stringr::str_detect(disaggregate,"AgeLessThanTen") ~ "Unknown Sex",
                    ###Other
                        #stringr::str_detect(categoryoptioncombo,"TG") ~ "Other",
                    ###Designated Male/Female
                        TRUE ~ stringr::str_extract(categoryoptioncombo,"Male|Female|Unknown Sex")
                    ),
            ##Fix PMTCT_EID_POS_12MO & PMTCT_EID_POS_2MO
                Age = dplyr::case_when(
                        indicator == "PMTCT_EID_POS_12MO" ~ "2 - 12 months",
                        indicator == "PMTCT_EID_POS_2MO" ~ "<= 2 months",
                        TRUE ~ Age
                    ),
                AggregatedAge = dplyr::case_when(
                        indicator %in% c("PMTCT_EID_POS_12MO","PMTCT_EID_POS_2MO") ~ "<15",
                        TRUE ~ AggregatedAge
                    ),
                HIVStatus = dplyr::case_when(
                        indicator %in% c("PMTCT_EID_POS_12MO","PMTCT_EID_POS_2MO") ~ "HIV Positive",
                        TRUE ~ HIVStatus
                    ),
                indicator = stringr::str_replace(indicator,"PMTCT_EID_POS_(1)?2MO","PMTCT_EID"),
                indicatorCode = paste(
                    indicator,numeratorDenom,supportType,disaggregate,resultsTargets,
                    Age,AggregatedAge,AggregatedAgeFlag,
                    Sex,
                    Modality,HIVStatus,HIVStatusNotes,KnownNewStatus,HIVSelfTest,HIVSelfTestUser,
                    KeyPop,KP_Estimates,
                    ARTStatus,NewExistingART,
                    Indication,
                    TBStatus,TB_TherapyType,TB_SpecimenSent,TB_TestType,
                    pregBF,
                    PMTCT_FinalOutcome,
                    VMMCTechnique,
                    ViolenceType,PEP_Services,
                    OVC_ServiceArea,
                    OVC_ProgramStatus
                    ,sep="|")
            )
    
    #Add TARDIS (Time and Relative Dimension Indicator Sequence) to link indicators across
    #       time period, org level (community/facility), targets/results datasets, and across
    #       MER versions, where these indicators translate to the same measure in practice.
    
    TARDIS.uids <- read.csv(TARDIS.filepath,stringsAsFactors = F) %>%
        select(indicatorCode,TARDIS = indicatoruid)
    
        ds <- ds %>%
            dplyr::left_join(TARDIS.uids, by = "indicatorCode")
    
    return(ds)
}

generate_indicator_map <- function(input.datasets,indicator.filters,TARDIS.filepath){
    #Download code lists selected in input.datasets
        indicator.map<-NULL
        for (i in 1:NROW(input.datasets)) {
            indicator.map <- dplyr::bind_rows(indicator.map,
                                       getCodeLists(ds = input.datasets$dataset[i],
                                                    pd = input.datasets$period[i],
                                                    lvl = input.datasets$orgLevel[i],
                                                    rT = input.datasets$resultsTargets[i]
                                                   )
                                       )
        }
        
        indicator.map <- indicator.map %>%
            dplyr::mutate(
                indicator = stringr::str_extract(dataelement,"^(\\S)+(?=\\s)")
            ) %>%
            dplyr::select(-dataset,-categoryoptioncombocode,-dataelementdesc,-shortname)
    
    #Setup filters selected in indicator.filters
        fields <- indicator.map %>%
            dplyr::select(indicator) %>%
            unique()
        if (indicator.filters$filter_function == "excludes") {
             fields <- fields %>%
                dplyr::filter(!indicator %in% indicator.filters$fields)
        } else {
            fields <- fields %>%
                dplyr::filter(indicator %in% indicator.filters$fields)
        }
  
        indicator.map <- indicator.map %>%
            dplyr::filter(indicator %in% fields$indicator)
        
    #Expand Code Lists
        indicator.map <- expandCodeLists(codeList = indicator.map,TARDIS.filepath = TARDIS.filepath)
        
        
    return(indicator.map)
    
}