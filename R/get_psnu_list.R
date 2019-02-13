#Produce PSNU List (for UNAIDS use in Spectrum, and other purposes as needed)

# install.packages("httr")
# install.packages("jsonlite")
# install.packages("dplyr")
# install.packages("stringr")

#CHANGE ME!
secrets <- "/Users/scott/.secrets/datim.json"
outFolder <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/3) Testing & Deployment/Spectrum"




#DON'T CHANGE!

require(dplyr)
require(httr)
require(jsonlite)
require(stringr)



get_psnu_list <- function(secrets) {
    
    datimvalidation::loadSecrets(secrets)
    
    psnu_levels <-
        paste0(getOption("baseurl"),
               "api/dataStore/dataSetAssignments/ous") %>%
        httr::GET() %>%
        httr::content(., "text") %>%
        jsonlite::fromJSON(., flatten = TRUE) %>%
        do.call(rbind.data.frame, .) %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        dplyr::mutate(country_name = dplyr::case_when(
            country == 3 ~ name3,
            country == 4 ~ name4
            )) %>%
        dplyr::select(operating_unit = name3, country_name, country, prioritization, planning, community, facility)
    
    orgHierarchy <-
        paste0(getOption("baseurl"), "/api/sqlViews/kEtZ2bSQCu2/data.json") %>%
        httr::GET() %>%
        httr::content(., "text") %>%
        jsonlite::fromJSON(., flatten = TRUE)
    
    ous_list <- as.data.frame(orgHierarchy$rows,stringsAsFactors = FALSE) %>% 
        setNames(.,orgHierarchy$headers$name) %>%
        dplyr::mutate(
            military = dplyr::case_when(stringr::str_detect(name, "_Military") ~ "Y"),
            country_name = dplyr::case_when(
                stringr::str_detect(level3name, "Region") & level4name != "" ~ level4name,
                TRUE ~ level3name)) %>%
        dplyr::inner_join(psnu_levels,by=c("country_name")) %>%
        dplyr::mutate(level = as.numeric(level)) %>%
        filter(level == planning | military == "Y") %>%
        select(operating_unit = level3name, country_name, name, uid = organisationunituid, everything(),
               -planning, -military, -prioritization, -community, -facility, -organisationunitid, -level, -country, -operating_unit,
               -uidlevel7, -level7name, -uidlevel8, -level8name, -uidlevel9, -level9name) %>%
        unique()
    
    return(ous_list)
}

get_psnu_list(secrets) %>%
    write.csv(paste0(outFolder,"/psnu_list.csv"), row.names = FALSE)