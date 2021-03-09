output_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/5) Maintenance & Support/Site Lists"

datapack_names <- c("Nigeria","West Africa Region")

secrets <- "/Users/scott/.secrets/datim.json"
datapackr::loginToDATIM(secrets)

for (i in 1:length(datapack_names)) {
  datapack_name <- datapack_names[i] 
  
  country_uids <- datapackr::dataPackMap %>%
    dplyr::filter(data_pack_name == datapack_name) %>%
    dplyr::select(country_uid) %>%
    dplyr::distinct() %>%
    dplyr::pull(country_uid)
  
  sites <- getSiteList(country_uids = country_uids,
                        include_mil = TRUE) %>%
    dplyr::select(country_name, country_uid, psnu, psnu_uid, orgunit = name,
                  orgunituid = id, site_type) %>%
    dplyr::arrange(country_name, psnu, orgunit, site_type)
  
  path <- paste0(output_path,
                 if (is.na(stringr::str_extract(output_path,"/$"))) {"/"} else {},
                 "SiteList","_",
                 datapack_name,"_",
                 format(Sys.time(), "%Y%m%d%H%M%S"),
                 ".csv")
  
  readr::write_csv(sites, path)
  
}

