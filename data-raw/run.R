support_files <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/3) Testing & Deployment/Support Files"
output_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/5) Maintenance & Support/Final Data Packs/Outputs"
secrets <- "/Users/scott/.secrets/datim.json"

### END EDITS #####
# devtools::install_git(url = "https://github.com/pepfar-datim/data-pack-commons.git",
#                       ref = "prod",
#                       upgrade = FALSE)



devtools::install_git(url = "https://github.com/pepfar-datim/datapackr.git",
                      ref = "master",
                      upgrade = FALSE)

datapackr::loginToDATIM(secrets)

# Read Data Pack
d <- datapackr::unPackData()

# Grab densities
density_data_files <-
  list.files(
    paste0(support_files,
           if (is.na(stringr::str_extract(support_files,"/$"))) {"/"} else {},
           "density_data/"),
    pattern = d$info$datapack_name) %>% 
  sort(decreasing = TRUE)

density <- readr::read_rds(
  paste0(support_files,
         if (is.na(stringr::str_extract(support_files,"/$"))) {"/"} else {},
         "density_data/",
         density_data_files[1]
         )
  )

# Pull Mechanism Information
if(!exists("mechanisms_19T")){
  mechanisms_19T <<- datapackcommons::Get19TMechanisms(getOption("baseurl"))
}

# Distribute to Site
d <- datapackcommons::DistributeToSites(d,
                                        mechanisms = mechanisms_19T,
                                        site_densities = density)

# Pack Site Tool
FACTSMechs_path = paste0(
  support_files,
  if (is.na(stringr::str_extract(support_files,"/$"))) {"/"} else {},
  "FACTSMechanismExtract.csv")

d <- readRDS("/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/5) Maintenance & Support/Final Data Packs/Namibia_distributed_20190314_213054.rds")

datapackr::packSiteTool(d,
                        includeFACTS = TRUE,
                        FACTSMechs_path = FACTSMechs_path,
                        output_path = output_path)



datapackr::exportPackr(d,
                       output_path,
                       type = "Results Archive",
                       d$info$datapack_name)

filepath = paste0(output_path,
              if (is.na(stringr::str_extract(output_path,"/$"))) {"/"} else {},
              type,"_",
              d$info$datapack_name,"_",
              format(Sys.time(), "%Y%m%d%H%M%S"),
              extension
            )

readr::write_csv(d$datim$PSNUxIM,
                 filepath)


mechs <- datapackr::getMechList(include_dedupe = TRUE,
                                FY = 2020)
