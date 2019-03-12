support_files <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/3) Testing & Deployment/Support Files"
output_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/5) Maintenance & Support/Final Data Packs/Outputs"
secrets <- "/Users/scott/.secrets/triage.json"

### END EDITS #####
# devtools::install_git(url = "https://github.com/pepfar-datim/data-pack-commons.git",
#                       ref = "prod",
#                       upgrade = FALSE)



devtools::install_git(url = "https://github.com/pepfar-datim/datapackr.git",
                      ref = "prod",
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
  if (is.na(stringr::str_extract(output_path,"/$"))) {"/"} else {},
  "FACTSMechanismExtract.csv")

datapackr::packSiteTool(d,
                        includeFACTS = TRUE,
                        FACTSMechs_path = FACTSMechs_path,
                        output_path = output_path)
