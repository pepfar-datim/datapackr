support_files <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/3) Testing & Deployment/Support Files"
output_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/5) Maintenance & Support/Draft Data Packs/Outputs"

### NO TOUCHING #####

#Read Data Pack
d <- datapackr::unPackData(output_path = output_path)

# Grab density
density <- readDensity(support_files)

#Distribute to Site
d <- datapackcommons::DistributeToSites(d,
                                        mechanisms = mechanisms_19T,
                                        site_densities = density)

# Pack Site Tool
datapackr::packSiteTool(d)


View(eswatini_site_density$data$site$distributed)
View(d$data$site$distributed)