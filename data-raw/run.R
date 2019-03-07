support_files <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/3) Testing & Deployment/Support Files"
output_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/5) Maintenance & Support/Draft Data Packs/Outputs"
secrets <- "/Users/scott/.secrets/triage.json"

### END EDITS #####
datapackr::loginToDATIM(secrets)

#Read Data Pack
d <- datapackr::unPackData(output_path = output_path)

# Grab density
density <- readDensity(support_files)

#Distribute to Site
d <- datapackcommons::DistributeToSites(d,
                                        mechanisms = mechanisms_19T,
                                        site_densities = density)

d <- eswatini_site_density

# Pack Site Tool
datapackr::packSiteTool(d)
