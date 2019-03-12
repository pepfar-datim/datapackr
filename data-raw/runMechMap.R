output_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/5) Maintenance & Support/Mechanism Maps"
FACTSMechs_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/3) Testing & Deployment/Support Files/FACTSMechanismExtract.csv"

FY = 2019

datapack_names = c("Uganda","South Sudan","Tanzania",
                   "Ethiopia","Malawi","Burundi","Rwanda")

secrets <- "/Users/scott/.secrets/triage.json"

## Don't touch ####
datapackr::loginToDATIM(secrets)

mapply(function(x) datapackr::packMechMap(datapack_name = x,
                                          FY = FY,
                                          output_path = output_path,
                                          includeFACTS = TRUE,
                                          FACTSMechs_path = FACTSMechs_path),
       datapack_names)