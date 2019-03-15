output_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/5) Maintenance & Support/Mechanism Maps"
FACTSMechs_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/3) Testing & Deployment/Support Files/FACTSMechanismExtract.csv"

datapack_names = c("Botswana","Lesotho","Mozambique",
                   "Namibia","Eswatini","Angola","Zambia",
                   "Zimbabwe","South Africa")

secrets <- "/Users/scott/.secrets/datim.json"

## Don't touch ####
datapackr::loginToDATIM(secrets)

mapply(function(x) datapackr::packMechMap(datapack_name = x,
                                          FY = datapackr::cop_year(),
                                          output_path = output_path,
                                          includeFACTS = TRUE,
                                          FACTSMechs_path = FACTSMechs_path),
       datapack_names)

mechMap_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/5) Maintenance & Support/Final Data Packs/Mechanism Map_Tanzania_11March2019_Merged.xlsx"
m <- datapackr::unPackMechanismMap(mechMap_path)