output_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/5) Maintenance & Support/Mechanism Maps"

datapack_names = c("Cameroon","Cote d'Ivoire","Democratic Republic of the Congo",
                   "Nigeria","Ukraine","Vietnam","West Africa Region")

secrets <- "/Users/scott/.secrets/datim.json"

## Don't touch ####
datapackr::loginToDATIM(secrets)

mapply(function(x) datapackr::packMechMap(datapack_name = x,
                                          FY = datapackr::getCurrentCOPYear(),
                                          output_path = output_path),
       datapack_names)

mechMap_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/5) Maintenance & Support/Final Data Packs/Mechanism Map_Tanzania_11March2019_Merged.xlsx"
m <- datapackr::unPackMechanismMap(mechMap_path)