output_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/5) Maintenance & Support/Mechanism Maps"

FY = 2019

datapack_names = c("Uganda","South Sudan","Tanzania",
                   "Ethiopia","Malawi","Burundi","Rwanda")

secrets <- "/Users/scott/.secrets/triage.json"

## Don't touch ####
datapackr::loginToDATIM(secrets)

mapply(function(x) datapackr::packMechMap(x, FY, output_path),
       datapack_names)