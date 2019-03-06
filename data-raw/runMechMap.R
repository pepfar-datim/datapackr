output_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/5) Maintenance & Support/Mechanism Maps"

FY = 2019

datapack_uid = "PqlFzhuPcF1"

secrets <- "/Users/scott/.secrets/triage.json"


## Don't touch ####
datapackr::loginToDATIM(secrets)

datapackr::packMechMap(datapack_uid, FY, output_path)