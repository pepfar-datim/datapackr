datimutils::loginToDATIM("~/.secrets/datim.json")

snuxim_model_data_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 20/3) Testing & Deployment/PSNUxIM_20200207.rds"
output_folder <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 20/3) Testing & Deployment"
model_data_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 20/3) Testing & Deployment/model_data_pack_input_20_20200220_1_flat.rds"

d <- unPackTool()

d <- checkAnalytics(d,
                    model_data_path)

d <- writePSNUxIM(d, snuxim_model_data_path, output_folder)