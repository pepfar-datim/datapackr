### CHANGE ME ######

#batch1 <- c("Angola","Botswana","Burundi","Cameroon")
#batch2 <- c("Dominican Republic","Eswatini")
#batch3 <- c("Haiti","Lesotho","Malawi","Namibia","Rwanda","South Sudan")
#batch4 <- c("Ukraine","Vietnam","Zambia","Zimbabwe","Cote d'Ivoire")
#longBatch <- c("Ethiopia","Democratic Republic of the Congo","Kenya","South Africa","Nigeria")
#medBatch <- c("Mozambique","Tanzania","Uganda")
#regionalBatch <- c("Asia Region","Caribbean Region","Central America Region","West-Central Africa Region")
# Asia Region took FOREVER too

#problemBatch <- c("Nigeria","Ethiopia","Democratic Republic of the Congo")

ou_name_list <- c("Asia Region")

support_files_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/3) Testing & Deployment/Support Files/"

output_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/3) Testing & Deployment/Data Packs/"

secrets <- "/Users/scott/.secrets/datim.json"

### NO TOUCHY #####
library(tidyverse)
#require(devtools)
#install_github("awalker89/openxlsx")
#install.packages("devtools")
#library(devtools)
#install_github(repo="jason-p-pickering/datim-validation")

d <- list(
    keychain = list(
        ou_name_list = ou_name_list,
        support_files_path = support_files_path,
        output_path = output_path,
        secrets = secrets,
        template_filename = "COP19 Data Pack Template v1.3.xlsx",
        model_data_filename = "regional_model_data_pack_input_20190118.rds",
        SNUxIM_filename = "MER_Structured_Dataset_PSNU_IM_FY17-18_20181221_v2_1.txt",
        fv_indicator_map = "DP FV Indicator Mapping.xlsx",
        FACTSMechs = "FACTSMechanismExtract.csv",
        DPconfig = "DataPackConfiguration.csv"
    )
)

d <- loadSupportFiles(d)

DataPackGeneratR(d)

produceSoloSNUxIM(d)
