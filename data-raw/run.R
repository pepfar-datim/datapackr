
support_files_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/3) Testing & Deployment/Support Files/"

output_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 19/5) Maintenance & Support/Draft Data Packs/Outputs"

secrets <- "/Users/scott/.secrets/datim.json"

### NO TOUCHY #####
library(tidyverse)
library(magrittr)


unPackData(
    support_files_path,
    output_path,
    secrets)
