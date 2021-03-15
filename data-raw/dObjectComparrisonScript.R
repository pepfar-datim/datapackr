#Note to update the custom libraries the following pattern needs to be followed:
#mkdir
#chmod u+w

# options(install.opts = "--no-staged-install")
# 
#remotes::install_github(repo = "https://github.com/pepfar-datim/datapackr.git", ref = "master", force = T, lib = "tests/custom_datapackr/master")

#################################################################

datimutils::loginToDATIM("~/.secrets/datim.json")
.libPaths( c( .libPaths(), "~/datapackr/tests/custom_datapackr") )
analytics_data_path <- "~/datapackr/tests/datapackr_test_files/Testing/With PSNUxIM/Data Pack_Malawi_20210121230425.xlsx"
model_data_path <- "~/datapackr/tests/datapackr_test_files/Testing/support_files/model_data_pack_input_21_20210208_1_flat.rds"

create_d_objects <- function(branch_1, branch_2){
  
  unloadNamespace("datapackr")
  search_item <- paste("package", "datapackr", sep = ":")
  
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
  library(datapackr, lib.loc=paste0("~/datapackr/tests/custom_datapackr/", branch_1))
  
  print(paste0("GENERATING OBJECTS WITH PACKGE VERSION: ", packageVersion("datapackr")))
  
  capture.output(b1_d1 <- datapackr::unPackTool( analytics_data_path
                            , d2_session = d2_default_session
))


  capture.output(b1_d2 <- datapackr::checkAnalytics(b1_d1,
                               model_data_path
                               ,d2_session = d2_default_session
))


unloadNamespace("datapackr")
search_item <- paste("package", "datapackr", sep = ":")

while(search_item %in% search())
{
  detach(search_item, unload = TRUE, character.only = TRUE)
}
library(datapackr, lib.loc=paste0("~/datapackr/tests/custom_datapackr/", branch_2))

print(paste0("GENERATING OBJECTS WITH PACKGE VERSION: ", packageVersion("datapackr")))

capture.output(b2_d1 <- datapackr::unPackTool( analytics_data_path
                                , d2_session = d2_default_session
))


capture.output(b2_d2 <- datapackr::checkAnalytics(b2_d1,
                                   model_data_path
                                   ,d2_session = d2_default_session
))


return(list(b1_d1,b2_d1,b1_d2,b2_d2))

}

objects_view <- create_d_objects("master","dev")
all.equal(objects_view[[1]], objects_view[[2]], use.names=T)


