# Load PSNUs into package from DATIM ####
library(magrittr)
library(datapackr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "cop-test.json")
datimutils::loginToDATIM(secrets)

# Processing
datapack_cogs <- datapackr::datapack_cogs

datapack_cogs$COP22 <-
    datimutils::getMetadata(categoryOptionGroups,
                            fields = "id,name,categoryOptions[id,name]", # nolint
                            "groupSets.name:like:COP 22 Data Pack",
                            d2_session = d2_default_session)

datapack_cogs$COP23 <-
  datimutils::getMetadata(categoryOptionGroups,
                          fields = "id,name,categoryOptions[id,name]", # nolint
                          "groupSets.name:like:COP 23 Data Pack",
                          d2_session = d2_default_session)

save(datapack_cogs, file = "./data/datapack_cogs.rda", compress = "xz")
