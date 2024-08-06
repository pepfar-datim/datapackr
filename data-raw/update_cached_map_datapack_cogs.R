# Load PSNUs into package from DATIM ####
library(magrittr)
library(datapackr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

# Processing
datapack_cogs <- datapackr::datapack_cogs

datapack_cogs$COP23 <-
  datimutils::getMetadata(categoryOptionGroups,
                          fields = "id,name,categoryOptions[id,name]", # nolint
                          "groupSets.id:eq:CIqgMytqbMA",
                          d2_session = d2_default_session)

datapack_cogs$COP24 <-
  datimutils::getMetadata(categoryOptionGroups,
                          fields = "id,name,categoryOptions[id,name]", # nolint
                          "groupSets.id:eq:NRX13fNOP2L",
                          d2_session = d2_default_session)

datapack_cogs$COP25 <-
  datimutils::getMetadata(categoryOptionGroups,
                          fields = "id,name,categoryOptions[id,name]", # nolint
                          "groupSets.id:eq:qUYbKI4AWHW",
                          d2_session = d2_default_session)

waldo::compare(datapackr::datapack_cogs, datapack_cogs)

usethis::use_data(datapack_cogs, overwrite = TRUE, compress = "xz")
