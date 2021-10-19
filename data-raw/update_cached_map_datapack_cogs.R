# Load PSNUs into package from DATIM ####
library(magrittr)
library(datapackr)

secrets <- "~/.secrets/datim.json"

datapackr::loginToDATIM(secrets)

# Processing
datapack_cogs <-
  datimutils::getMetadata(categoryOptionGroups,
                          fields = "id,name,categoryOptions[id,name]", # nolint
                          "groupSets.name:like:COP 21 Data Pack",
                          d2_session = d2_default_session)

save(datapack_cogs, file = "./data/datapack_cogs.rda", compress = "xz")