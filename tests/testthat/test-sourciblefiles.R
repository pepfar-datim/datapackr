
test_that("pack a cop 21 data pack", {
  source("data-raw/update_cached_PSNUs.R")
  source("data-raw/update_cached_de_coc_co_map.R")
  source("data-raw/update_cop21_datapack_schema.R")
  source("data-raw/produceConfigFile.R")
  
  cleanup()
  
})