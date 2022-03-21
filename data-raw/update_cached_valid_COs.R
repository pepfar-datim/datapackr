## Save Valid COs ####

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "test-mer2.json")
datimutils::loginToDATIM(secrets)

valid_category_options <- getValidCategoryOptions(cop_year = getCurrentCOPYear())

save(valid_category_options, file = "./data/valid_category_options.rda", compress = "xz")