## Save Valid COs ####

secrets <- "/Users/scott/.secrets/test-mer2.json"

loginToDATIM(secrets)

valid_category_options <- getValidCategoryOptions(cop_year = getCurrentCOPYear())

save(valid_category_options, file = "./data/valid_category_options.rda", compress = "xz")