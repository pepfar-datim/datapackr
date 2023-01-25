context("Create a COP23 Target Setting Tool")

with_mock_api({
  test_that("We can write an COP23 Target Setting tool", {

    # For Generating Individual Data Packs ####
    generation_list <- c("Eswatini")

    pick <- datapackr::COP21_datapacks_countries %>%
      dplyr::filter(datapack_name %in% generation_list) %>%
      dplyr::arrange(datapack_name)

    output_folder <- paste0("/tmp/", stringi::stri_rand_strings(1, 20))
    dir.create(output_folder)

    #Suppress console output

    d <- packTool(model_data_path = test_sheet("COP23_model_data_random.rds"),
      tool = "Data Pack",
                  datapack_name = pick$datapack_name[1],
                  country_uids = unlist(pick$country_uids[1]),
                  template_path = test_sheet("COP23_Data_Pack_Template.xlsx"),
                  cop_year = 2023,
                  output_folder = output_folder,
                  results_archive = FALSE,
                  expand_formulas = TRUE,
                  d2_session = training)

  expect_setequal(names(d), c("keychain", "info", "tool", "data"))
  expect_equal("Eswatini", d$info$datapack_name)

  skip("Need to add Spectrum data perhaps?")
  #Be sure we can unpack what we just wrote to a file
  d_out <- unPackTool(submission_path = d$info$output_file, d2_session = training)


})
})
