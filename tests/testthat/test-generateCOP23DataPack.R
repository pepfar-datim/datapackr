context("Create a COP23 Target Setting Tool")

with_mock_api({
  test_that("We can write an COP23 Target Setting tool", {

    template_path <- file.path(system.file("extdata", package = "datapackr"), "COP23_Data_Pack_Template.xlsx")

    expect_true(file.exists(template_path))

    # For Generating Individual Data Packs ####
    generation_list <- c("Malawi")

    pick <- datapackr::COP21_datapacks_countries %>%
      dplyr::filter(datapack_name %in% generation_list) %>%
      dplyr::arrange(datapack_name)

    output_folder <- paste0("/tmp/", stringi::stri_rand_strings(1, 20))
    dir.create(output_folder)

    #Suppress console output

    spectrum_data <- readRDS(test_sheet("COP23_spectrum_data_random_MW.rds"))

    d <- packTool(model_data_path = test_sheet("COP23_datapack_model_data_random_MW.rds"),
      tool = "Data Pack",
                  datapack_name = pick$datapack_name[1],
                  country_uids = unlist(pick$country_uids[1]),
                  template_path = template_path,
                  cop_year = 2023,
                  output_folder = output_folder,
                  results_archive = FALSE,
                  expand_formulas = TRUE,
                  spectrum_data = spectrum_data,
                  d2_session = training)

  expect_setequal(names(d), c("keychain", "info", "tool", "data"))
  expect_equal("Malawi", d$info$datapack_name)

  #Open the generated tool in libreoffice to kick off the formulas
  #Do not even try and do this on Windows
  skip_if(Sys.info()["sysname"] == "Windows")

  #MacOS users will need to install LibreOffice
  lo_path <- ifelse(Sys.info()["sysname"] == "Darwin",
                    "/Applications/LibreOffice.app/Contents/MacOS/soffice",
                    #Needs to be relative, but can't figure out terminal command
                    #got to ls /Applications/ | grep -i libre
                    system("which libreoffice", intern = TRUE))

  #Skip this if we cannot execute libreoffice
  skip_if(file.access(lo_path, 1) != 0)

  out_dir <- paste0(output_folder, "/out")
  dir.create(out_dir)

  Sys.setenv(LD_LIBRARY_PATH = ifelse(Sys.info()["sysname"] == "Darwin",
                                      "/Applications/LibreOffice.app/Contents/MacOS/soffice",
                                      "/usr/lib/libreoffice/program/"))
  sys_command <- paste0(ifelse(Sys.info()["sysname"] == "Darwin",
                               "/Applications/LibreOffice.app/Contents/MacOS/soffice",
                               "libreoffice"),
                        " --headless --convert-to xlsx --outdir ", out_dir, " '", d$info$output_file, "'")
  system(sys_command)

  out_file <- paste0(out_dir, "/", basename(d$info$output_file))

  #Unpack this tool which has been "opened" in libreoffice
  d_opened <- unPackTool(submission_path = out_file, d2_session = training)
  expect_identical(d$info$datapack_name, d_opened$info$datapack_name)
  expect_setequal(names(d_opened), c("keychain", "info", "data", "tests", "datim", "sheets"))
  expect_true(NROW(d_opened$data$analytics) > 0)
  expect_true(all(d_opened$data$analytics$mechanism_desc == "default"))

})
})
