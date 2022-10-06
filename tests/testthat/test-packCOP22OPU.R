context("Create a COP22 OPU")

with_mock_api({
  test_that("We can write an COP22 OPU tool", {

    # For Generating Individual Data Packs ####
    generation_list <- c("Eswatini")

    pick <- datapackr::COP21_datapacks_countries %>%
      dplyr::filter(datapack_name %in% generation_list) %>%
      dplyr::arrange(datapack_name)

    output_folder <- paste0("/tmp/", stringi::stri_rand_strings(1, 20))
    dir.create(output_folder)

    d <- packTool(tool = "OPU Data Pack",
             datapack_name = pick$datapack_name[1],
             country_uids = unlist(pick$country_uids[1]),
             template_path = NULL,
             cop_year = 2022,
             output_folder = output_folder,
             results_archive = FALSE,
             d2_session = training)

  #Write some more tests here to test metadata
  testthat::expect_identical(d$info$datapack_name, "Eswatini")

  #Be sure we can unpack what we just wrote to a file
  d_out <- unPackTool(submission_path = d$info$output_file, d2_session = training)

  expect_identical(d$info$datapack_name, d_out$info$datapack_name)
  expect_setequal(names(d_out), c("keychain", "info", "data", "tests", "datim"))
  expect_setequal(names(d_out$datim$OPU), c("dataElement", "period",
                                           "orgUnit", "categoryOptionCombo", "attributeOptionCombo", "value"))

  #Open the generated tool in libreoffice to kick off the formulas
  #Do not even try and do this on Windows
  skip_if(Sys.info()["sysname"] == "Windows")
  lo_path <- system("which libreoffice", intern = TRUE)
  #Skip this if we cannot execute libreoffice
  skip_if(file.access(lo_path, 1) != 0)

  out_dir <- paste0(output_folder, "/out")
  dir.create(out_dir)

  Sys.setenv(LD_LIBRARY_PATH = "/usr/lib/libreoffice/program/")
  sys_command <- paste0("libreoffice --headless --convert-to xlsx --outdir ", out_dir, " '", d$info$output_file, "'")
  system(sys_command)

  out_file <- paste0(out_dir, "/", basename(d$info$output_file))

   #Unpack this tool which has been "opened" in libreoffice
   d_out <- unPackTool(submission_path = out_file, d2_session = training)
   expect_identical(d$info$datapack_name, d_out$info$datapack_name)
   expect_setequal(names(d_out), c("keychain", "info", "data", "tests", "datim"))
   expect_setequal(names(d_out$datim$OPU), c("dataElement", "period",
                                             "orgUnit", "categoryOptionCombo", "attributeOptionCombo", "value"))

   unlink(output_folder, recursive = TRUE)

  })
})
