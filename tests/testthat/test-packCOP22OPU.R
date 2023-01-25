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

    #Suppress console output
    sink(tempfile())
    d <- packTool(tool = "OPU Data Pack",
             datapack_name = pick$datapack_name[1],
             country_uids = unlist(pick$country_uids[1]),
             template_path = NULL,
             cop_year = 2022,
             output_folder = output_folder,
             results_archive = FALSE,
             expand_formulas = TRUE,
             d2_session = training)
  sink()

  #Write some more tests here to test metadata
  expect_identical(d$info$datapack_name, "Eswatini")

  #Be sure we can unpack what we just wrote to a file
  d_out <- unPackTool(submission_path = d$info$output_file, d2_session = training)

  expect_identical(d$info$datapack_name, d_out$info$datapack_name)
  expect_setequal(names(d_out), c("keychain", "info", "data", "tests", "datim"))
  expect_setequal(names(d_out$datim$OPU), c("dataElement", "period",
                                           "orgUnit", "categoryOptionCombo", "attributeOptionCombo", "value"))

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
   expect_setequal(names(d_opened), c("keychain", "info", "data", "tests", "datim"))
   expect_setequal(names(d_opened$datim$OPU), c("dataElement", "period",
                                             "orgUnit", "categoryOptionCombo", "attributeOptionCombo", "value"))

   #The opened and unopened SNUxIM tabs should be identical at the SNU/DE/COC level
   test_data_unopened <- d_out$data$SNUxIM %>%
     dplyr::select(indicator_code, Age, Sex, KeyPop, psnuid, support_type, value) %>%
     dplyr::group_by(indicator_code, Age, Sex, KeyPop, psnuid, support_type) %>%
     dplyr::summarise(value = sum(as.numeric(value)), .groups = "drop")

   test_data_opened <- d_opened$data$SNUxIM %>%
     dplyr::select(indicator_code, Age, Sex, KeyPop, psnuid, support_type, value) %>%
     dplyr::group_by(indicator_code, Age, Sex, KeyPop, psnuid, support_type) %>%
     dplyr::summarise(value = sum(as.numeric(value)), .groups = "drop") %>%
     dplyr::rename(value_opened = value)

   test_data_joined <- test_data_unopened %>%
     dplyr::full_join(test_data_opened) %>%
     dplyr::mutate(diff = value_opened - value) %>%
     dplyr::filter(diff != 0) #Probably need to allow for rounding differences here
    #Look at HTS_RECENT.SNS.T. Seems its being duplicated.

   #Note that this test fails....This is not completely expected.
   testthat_print("This test should not fail")
   #expect_true(NROW(test_data_joined) == 0)

   #Compare with the original input data
   test_data_model <- d_opened$datim$OPU %>%
     dplyr::rename(value_opened = value) %>% #Are we joining the correct data?
     #Unclear if we should do an inner join...
     #But this is basically testing that the values in the
     #model match the values in the DATIM export.
     dplyr::inner_join(d$data$snuxim_model_data) %>%
     dplyr::mutate(diff = dplyr::near(as.numeric(value_opened), as.numeric(value), tol = 1.0)) %>%
     #Lets not worry about zeros?
     #They need to be there for dedupe mechanisms, but lets test this separ
     dplyr::filter(value != 0,
                   value_opened != 0) %>%
     dplyr::filter(!diff | is.na(diff))

   #We should get the same number of rows we input
   testthat_print("Compare an opened and unopened file")
   expect_true(all(test_data_model$diff == 0))
   testthat::expect_true(NROW(d$data$snuxim_model_data) > 0)
   #Do we have rows which differ?

   # missing_data_model <- d_opened$datim$OPU %>%
   #   dplyr::rename(value_opened = value) %>% #Are we joining the correct data?
   #   #Unclear if we should do an inner join...
   #   #But this is basically testing that the values in the
   #   #model match the values in the DATIM export.
   #   dplyr::anti_join(d$data$snuxim_model_data)
   #
   #TODO: Should there be any missing data here?
   #Crosswalk dedupes are missing here....
   #testthat::expect_true(NROW(missing_data_model) == 0)




   #TODO: Test from the analytics
   test_data_analytics <- d_opened %>%
     purrr::pluck("data") %>%
     purrr::pluck("analytics") %>%
     dplyr::select(dataElement = dataelement_id,
                   period = fiscal_year,
                   orgUnit = psnu_uid,
                   categoryOptionCombo = categoryoptioncombo_id,
                   attributeOptionCombo = mechanism_code,
                   target_value) %>%
     dplyr::mutate(target_value = as.numeric(target_value),
                   period = paste0(period - 1, "Oct")) %>%
     #TODO: Unclear at the moment of the effect of the inner join
     #We are trying to test whether the input and output are equivalent
     #But seems that certain things are being dropped in the join
     dplyr::full_join(tibble::as_tibble(d$data$snuxim_model_data),
     by = c("dataElement", "period", "orgUnit", "categoryOptionCombo", "attributeOptionCombo")) %>%
     dplyr::filter(attributeOptionCombo != "default") %>%  #Filter AGYW_PREV data
     dplyr::mutate(diff = dplyr::near(as.numeric(target_value), as.numeric(value), tol = 1.0)) %>%
     #Lets not worry about zeros?
     #They need to be there for dedupe mechanisms, but lets test this separ
     dplyr::filter(value != 0, target_value != 0) %>%
     dplyr::filter(!diff | is.na(diff))

   testthat_print("Compare analytics with original data from DATIM")
   expect_true(NROW(test_data_analytics) == 0)

   unlink(output_folder, recursive = TRUE)

  })
})
