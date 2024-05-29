context("Create a COP23 PSNUxIM Tool")

with_mock_api({
  test_that(
    "We can write an COP23 PSNUxIM tool", {
      d <-
        unPackTool(
          submission_path = test_sheet("COP23_sample_DataPack_Malawi.xlsx"),
          tool = "Data Pack",
          country_uids = NULL,
          cop_year = 2023,
          d2_session = training)

      d <- unPackSheets(d, check_sheets = TRUE)


      output_folder <-
        paste0("/tmp/", stringi::stri_rand_strings(1, 20))
      dir.create(output_folder)


      d <- writePSNUxIM(d,
                        snuxim_model_data_path = test_sheet("COP23_SNUxIM_Model_Random.rds"),
                        output_folder = output_folder, d2_session = training)

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
        d_psnuxim <- unPackTool(submission_path = out_file, d2_session = training)
        expect_identical(d$info$datapack_name, d_psnuxim$info$datapack_name)
        expect_setequal(names(d_psnuxim), c("keychain", "info", "data", "tests", "datim", "sheets"))
        expect_setequal(names(d_psnuxim$datim$OPU), c("dataElement", "period",
                                                     "orgUnit", "categoryOptionCombo", "attributeOptionCombo", "value"))

        #Compare with the original data from the TST tool
        test_data_analytics <- d_psnuxim %>%
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
          dplyr::full_join(
            tibble::as_tibble(d$data$snuxim_model_data),
            by = c(
              "dataElement",
              "period",
              "orgUnit",
              "categoryOptionCombo",
              "attributeOptionCombo"
            )
          ) %>%
          dplyr::filter(attributeOptionCombo != "default") %>%  #Filter AGYW_PREV data
          dplyr::mutate(diff = dplyr::near(as.numeric(target_value), as.numeric(value), tol = 1.0)) %>%
          dplyr::filter(value != 0, target_value != 0) %>%
          dplyr::filter(!diff | is.na(diff))

        expect_true(NROW(test_data_analytics) == 0, "PSNUxIM data does not match the model data")

        unlink(output_folder, recursive = TRUE)

  })
})
