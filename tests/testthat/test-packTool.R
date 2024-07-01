

context("Create a COP22 Data Pack")

test_that("Can pack a datapack", {

    #  skip("Need to supply a valid model here")
      # For Generating Individual Data Packs ####
      generation_list <- c("Eswatini")
      cop_year <- 2024

      pick <- datapackr::COP21_datapacks_countries %>%
        dplyr::filter(datapack_name %in% generation_list) %>%
        dplyr::arrange(datapack_name)

      output_folder <- paste0("/tmp/", stringi::stri_rand_strings(1, 20))
      dir.create(output_folder)

      model_data <- list()
      model_data$V0qMZH29CtN <- tribble(
           ~indicator_code, ~period, ~psnu_uid, ~age_option_uid, ~sex_option_uid, ~kp_option_uid, ~value,
           "PMTCT_STAT.D.T_1", "2021Oct", "Yhf4p9zEkYl", "jcGQdcpPSJP", "Z1EnpTPaUfq", "NA", 11
         )

      # passing model object
      d <- packTool(tool = "Data Pack",
                    datapack_name = pick$datapack_name[1],
                    country_uids = unlist(pick$country_uids[1]),
                    template_path = NULL,
                    model_data_path = NULL,
                    model_data = model_data,
                    cop_year = cop_year,
                    output_folder = output_folder,
                    results_archive = FALSE,
                    expand_formulas = TRUE,
                    d2_session = training)

      #Write some more tests here to test metadata
      testthat::expect_identical(d$info$datapack_name, "Eswatini")

      # Be sure we can unpack what we just wrote to a file (currently cannot unpack)
      # see https://jira.pepfar.net/browse/DP-818
      testthat::expect_error(
        unPackTool(submission_path = d$info$output_file, d2_session = training),
        paste0("For type 'Undistributed MER', expected to see data from the main tabs of your Data Pack. ",
               "However, this appears to be missing."
        )
      )

      #expect_identical(d$info$datapack_name, d_out$info$datapack_name)
      #expect_setequal(names(d_out), c("keychain", "info", "data", "tests", "datim"))

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

})
