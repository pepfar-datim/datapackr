context("Create a COP23 PSNUxIM Tool")

with_mock_api({
  test_that(
    "We can write an COP23 PSNUxIM tool",{
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
    d_opened <- unPackTool(submission_path = out_file, d2_session = training)

    expect_setequal(names(d_opened), c("keychain", "info", "data", "tests", "datim", "sheets"))



    #TODO: Add additional tests for data equality

  })
})