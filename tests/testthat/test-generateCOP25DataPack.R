context("Create a COP25 Target Setting Tool")

with_mock_api({
  test_that("We can write an COP25 Target Setting tool", {

    template_path <- getTemplate("COP25_Data_Pack_Template.xlsx")

    expect_true(file.exists(template_path))

    # For Generating Individual Data Packs ####
    generation_list <- c("Malawi")

    pick <- datapackr::COP21_datapacks_countries %>%
      dplyr::filter(datapack_name %in% generation_list) %>%
      dplyr::arrange(datapack_name)

    output_folder <- paste0("/tmp/", stringi::stri_rand_strings(1, 20))
    dir.create(output_folder)

    #Suppress console output
# Tue Jul 30 13:11:27 2024 - Need to update when time arrives.
    spectrum_data <- readRDS(test_sheet("COP24_spectrum_data_random_MW.rds"))

    d <- packTool(model_data_path = test_sheet("COP24_datapack_model_data_random_MW.rds"),
                  tool = "Data Pack",
                  datapack_name = pick$datapack_name[1],
                  country_uids = unlist(pick$country_uids[1]),
                  template_path = template_path,
                  cop_year = 2025,
                  output_folder = output_folder,
                  results_archive = FALSE,
                  expand_formulas = TRUE,
                  spectrum_data = spectrum_data,
                  d2_session = training)

    expect_setequal(names(d), c("keychain", "info", "tool", "data"))
    expect_equal(d$info$datapack_name, "Malawi")

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


    d_data_targets_names <- c("PSNU", "psnuid", "sheet_name", "indicator_code", "Age", "Sex", "KeyPop", "value")
    d_data_tests_types <- c("tbl_df", "tbl", "data.frame")


    d <- unPackSheets(d_opened, check_sheets = TRUE)


    expect_true(!is.null(d_opened$data$MER))
    expect_setequal(class(d_opened$data$MER), c("tbl_df", "tbl", "data.frame"))
    expect_identical(unname(sapply(d_opened$data$MER, typeof)), c(rep("character", 7), "double"))
    expect_setequal(names(d_opened$data$MER), d_data_targets_names)
    expect_true((NROW(d_opened$data$MER) > 0))

    expect_true(!is.null(d_opened$data$SUBNAT_IMPATT))
    expect_setequal(class(d_opened$data$SUBNAT_IMPATT), c("tbl_df", "tbl", "data.frame"))
    expect_identical(unname(sapply(d_opened$data$SUBNAT_IMPATT, typeof)), c(rep("character", 7), "double"))
    expect_setequal(names(d_opened$data$SUBNAT_IMPATT), d_data_targets_names)
    expect_true((NROW(d_opened$data$SUBNAT_IMPATT) > 0))


    validation_summary <- validationSummary(d_opened)
    expect_named(validation_summary,
                 c("count", "country_name", "country_uid",
                   "ou", "ou_id", "test_name", "validation_issue_category"),
                 ignore.order = TRUE)


    #DP-837
    #Specific test of AGYW_PREV orgunits
    agyw_have <- d$sheets$AGYW %>%
      dplyr::select(PSNU) %>%
      dplyr::distinct() %>%
      dplyr::mutate(psnu_uid = stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)")) %>%
      dplyr::arrange(PSNU)

    agyw_want <- getValidOrgUnits("2025") %>%
      dplyr::filter(country_uid %in% d$info$country_uids) %>%
      add_dp_label(., "2025") %>%
      dplyr::arrange(dp_label) %>%
      dplyr::filter(!is.na(DREAMS)) %>%
      dplyr::select(PSNU = dp_label, psnu_uid = uid) %>%
      dplyr::arrange(PSNU)

    expect_identical(agyw_want, agyw_have)


    #Check the PSNUs in normal sheets, excluding the PSNUxIM tab, Year 2 and AGYW
    discard_names <- function(l, kn) {
      l[!(names(l) %in% kn)]
    }

    extract_PSNU <- function(df) {
      df %>%
        dplyr::select(PSNU) %>%
        dplyr::distinct() %>%
        dplyr::mutate(psnu_uid =
                        stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)")) %>%
        dplyr::arrange(PSNU)
    }

    sheet_psnus <- d$sheets %>%
      discard_names(c("PSNUxIM", "Year 2", "AGYW")) %>%
      purrr::map(extract_PSNU)

    wanted_psnus <-
      getValidOrgUnits("2025") %>%
      dplyr::filter(country_uid %in% d$info$country_uids) %>%
      add_dp_label(., "2025") %>%
      dplyr::arrange(dp_label) %>%
      ## Remove DSNUs
      dplyr::filter(org_type != "DSNU") %>%
      dplyr::select(PSNU = dp_label, psnu_uid = uid)

    expect_true(all(unlist(purrr::map(sheet_psnus, identical, wanted_psnus))))

    #DP-970--Duplicates in the Year2 tab
    duplicated_export_rows <- d$datim$year2 %>%
      dplyr::select(dataElement, period, orgUnit, categoryOptionCombo, attributeOptionCombo) %>%
      dplyr::group_by_all() %>%
      dplyr::mutate(n = dplyr::n()) %>%
      dplyr::filter(n > 1) %>%
      NROW()

    expect_equal(duplicated_export_rows, 0L)


    #There should be no zeros in d$data$SNUxIM except for dedpe
    expect_false(any(d$data$SNUxIM[d$data$SNUxIM$value == 0 & !grepl("^0000[01]", d$data$SNUxIM$mech_code), ]))

  })
})
