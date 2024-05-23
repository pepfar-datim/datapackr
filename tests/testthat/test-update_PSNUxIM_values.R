
context("Update PSNUxIM Target values")

test_that(
  "Can update PSNUxIM Target values",  {
    d <- list()
    d$info$tool <- "Data Pack"
    d$info$cop_year <- 2023
    d$keychain$psnuxim_file_path <-  getTemplate("COP23_PSNUxIM_Template.xlsx")


    d$sheets$PSNUxIM <- tibble::tribble(

      ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~`DataPackTarget`,
      "Erongo [DZGJZHBfYq5]", "HTS_TST.PostANC1.Neg.T", "10-14", "Female", NA, 1
    )

    d$tests$non_equal_targets <- d$sheets$PSNUxIM %>%
      dplyr::mutate(MainTabsTarget = 2L, are_equal = FALSE)

    d <- updatePSNUxIMTargetValues(d)

    expect_true(!is.null(d$tool$wb))

    out_file1 <- paste0(tempfile(), ".xlsx")

    openxlsx::saveWorkbook(d$tool$wb, file = out_file1)
    header_row <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)

    p <- openxlsx::read.xlsx(out_file1,
                             "PSNUxIM",
                             startRow = header_row)

    expect_equal(p$DataPackTarget[1], 2L)


  })
