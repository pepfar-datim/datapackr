context("test-checkColStructure")

test_that("Can check missing column...", {

  # create minimal schema data
  d <- list()
  d$info$messages <- MessageQueue()
  d$info$cop_year <- 2024
  d$info$tool <- "Data Pack"
  d$info$has_psnuxim <- TRUE

  d$info$schema <-
    tribble(
      ~indicator_code, ~col, ~sheet_name,
                 "PSNU",              1, "PSNUxIM",
       "indicator_code",              2, "PSNUxIM",
                  "Age",              3, "PSNUxIM",
                  "Sex",              4, "PSNUxIM",
               "KeyPop",              5, "PSNUxIM",
                   "ID",              6, "PSNUxIM"
    )

  d$data$SNUxIM <-
    tribble(
      ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop,
      NA, NA, NA, NA, NA
    )

  sheet <- "PSNUxIM"
  header_row <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)
  d <- checkColStructure(d, sheet = sheet)

  # expect missing column ID message
  testthat::expect_equal(
    d$info$messages$message,
    paste0("ERROR! In tab PSNUxIM, MISSING COLUMNS:",
           " Please ensure no columns have been deleted or renamed",
           " from the original Data Pack you have received. ->  \n\t* ID\n")
    )

  # expect level error
  testthat::expect_equal(d$info$messages$level, "ERROR")

})


test_that("Can check duplicate columns and out of order...", {

  # create minimal schema data
  d <- list()
  d$info$messages <- MessageQueue()
  d$info$cop_year <- 2024
  d$info$tool <- "Data Pack"
  d$info$has_psnuxim <- TRUE

  d$info$schema <-
    tribble(
      ~indicator_code, ~col, ~sheet_name,
      "PSNU",              1, "PSNUxIM",
      "indicator_code",              2, "PSNUxIM",
      "Age",              3, "PSNUxIM",
      "Sex",              4, "PSNUxIM",
      "KeyPop",              5, "PSNUxIM",
      "ID",              6, "PSNUxIM"
    )

  d$data$SNUxIM <-
    tribble(
      ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop, ~ID, ~KeyPop,
      NA, NA, NA, NA, NA, NA, NA
    )

  sheet <- "PSNUxIM"
  header_row <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)
  d <- checkColStructure(d, sheet = sheet)

  # expect error on duplicate columns
  testthat::expect_equal(
    d$info$messages$message[1],
    paste0("ERROR! In tab PSNUxIM, DUPLICATE COLUMNS: ",
           "The following required columns appear twice.",
           " This must be resolved in your submission in order for processing to continue.",
           " Please review those columns flagged by this test to determine whether they may",
           " have been inadvertently duplicated. ->  \n\t* KeyPop\n")
    )


  testthat::expect_equal(d$info$messages$level[1], "ERROR")

  # expect warning on out of order columns
  testthat::expect_equal(
    d$info$messages$message[2],
    paste0("WARNING! In tab PSNUxIM, OUT OF ORDER COLUMNS:",
           " While it is permitted to rearrange columns within your Data Pack as needed,",
           " this is not encouraged as it may introduce unintended formula errors.",
           " Please review these columns to ensure their rearrangement",
           " has not caused any issues. ->  \n\t* KeyPop\n")
    )
  testthat::expect_equal(d$info$messages$level[2], "WARNING")
})
