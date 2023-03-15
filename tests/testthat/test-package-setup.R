context("Package setup")

test_that("We can pick a schema", {

  test_schema <-  pick_schema(2022, "OPU Data Pack")
  testthat::expect_identical(test_schema, cop22OPU_data_pack_schema)



  test_schema <-  pick_schema(2021, "Data Pack")
  testthat::expect_identical(test_schema, cop21_data_pack_schema)
  test_schema <-  pick_schema(2022, "Data Pack")
  testthat::expect_identical(test_schema, cop22_data_pack_schema)
  test_schema <-  pick_schema(2023, "Data Pack")
  testthat::expect_identical(test_schema, cop23_data_pack_schema)


  expect_error(pick_schema(1999, "PSNUxIM"))
  test_schema <-  pick_schema(2023, "PSNUxIM")
  testthat::expect_identical(test_schema, cop23_psnuxim_schema)



  #Throw an error for garbage inputs
  expect_error(pick_schema(1999, "Foo Pack"))
  expect_error(pick_schema(NA, NA))

  }
)

test_that("We can pick template file", {
  test_template <-  pick_template_path(2021, "OPU Data Pack")
  expect_true(grepl("COP21_OPU_Data_Pack_Template.xlsx",
                    test_template))
  expect_true(file.exists(test_template))

  test_template <-  pick_template_path(2021, "Data Pack")
  expect_true(grepl("COP21_Data_Pack_Template.xlsx",
                    test_template))
  expect_true(file.exists(test_template))

  test_template <-  pick_template_path(2022, "Data Pack")
  expect_true(grepl("COP22_Data_Pack_Template.xlsx",
                    test_template))
  expect_true(file.exists(test_template))

  test_template <-  pick_template_path(2023, "Data Pack")
  expect_true(grepl("COP23_Data_Pack_Template.xlsx",
                    test_template))
  expect_true(file.exists(test_template))

  test_template <-  pick_template_path(2022, "OPU Data Pack")
  expect_true(grepl("COP22_OPU_Data_Pack_Template.xlsx",
                    test_template))
  expect_true(file.exists(test_template))


  test_template <-  pick_template_path(2023, "PSNUxIM")
  expect_true(grepl("COP23_PSNUxIM_Template.xlsx",
                    test_template))
  expect_true(file.exists(test_template))

  expect_error(pick_template_path(2023, "OPU Data Pack"))


  #Throw an error for garbage inputs
  expect_error(pick_template_path(1999, "Foo Pack"))
  expect_error(pick_template_path(NA, NA))

}
)


test_that("We can check datapack paramaters", {

  options(rlang_interactive = TRUE)

  # check_params ####
  # Test that when not supplied any parameters, check_params returns list of length 0.
  test_params <- check_params()
  expect_type(test_params, "list")
  expect_equal(length(test_params), 0L)

  # country_uids ####
  #Test for a valid country UID
  test_params <- check_params(country_uids = "JTypsdEUNPw")
  expect_type(test_params, "list")
  expect_equal(test_params$country_uids, "JTypsdEUNPw")

  #Throw an error if supplied only invalid country UIDs
  expect_error(check_params(country_uids = "foo", force = TRUE))

  #Test for mix of valid and invalid country_uids
  mix <- check_params(country_uids = c("JTypsdEUNPw", "foo"))
  single_valid <- check_params(country_uids = "JTypsdEUNPw")
  expect_equal(mix, single_valid)

  # Throw an error if the argument is NULL or invalid, and force = TRUE
  expect_error(check_params(country_uids = NULL, force = TRUE))
  expect_error(check_params(country_uids = "foo", force = TRUE))

  # If country_uids is NULL or invalid and force = FALSE, expect all countries returned
  all_countries <- sort(unique(valid_PSNUs$country_uid))
  expect_equal(
    sort(check_params(country_uids = NULL, force = FALSE)$country_uids),
    all_countries)
  expect_equal(
    sort(check_params(country_uids = "foo", force = FALSE)$country_uids),
    all_countries)


  # TODO: Add test for check_PSNUs


  # cop_year ####
  # Test for a valid COP year
  test_params <- check_params(cop_year = 2022)
  expect_type(test_params, "list")
  expect_equal(test_params$cop_year, 2022)

  # Test for valid COP year supplied as character
  test_params <- check_params(cop_year = "2022")
  expect_type(test_params, "list")
  expect_equal(test_params$cop_year, 2022)

  # Test for valid COP Year supplied in substring
  test_params <- check_params(cop_year = "COP2022 I think")
  expect_type(test_params, "list")
  expect_equal(test_params$cop_year, 2022)

  #When supplied NULL, return the current COP year
  test_params <- check_params(cop_year = NULL)
  expect_type(test_params, "list")
  expect_equal(test_params$cop_year, datapackr::getCurrentCOPYear())

  # When supplied missing argument at individual function level, return the current COP year
  test_param <- check_cop_year()
  expect_type(test_param, "double")
  expect_equal(test_param, datapackr::getCurrentCOPYear())

  # Error on a bogus COP year
  expect_error(check_params(cop_year = 1999))


  # tool ####
  # Can check a valid tool
  test_params <- check_params(tool = "Data Pack")
  expect_type(test_params, "list")
  expect_equal(test_params$tool, "Data Pack")

  # NULL or missing returns "Data Pack" default
  test_params <- check_params(tool = NULL)
  expect_type(test_params, "list")
  expect_equal(test_params$tool, "Data Pack")

  test_param <- check_tool()
  expect_type(test_param, "character")
  expect_equal(test_param, "Data Pack")

  # Can error on a bogus tool
  expect_error(check_params(tool = "Foo Pack"))


  # season ####
  # Can check a valid season
  test_params <- check_params(season = "COP")
  expect_type(test_params, "list")
  expect_equal(test_params$season, "COP")

  # Can error on a bogus season
  expect_error(check_params(season = "The Long Winter"))

  # Return the season automatically if a DataPack and season is
  # explicit set to NULL
  test_params <- check_params(tool = "Data Pack", season = NULL)
  expect_type(test_params, "list")
  expect_named(test_params, c("tool", "season"))
  expect_equal(test_params$season, "COP")
  expect_equal(test_params$tool, "Data Pack")
  expect_message(test_params <- check_params(tool = "Data Pack", season = NULL))

  # Return the season automatically if an OPU DataPack and season is
  # explicit set to NULL
  test_params <- check_params(tool = "PSNUxIM", season = NULL)
  expect_type(test_params, "list")
  expect_named(test_params, c("tool", "season"))
  expect_equal(test_params$season, "COP")
  expect_equal(test_params$tool, "PSNUxIM")
  expect_message(check_params(tool = "PSNUxIM", season = NULL))

  # Deduce the season if not provided
  test_params <- check_params(season = NULL)
  expect_type(test_params, "list")
  expect_named(test_params, c("season"))
  expect_equal(test_params$season, "COP")
  expect_message(check_params(season = NULL))

  # If season and tool both provided, but don't match, issue warning, but leave
  # both in place.
  test_params <- check_params(tool = "PSNUxIM", season = "COP")
  expect_type(test_params, "list")
  expect_named(test_params, c("tool", "season"))
  expect_equal(test_params$season, "COP")
  expect_equal(test_params$tool, "PSNUxIM")
  expect_message(check_params(tool = "PSNUxIM", season = "COP"))


  # schema ####
  # Can check a valid schema
  test_params  <-
    check_params(
      schema = datapackr::cop22_data_pack_schema,
      cop_year = 2022,
      season = "COP"
    )
  expect_type(test_params, "list")
  expect_setequal(names(test_params), c("schema", "cop_year", "season"))
  expect_identical(test_params$schema, datapackr::cop22_data_pack_schema)

  # Return a message when using an invalid combination of schema/cop_year/season
  expect_message(
    test_params  <-
      check_params(
        schema = datapackr::cop21_data_pack_schema,
        cop_year = 2022,
        season = "COP"
      )
  )

  # datapack_name ####
  # Test valid combination
  test_args <- list(datapack_name = "Zambia", country_uids = "f5RoebaDLMx", cop_year = 2022)
  test_params <- do.call(check_params, test_args)
  expect_true(identical(sort(unlist(test_params)), sort(unlist(test_args))))

  # This will return a handled error, but will NOT return "Global"
  expect_error(test_params <-
                 check_params(datapack_name = NULL, country_uids = NULL, cop_year = 2022,
                 ), "Must supply valid country_uids.")




  # Expect a message if datapack_name and country_uids do not match (but allow
  # custom names)
  test_args <- list(datapack_name = "Demoland", country_uids = "f5RoebaDLMx")
  expect_message(do.call(check_params, test_args))

  # Expect an error here
  test_args <- list(datapack_name = "Zambia", country_uids = "abc12345678")
  expect_error(do.call(check_params, test_args))


  # Template Path ####
  # Test valid combination
  template_path <- pick_template_path(cop_year = 2022, tool = "Data Pack")
  test_args <- list(template_path = template_path, cop_year = 2022, tool = "Data Pack")
  test_params <- do.call(check_params, test_args)
  expect_setequal(names(test_params), c("cop_year", "tool", "template_path"))
  expect_true(identical(sort(unlist(test_params)), sort(unlist(test_args))))

  # Test deduction power
  test_args <- list(template_path = NULL, cop_year = 2023, tool = "PSNUxIM")
  test_params <- do.call(check_params, test_args)
  expect_setequal(names(test_params), c("cop_year", "tool", "template_path"))
  expected_path <- pick_template_path(cop_year = 2023, tool = "PSNUxIM")
  expect_identical(test_params$template_path, expected_path)

  # Test invalid combination
  template_path <- pick_template_path(cop_year = 2023, tool = "Data Pack")
  test_args <- list(template_path = template_path, cop_year = 2023, tool = "PSNUxIM")
  expect_message(do.call(check_params, test_args))

  # Sheets ----
  tool <- "Data Pack"
  cop_year <- 2022

  expect_silent(sheets <- checkSheets(sheets = c("HTS", "Cascade", "GEND"),
                                      tool = tool, cop_year = cop_year,
                                      all_sheets = FALSE,
                                      operation = "unpack"))

  expect_silent(sheets <- checkSheets(sheets = c("HTS", "Cascade", "GEND"),
                                      tool = tool, cop_year = cop_year,
                                      all_sheets = FALSE,
                                      operation = "pack"))

  expect_silent(sheets <- checkSheets(sheets = c("HTS", "Cascade", "GEND"),
                                      tool = tool, cop_year = cop_year,
                                      all_sheets = FALSE,
                                      operation = "schema"))

  expect_silent(sheets <- checkSheets(sheets = c("HTS", "Cascade", "GEND", "Home"),
                                      tool = tool, cop_year = cop_year,
                                      all_sheets = TRUE))

  expect_silent(
    sheets <- checkSheets(sheets = c("HTS", "Cascade", "GEND", "Home", "PSNUxIM"),
                          tool = tool, cop_year = cop_year,
                          all_sheets = TRUE,
                          psnuxim = TRUE))

  expect_warning(sheets <- checkSheets(sheets = c("HTS", "Cascade", "GEND", "Home"),
                                       tool = tool, cop_year = cop_year,
                                       all_sheets = FALSE))

  expect_warning(sheets <- checkSheets(sheets = c("HTS", "Cascade", "GEND", "foo"),
                                       tool = tool, cop_year = cop_year,
                                       all_sheets = FALSE))

  expect_warning(sheets <- checkSheets(sheets = c("HTS", "Cascade", "GEND", "PSNUxIM"),
                                       tool = tool, cop_year = cop_year,
                                       all_sheets = FALSE,
                                       psnuxim = FALSE))

  #2023 Sheets nuances
  expect_warning(sheets <- checkSheets(sheets = c("HTS", "KP Validation"),
                                       tool = tool, cop_year = 2023,
                                       all_sheets = FALSE,
                                       operation = "unpack"))

  expect_warning(sheets <- checkSheets(sheets = c("HTS", "Year 2"),
                                       tool = tool, cop_year = 2023,
                                       all_sheets = FALSE,
                                       operation = "pack"))

  expect_silent(sheets <- checkSheets(sheets = c("HTS", "Year 2"),
                                      tool = tool, cop_year = 2023,
                                      all_sheets = FALSE,
                                      operation = "unpack"))

  expect_silent(sheets <- checkSheets(sheets = c("HTS", "KP Validation"),
                                      tool = tool, cop_year = 2023,
                                      all_sheets = FALSE,
                                      operation = "pack"))

  expect_silent(sheets <- checkSheets(sheets = c("HTS", "KP Validation", "Year 2"),
                                      tool = tool, cop_year = 2023,
                                      all_sheets = FALSE,
                                      operation = "schema"))

  }
)
