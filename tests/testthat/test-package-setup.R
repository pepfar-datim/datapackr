context("Package setup")

test_that("We can pick a schema", {

  test_schema<-pick_schema(2020, "OPU Data Pack")
  testthat::expect_identical(test_schema, datapackr::cop20OPU_data_pack_schema)

  test_schema<-pick_schema(2021, "OPU Data Pack")
  testthat::expect_identical(test_schema, datapackr::cop21OPU_data_pack_schema)


  expect_error(pick_schema(1999, "OPU Data Pack"))

  test_schema<-pick_schema(2020, "Data Pack")
  testthat::expect_identical(test_schema, datapackr::cop20_data_pack_schema)
  test_schema<-pick_schema(2021, "Data Pack")
  testthat::expect_identical(test_schema, datapackr::cop21_data_pack_schema)

  #Are we sure we want to return this object for anything other than 2020
  # and 2021?
  test_schema<-pick_schema(1999, "Data Pack")
  testthat::expect_identical(test_schema, datapackr::data_pack_schema)


  #Throw an error for garbage inputs
  expect_error(pick_schema(1999, "Foo Pack"))
  expect_error(pick_schema(NA, NA))

  }
)

test_that("We can pick template file", {

  test_template<-pick_template_path(2020, "OPU Data Pack")
  expect_true(grepl("COP20_OPU_Data_Pack_Template.xlsx",
                    test_template))
  expect_true(file.exists(test_template))
  test_template<-pick_template_path(2021, "OPU Data Pack")
  expect_true(grepl("COP21_OPU_Data_Pack_Template.xlsx",
                    test_template))
  expect_true(file.exists(test_template))

  test_template<-pick_template_path(2021, "Data Pack")
  expect_true(grepl("COP21_Data_Pack_Template.xlsx",
                    test_template))
  expect_true(file.exists(test_template))

  test_template<-pick_template_path(2020, "Data Pack")
  expect_true(grepl("COP20_Data_Pack_Template_vFINAL.xlsx",
                    test_template))
  expect_true(file.exists(test_template))


  #Throw an error for garbage inputs
  expect_error(pick_template_path(1999, "Foo Pack"))
  expect_error(pick_template_path(NA, NA))

}
)


test_that("We can check datapack paramaters", {
  test_params<-check_params()
  expect_type(test_params, "list")
  #Is the intent that we should return a zero-length list with no arguments?
  expect_equal(length(test_params), 0L)

  #Test for a valid country UID
  test_params<-check_params(country_uids = "JTypsdEUNPw")
  expect_type(test_params, "list")
  expect_equal(test_params$country_uids, "JTypsdEUNPw")

  #Test for an invalid country UID
  test_params<-check_params(country_uids = "foo")
  expect_type(test_params, "list")
  expect_equal(length(check_params(country_uids = "foo")$country_uids), 0L)

  #Throw an error if the argument is NULL
  expect_error(check_params(country_uids = NULL))

  #Test for a valid COP year
  test_params<-check_params(cop_year = 2020)
  expect_type(test_params, "list")
  expect_equal(test_params$cop_year, 2020)

  #When supplied NULL, return the current COP year
  #Are we sure about this?
  test_params<-check_params(cop_year = NULL)
  expect_type(test_params, "list")
  expect_equal(test_params$cop_year, datapackr::getCurrentCOPYear())

  #Error on a bogus COP year
  expect_error(check_params(cop_year = 1999))

  #Can check a valid tool
  test_params<-check_params(tool = "Data Pack")
  expect_type(test_params, "list")
  expect_equal(test_params$tool, "Data Pack")

  #Can error on a bogus tool
  expect_error(check_params(tool = "Foo Pack"))

  #Can check a valid season
  test_params<-check_params(season = "COP")
  expect_type(test_params, "list")
  expect_equal(test_params$season, "COP")

  #Can error on a bogus tool
  expect_error(check_params(season = "The Long Winter"))

  #Return the season automatically if a DataPack and season is
  #explicit set to NULL
  test_params<-check_params(tool="Data Pack", season = NULL)
  expect_type(test_params, "list")
  expect_named(test_params, c("tool", "season"))
  expect_equal(test_params$season, "COP")
  expect_equal(test_params$tool, "Data Pack")

  #Return the season automatically if an OPU DataPack and season is
  #explicit set to NULL
  test_params<-check_params(tool="OPU Data Pack", season = NULL)
  expect_type(test_params, "list")
  expect_named(test_params, c("tool", "season"))
  expect_equal(test_params$season, "OPU")
  expect_equal(test_params$tool, "OPU Data Pack")


  #Can check a valid schema
  test_params <-
    check_params(
      schema = datapackr::cop20_data_pack_schema,
      cop_year = 2020,
      season = "COP"
    )
  expect_type(test_params, "list")
  expect_setequal(names(test_params), c("schema", "cop_year", "season"))

  # #Return a message when using an invalid combination of schema/cop_year/season
  #
  # expect_message(
  #   test_params <-
  #     check_params(
  #       schema = datapackr::cop20_data_pack_schema,
  #       cop_year = 2021,
  #       season = "COP"
  #     )
  # )
  #This will return a handled error, but will NOT return "Global"
  expect_error(test_params <-
    check_params(datapack_name = NULL, country_uids = NULL
    ), "Must supply country_uids.")

  test_args<-list(datapack_name = "Zambia", country_uids = "f5RoebaDLMx")
  test_params<-do.call(check_params, test_args)
  expect_true(identical(sort(unlist(test_params)), sort(unlist(test_args))))

  #Expect an error here but there is not one?
  # test_args<-list(datapack_name = "Demoland", country_uids = "f5RoebaDLMx")
  # expect_error(do.call(check_params, test_args))

  #Expect an error here but there is not one?
  #Only get a command line message
  # test_args<-list(datapack_name = "Zambia", country_uids = "abc12345678")
  # expect_error(do.call(check_params, test_args))
  }
)
