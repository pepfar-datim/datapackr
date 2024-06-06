
test_that("We can get sheets to skip", {

  tool <- "Data Pack"
  cop_year <- 2024
  test_schema <- pick_schema(cop_year, tool)
  this_skip <- getSkipSheets(test_schema, tool, cop_year)
  expect_named(this_skip, c("package_skip", "num", "names")) })


flubSkippedSheets <- function(schema) {
  schema %>% dplyr::filter(sheet_name != "Spectrum")
}

test_that("We can flag missing sheets which are skipped", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)
  #Simulate deleting the Spectrum tab
  bad_schema <- flubSkippedSheets(ref_schema)
  test_results <- checkSchema_SkippedSheets(bad_schema, tool, cop_year)
  expect_true(length(test_results) > 0)
  expect_named(test_results, c("error", "data"))
})

test_that("We can pass when all skip sheets are present in the schema", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  test_results <- checkSchema_SkippedSheets(ref_schema, tool, cop_year)
  expect_true(length(test_results) == 0)
  expect_null(names(test_results))
})


test_that("We can pass when schema sheets are ordered sequentially", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  test_results <- checkSchema_SheetNums(ref_schema)
  expect_true(length(test_results) == 0)
  expect_null(names(test_results))
})

test_that("We can flag when sheets are not ordered sequentially", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)
  bad_schema <- ref_schema %>%
    dplyr::filter(sheet_num != 2)

  test_results <- checkSchema_SheetNums(bad_schema)

  expect_true(length(test_results) > 0)
  expect_named(test_results, c("error", "data"), ignore.order = TRUE)
})


test_that("We can pass when schema names match the package", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  test_results <- checkSchema_SheetNames(ref_schema, ref_schema)
  expect_true(length(test_results) == 0)
  expect_null(names(test_results))
})

test_that("We can flag when sheet names do not match the reference schema", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)
  bad_schema <- ref_schema %>%
    dplyr::filter(sheet_num != 2)

  test_results <- checkSchema_SheetNames(ref_schema, bad_schema)

  expect_true(length(test_results) > 0)
  expect_named(test_results, c("error", "data"), ignore.order = TRUE)
})

test_that("We can pass when data sets are valid", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  test_results <- checkSchema_InvalidDatasets(ref_schema, tool, cop_year)
  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) == 0)
  expect_named(test_results,
               c("sheet_name", "data_structure", "col", "indicator_code", "dataset", "col_type"),
               ignore.order = TRUE)

})

test_that("We can flag when data sets are invalid", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)
  bad_schema <- ref_schema %>%
    dplyr::mutate(dataset = dplyr::case_when(col_type == "reference" ~ "foobar",
                                             TRUE ~ dataset))

  test_results <- checkSchema_InvalidDatasets(bad_schema, tool, cop_year)

  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) > 0L)
  expect_named(test_results,
               c("sheet_name", "data_structure", "col", "indicator_code", "dataset", "col_type"),
               ignore.order = TRUE)
 # Skipped sheets should have no data set
  this_skip <- getSkipSheets(ref_schema, tool, cop_year)
  bad_schema <- ref_schema %>%
    dplyr::mutate(dataset = dplyr::case_when(sheet_name %in% this_skip$names ~ "foobar",
                                   TRUE ~ dataset))

  test_results <- checkSchema_InvalidDatasets(bad_schema, tool, cop_year)

  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) > 0L)
  expect_named(test_results,
               c("sheet_name", "data_structure", "col", "indicator_code", "dataset", "col_type"),
               ignore.order = TRUE)
  })

test_that("We can pass valid column types", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  test_results <- checkSchema_InvalidColType(ref_schema, tool, cop_year)
  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) == 0)
  expect_named(test_results,
               c("sheet_name", "col", "indicator_code", "data_structure", "col_type"),
               ignore.order = TRUE)

})


flubColumnTypes <- function(schema) {
  schema %>%
    dplyr::mutate(col_type = dplyr::case_when(col_type == "reference" ~ "foobar",
                                              TRUE ~ col_type))
}


test_that("We can flag when column types are invalid", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)
  bad_schema <- flubColumnTypes(ref_schema)

  test_results <- checkSchema_InvalidColType(bad_schema, tool, cop_year)

  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) > 0L)
  expect_named(test_results,
               c("sheet_name", "col", "indicator_code", "data_structure", "col_type"),
               ignore.order = TRUE)

})

test_that("We can pass valid value types", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  test_results <- checkSchema_InvalidValueType(ref_schema, tool, cop_year)
  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) == 0)
  expect_named(test_results,
               c("sheet_name", "col", "indicator_code", "value_type"),
               ignore.order = TRUE)

})

test_that("We can flag invalid value types", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)
  bad_schema <- ref_schema %>%
    dplyr::mutate(value_type = dplyr::case_when(value_type == "integer" ~ "foobar",
                                               TRUE ~ value_type))

  test_results <- checkSchema_InvalidValueType(bad_schema, tool, cop_year)

  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) > 0L)
  expect_named(test_results,
               c("sheet_name", "col", "indicator_code", "value_type"),
               ignore.order = TRUE)

})

test_that("We can pass valid ages", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  test_results <- checkSchema_ValidAges(ref_schema)
  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) == 0)
  expect_named(test_results,
               c("sheet_name", "col", "indicator_code", "name", "id"),
               ignore.order = TRUE)

})

test_that("We can flag invalid ages", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  modify_age <- function(x) {
    if (is.null(x)) {
      return(NULL)
      }

    dplyr::mutate(x, name = dplyr::case_when(name == "15-19" ~ "abc123", TRUE ~ name))
  }

   bad_schema <- ref_schema %>%
    dplyr::mutate(valid_ages = purrr::map(valid_ages, modify_age))

  test_results <- checkSchema_ValidAges(bad_schema)

  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) > 0L)
  expect_named(test_results,
               c("sheet_name", "col", "indicator_code", "name", "id"),
               ignore.order = TRUE)

  modify_id <- function(x) {
    if (is.null(x)) {
    return(NULL)
    }

    dplyr::mutate(x, name = dplyr::case_when(grepl("^tt", id) ~ "abc123", TRUE ~ id))
  }

  bad_schema <- ref_schema %>%
    dplyr::mutate(valid_ages = purrr::map(valid_ages, modify_id))

  test_results <- checkSchema_ValidAges(bad_schema)

  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) > 0L)
  expect_named(test_results,
               c("sheet_name", "col", "indicator_code", "name", "id"),
               ignore.order = TRUE)


})



test_that("We can pass valid sex identifiers", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  test_results <- checkSchema_ValidSexes(ref_schema)
  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) == 0)
  expect_named(test_results,
               c("sheet_name", "col", "indicator_code", "name", "id"),
               ignore.order = TRUE)

})

test_that("We can flag invalid sexes", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  modify_males <- function(x) {
    if (is.null(x)) {
      return(NULL)
      }

    dplyr::mutate(x, name = dplyr::case_when(name == "Male" ~ "Malez", TRUE ~ name))
  }

  bad_schema <- ref_schema %>%
    dplyr::mutate(valid_sexes = purrr::map(valid_sexes, modify_males))

  test_results <- checkSchema_ValidSexes(bad_schema)

  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) > 0L)
  expect_named(test_results,
               c("sheet_name", "col", "indicator_code", "name", "id"),
               ignore.order = TRUE)

  modify_id <- function(x) {

    if (is.null(x)) {
    return(NULL)
    }

    dplyr::mutate(x, id = dplyr::case_when(name == "Male" ~ "abc123", TRUE ~ id))
  }

  bad_schema <- ref_schema %>%
    dplyr::mutate(valid_sexes = purrr::map(valid_sexes, modify_id))

  test_results <- checkSchema_ValidSexes(bad_schema)

  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) > 0L)
  expect_named(test_results,
               c("sheet_name", "col", "indicator_code", "name", "id"),
               ignore.order = TRUE)


})

test_that("We can pass valid KP identifiers", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  test_results <- checkSchema_ValidKPs(ref_schema)
  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) == 0)
  expect_named(test_results,
               c("sheet_name", "col", "indicator_code", "name", "id"),
               ignore.order = TRUE)

})

test_that("We can flag KP identifiers", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  modify_pwid <- function(x) {
    if (is.null(x)) {
      return(NULL)
      }

    dplyr::mutate(x, name = dplyr::case_when(name == "PWID" ~ "DIWP", TRUE ~ name))
  }

  bad_schema <- ref_schema %>%
    dplyr::mutate(valid_kps = purrr::map(valid_kps, modify_pwid))

  test_results <- checkSchema_ValidKPs(bad_schema)

  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) > 0L)
  expect_named(test_results,
               c("sheet_name", "col", "indicator_code", "name", "id"),
               ignore.order = TRUE)

  modify_id <- function(x) {
    if (is.null(x)) {
    return(NULL)
    }
    dplyr::mutate(x, id = dplyr::case_when(name == "PWID" ~ "abc123", TRUE ~ id))
  }

  bad_schema <- ref_schema %>%
    dplyr::mutate(valid_kps = purrr::map(valid_kps, modify_id))

  test_results <- checkSchema_ValidKPs(bad_schema)

  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) > 0L)
  expect_named(test_results,
               c("sheet_name", "col", "indicator_code", "name", "id"),
               ignore.order = TRUE)


})


test_that("We can pass valid formulas", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  test_results <- checkSchema_Formulas(ref_schema)
  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) == 0)
  expect_named(test_results,
               c("col", "formula", "indicator_code", "sheet_name"),
               ignore.order = TRUE)

})

test_that("We can flag invalid formulas´", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  bad_formulas <- sample(seq_len(NROW(ref_schema)), 10)
  ref_schema$formula[bad_formulas] <- "#REF"
  test_results <- checkSchema_Formulas(ref_schema)

  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) == 10)
  expect_named(test_results,
               c("col", "formula", "indicator_code", "sheet_name"),
               ignore.order = TRUE)


})


test_that("We can pass valid data element identifiers", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  test_results <- checkSchema_DataElementSyntax(ref_schema)

  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) == 0)
  expect_named(
    test_results,
    c(
      "col",
      "dataelement_dsd",
      "dataelement_ta",
      "dataset",
      "indicator_code",
      "invalid_DSD_DEs",
      "invalid_TA_DEs",
      "sheet_name"
    ),
    ignore.order = TRUE
  )

})

test_that("We can flag invalid data elements´", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  bad_schema <- ref_schema %>%
    dplyr::mutate(dataelement_dsd = dplyr::case_when(col %% 4 == 0 ~ "foobar",
                                                     TRUE ~ dataelement_dsd),
                  dataelement_ta = dplyr::case_when(col %% 5 == 0 ~ "abc123",
                                                    TRUE ~ dataelement_ta))
  test_results <- checkSchema_DataElementSyntax(bad_schema)

  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) > 0)
  expect_named(
    test_results,
    c(
      "col",
      "dataelement_dsd",
      "dataelement_ta",
      "dataset",
      "indicator_code",
      "invalid_DSD_DEs",
      "invalid_TA_DEs",
      "sheet_name"
    ),
    ignore.order = TRUE
  )

})


test_that("We can pass valid category option identifiers", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  test_results <- checkSchema_COsSyntax(ref_schema)

  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) == 0)
  expect_named(
    test_results,
    c(
      "categoryoption_specified", "col", "indicator_code", "invalid_COs", "sheet_name"
    ),
    ignore.order = TRUE
  )

})

test_that("We can flag invalid data elements´", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)

  bad_schema <- ref_schema %>%
    dplyr::mutate(categoryoption_specified = dplyr::case_when(col %% 4 == 0 ~ "foobar",
                                                     TRUE ~ categoryoption_specified))
  test_results <- checkSchema_COsSyntax(bad_schema)

  expect_true(is.data.frame(test_results))
  expect_true(NROW(test_results) > 0)
  expect_named(
    test_results,
    c(
      "categoryoption_specified", "col", "indicator_code", "invalid_COs", "sheet_name"
    ),
    ignore.order = TRUE
  )

})

test_that("We can pass when a schema is valid", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)
  test_results <- checkSchema(schema = ref_schema, cop_year = cop_year, tool = tool)
  expect_true(length(test_results) == 0)
})

test_that("We can flag when a schema is invalid", {
  tool <- "Data Pack"
  cop_year <- 2024
  ref_schema <- pick_schema(cop_year, tool)
  bad_schema <- ref_schema %>%
    flubSkippedSheets(.) %>%
    flubColumnTypes(.)
  test_results <- checkSchema(schema = bad_schema, cop_year = cop_year, tool = tool)
  expect_true(length(test_results) > 0)
})
