context("Valid orgunit structure")

test_that("Can error on a missing cop year", {
  expect_error(getValidOrgUnits(NULL))
  expect_error(getValidOrgUnits(NA))
  expect_error(getValidOrgUnits(c(2021, 2022)))
  expect_silent(getValidOrgUnits(getCurrentCOPYear()))

})


test_that(
  "Structure of orgunits is the defined",
  {
    all_cop_years <- supportedCOPYears()
    expect_silent(all_orgunits <-
                    lapply(all_cop_years, getValidOrgUnits))

    expected_names <- c(
      "name",
      "uid",
      "org_type",
      "ou",
      "ou_uid",
      "country_name",
      "country_uid",
      "snu1",
      "snu1_uid",
      "lastUpdated",
      "ancestors",
      "organisationUnitGroups",
      "DREAMS"
    )

    expect_true(all(purrr::map_lgl(
      all_orgunits, ~ setequal(names(.x), expected_names)
    )))

    expect_true(all(purrr::map_lgl(all_orgunits, ~ all(is_uidish(
      .x$uid
    )))))

    expect_true(all(purrr::map_lgl(all_orgunits, ~ all(
      is_uidish(.x$ou_uid)
    ))))

    expect_true(all(purrr::map_lgl(all_orgunits, ~ all(
      is_uidish(.x$snu1_uid)
    ))))

    expect_true(all(purrr::map_lgl(all_orgunits, ~ all(
      is_uidish(.x$country_uid)
    ))))

    expect_true(all(purrr::map_lgl(all_orgunits, ~ is.list(.x$ancestors))))

    expect_true(all(purrr::map_lgl(
      all_orgunits, ~ is.list(.x$organisationUnitGroups)
    )))

    expect_true(all(purrr::map_lgl(all_orgunits, ~ is.integer(
      as.integer(as.Date(.x$lastUpdated, origin = "1970-01-01"))
    ))))


  })
