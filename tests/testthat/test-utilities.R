context("Test utility functions")

# swapColumns ----
test_that("Testing swapping of columns...", {

  # create input data frames
  df1 <-
    data.frame(
      w = c(0, 9, 8),
      x = c(1, 2, 3),
      y = c(4, 5, 6)
    )

  df2 <-
    data.frame(
      x = c(9, 9, 9),
      y = c(1, 1, 1),
      z = c(6, 6, 6)
    )

  # create output data frame
  df_output <-
    data.frame(
      w = c(0, 9, 8),
      x = c(9, 9, 9),
      y = c(1, 1, 1)
    )

  # perform test when not tibble
  res <- swapColumns(df1, df2)
  expect_equal(res, df_output)
  rm(res)

  # perform test when  tibble
  res <- swapColumns(df1 %>% tibble(), df2 %>% tibble())
  expect_equal(res, df_output %>% tibble())

})


test_that("Get a map of data elements and category options", {
  expect_error(getMapDataPack_DATIM_DEs_COCs("foo"))
  expect_error(getMapDataPack_DATIM_DEs_COCs(2023))
  expect_identical(getMapDataPack_DATIM_DEs_COCs(2021), datapackr::cop21_map_DataPack_DATIM_DEs_COCs)
  expect_identical(getMapDataPack_DATIM_DEs_COCs(2022), datapackr::cop22_map_DataPack_DATIM_DEs_COCs)
  expect_identical(getMapDataPack_DATIM_DEs_COCs("2021"), datapackr::cop21_map_DataPack_DATIM_DEs_COCs)

  de_map_names <- c("indicator_code", "col_type", "value_type",
                    "categoryoption_specified", "valid_ages.name",
                    "valid_ages.id", "valid_sexes.name", "valid_sexes.id",
                    "valid_kps.name", "valid_kps.id", "FY", "period",
                    "categoryOptions.ids", "dataelementuid", "hts_modality",
                    "period_dataset", "dataelementname", "categoryoptioncomboname",
                    "categoryoptioncombouid", "targets_results", "dataset", "resultstatus",
                    "resultstatus_inclusive", "disagg_type", "technical_area", "top_level",
                    "support_type", "numerator_denominator")


  de_map <- getMapDataPack_DATIM_DEs_COCs(2022)
  expect_named(de_map, de_map_names)
  de_map <- getMapDataPack_DATIM_DEs_COCs(2021)
  expect_named(de_map, de_map_names)

  expect_true(is.data.frame(de_map))

})


test_that("Can extract a UID", {
  uid_we_want <- "A1234bcDE56"
  string_w_uid <- paste0("This is a UID [", uid_we_want, "]")
  uid <- extract_uid(string_w_uid)

  expect_true(uid == uid_we_want)

  uid_1 <- uid_we_want
  uid_2 <- "B678cdEfg91"
  string_w_uids <- paste0("[", uid_1, ".", uid_2, "]")
  uids <- extract_uid_all(string_w_uids)

  expect_true(setequal(uids, c(uid_1, uid_2)))

})
