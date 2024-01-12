context("test-check-analytics")

test_that("PMTCT_EID coverage by 2 months old < 90% expect message", {

  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~PMTCT_EID.N.2.T, ~PMTCT_EID.D.T, ~cop_year,
    "a", 1, "<1", "F", NA, 1, 100, 2024,
    "b", 1, "<1", "F", NA, 90, 10, 2024
  )

  foo <- analyze_eid_2mo(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$PMTCT_EID.2mo.rate, 0.0099, tolerance = 1e-3)


})

test_that("PMTCT_EID coverage by 2 months old < 90% all zeros expect NULL", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~PMTCT_EID.N.2.T, ~PMTCT_EID.D.T, ~cop_year,
    "a", 1, "<1", "F", NA, 0, 0, 2024
  )

  foo <- analyze_eid_2mo(data)
  expect_null(foo)

})

test_that("PMTCT_EID coverage by 2 months missing data", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~PMTCT_EID.N.2.T, ~cop_year,
    "a", 1, "<1", "F", NA, 0, 2024
  )

  foo <- analyze_eid_2mo(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$msg, "Missing data.")

})

test_that("PMTCT_EID coverage by 2 months old > 90% expect NULL", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~PMTCT_EID.N.2.T, ~PMTCT_EID.D.T, ~cop_year,
    "a", 1, "<1", "F", NA, 100, 1, 2024
  )

  foo <- analyze_eid_2mo(data)
  expect_null(foo)

})

test_that("VMMC_CIRC Indeterminate Rate < 5% expect message", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~VMMC_CIRC.Pos.T, ~VMMC_CIRC.Neg.T, ~VMMC_CIRC.Unk.T,
    "a", 1, "<1", "M", NA, 1, 100, 100,
    "b", 2, "<1", "M", NA, 0, 0, 0
  )

  foo <- analyze_vmmc_indeterminate(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$VMMC_CIRC.indeterminateRate, 0.498, tolerance = 1e-3)

})

test_that("VMMC_CIRC Indeterminate Rate > 5% missing data expect NULL", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~VMMC_CIRC.Pos.T, ~VMMC_CIRC.Neg.T,
    "a", 1, "<1", "M", NA, 1, 100,
    "b", 2, "<1", "M", NA, 0, 0
  )

  foo <- analyze_vmmc_indeterminate(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$msg, "Missing data.")

})

test_that("VMMC_CIRC Indeterminate Rate > 5% expect NULL", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~VMMC_CIRC.Pos.T, ~VMMC_CIRC.Neg.T, ~VMMC_CIRC.Unk.T,
    "a", 1, "<1", "M", NA, 1, 100, 1,
    "b", 2, "<1", "M", NA, 0, 0, 0
  )

  foo <- analyze_vmmc_indeterminate(data)
  expect_null(foo)

})

test_that("VMMC_CIRC Indeterminate Rate all zeros expect NULL", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~VMMC_CIRC.Pos.T, ~VMMC_CIRC.Neg.T, ~VMMC_CIRC.Unk.T,
    "a", 1, "<1", "M", NA, 0, 0, 0,
    "b", 2, "<1", "M", NA, 0, 0, 0
  )

  foo <- analyze_vmmc_indeterminate(data)
  expect_null(foo)

})

test_that("VMMC_CIRC Indeterminate Rate all keypop not NA expect NULL", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~VMMC_CIRC.Pos.T, ~VMMC_CIRC.Neg.T, ~VMMC_CIRC.Unk.T,
    "a", 1, "<1", "M", "PWID", 0, 0, 1
  )

  foo <- analyze_vmmc_indeterminate(data)
  expect_null(foo)

})

test_that("PMTCT Known Pos/PMTCT Total >  0.75 expect message", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,
    ~PMTCT_STAT.N.New.Pos.T, ~PMTCT_STAT.N.KnownPos.T, ~PMTCT_STAT.N.New.Neg.T,
    "a", 1, "<1", "M", NA, 10, 100, 10,
    "b", 2, "<1", "M", NA, 0, 0, 0
  )

  foo <- analyze_pmtctknownpos(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$knownpos_ratio, 0.833, tolerance = 1e-3)

})

test_that("PMTCT Known Pos/PMTCT Total >  0.75 missing data", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,
    ~PMTCT_STAT.N.New.Pos.T, ~PMTCT_STAT.N.KnownPos.T,
    "a", 1, "<1", "M", NA, 10, 100,
    "b", 2, "<1", "M", NA, 0, 0
  )

  foo <- analyze_pmtctknownpos(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$msg, "Missing data.")


})

test_that("PMTCT Known Pos/PMTCT Total <  0.75 expect null", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,
    ~PMTCT_STAT.N.New.Pos.T, ~PMTCT_STAT.N.KnownPos.T, ~PMTCT_STAT.N.New.Neg.T,
    "a", 1, "<1", "M", NA, 10, 10, 10,
    "b", 2, "<1", "M", NA, 0, 0, 0,
    "c", 3, "<1", "M", NA, 25, 150, 25
  )

  expect_null(analyze_pmtctknownpos(data))

})

test_that("PMTCT Known Pos/PMTCT Total all zeros expect null", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~PMTCT_STAT.N.New.Pos.T
    , ~PMTCT_STAT.N.KnownPos.T, ~PMTCT_STAT.N.New.Neg.T,
    "a", 1, "<1", "M", NA, 0, 0, 0,
    "b", 2, "<1", "M", NA, 0, 0, 0
  )

  expect_null(analyze_pmtctknownpos(data))

})

test_that("TB Known Pos ratio > 75% expect message", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~TB_STAT.N.New.Pos.T, ~TB_STAT.N.KnownPos.T, ~TB_STAT.N.New.Neg.T,
    "a", 1, "<1", "M", NA, 25, 156, 25,
    "b", 2, "<1", "M", NA, 0, 0, 0
  )

  foo <- analyze_tbknownpos(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$knownpos_ratio, 0.757, tolerance = 1e-3)


})

test_that("TB Known Pos ratio > 75% expect message", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~TB_STAT.N.New.Pos.T, ~TB_STAT.N.KnownPos.T,
    "a", 1, "<1", "M", NA, 25, 151,
    "b", 2, "<1", "M", NA, 0, 0
  )

  foo <- analyze_tbknownpos(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$msg, "Missing data.")

})

test_that("TB Known Pos ratio < 75% expect message expect null", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~TB_STAT.N.New.Pos.T, ~TB_STAT.N.KnownPos.T, ~TB_STAT.N.New.Neg.T,
    "a", 1, "<1", "M", NA, 25, 150, 25,
    "b", 2, "<1", "M", NA, 0, 0, 0,
  )

  expect_null(analyze_tbknownpos(data))

})

test_that("PMTCT Known Pos/PMTCT Total all zeros expect null", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~TB_STAT.N.New.Pos.T, ~TB_STAT.N.KnownPos.T, ~TB_STAT.N.New.Neg.T,
    "a", 1, "<1", "M", NA, 0, 0, 0,
    "b", 2, "<1", "M", NA, 0, 0, 0
  )

  expect_null(analyze_tbknownpos(data))

})

test_that(" Test retention < 98% expect message", {

  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~TX_CURR.T, ~TX_CURR.Expected.T_1, ~TX_NEW.T, ~ cop_year,
    "a", 1, "<1", "F", NA, 97, 97, 3, 2024,
    "b", 2, "<1", "M", NA, 0, 0, 0, 2024
  )

  foo <- analyze_retention(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$TX.Retention.T, 0.97, tolerance = 1e-3)

})

test_that(" Test retention < 98% missing required data", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~TX_CURR.T, ~TX_CURR.T_1, ~cop_year,
    "a", 1, "<1", "F", NA, 97, 97, 2024,
    "b", 2, "<1", "M", NA, 0, 0, 2024
  )

  foo <- analyze_retention(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$msg, "Missing data.")

})

test_that(" Test retention > 100% expect message", {

  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~TX_CURR.T, ~TX_CURR.Expected.T_1, ~TX_NEW.T, ~cop_year,
    "a", 1, "<1", "F", NA, 101, 10, 90, 2024,
    "b", 2, "<1", "M", NA, 100, 10, 90, 2024
  )

  foo <- analyze_retention(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$TX.Retention.T, 1.01, tolerance = 1e-3)


})

test_that(" Test retention  =  99% expect NULL", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~TX_CURR.T, ~TX_CURR.Expected.T_1, ~TX_NEW.T, ~cop_year,
    "a", 1, "<1", "F", NA, 99, 100, 0, 2024,
    "b", 2, "<1", "M", NA, 0, 0, 0, 2024
  )

  expect_null(analyze_retention(data))

})

test_that(" Test retention all zeros expect NULL", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~TX_CURR.T, ~TX_CURR.Expected.T_1, ~TX_NEW.T, ~cop_year,
    "a", 1, "<1", "F", NA, 0, 0, 0, 2024,
    "b", 2, "<1", "M", NA, 0, 0, 0, 2024
  )

  expect_null(analyze_retention(data))

})

test_that(" Test linkage < 95% expect message", {

  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~HTS.Index.Pos.T,
     ~HTS_TST.SNS.Pos.T, ~TX_NEW.T,~HTS_TST.KP.Pos.T, ~TX_NEW.KP.T, ~cop_year,
    "a", 1, "25-49", "F", NA, 95, 5, 94, 0, 0, 2024,
    "b", 2, "25-49", "M", NA, 95, 5, 95, 0, 0, 2024
  )

  foo <- analyze_linkage(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$HTS_TST.Linkage.T, 0.94, tolerance = 1e-3)
})

test_that(" Test linkage < 95% missing data", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~HTS_INDEX_COM.New.Pos.T,
    ~HTS_INDEX_FAC.New.Pos.T, ~TX_NEW.T, ~HTS_TST.KP.Pos.T,  ~cop_year,
    "a", 1, "25-49", "F", NA, 95, 5, 94, 0,  2024,
    "b", 2, "25-49", "M", NA, 95, 5, 95, 0,  2024
  )

  foo <- analyze_linkage(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$msg, "Missing data.")
})

test_that(" Test KP linkage < 95% expect message", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~HTS_INDEX_COM.New.Pos.T,
     ~HTS_INDEX_FAC.New.Pos.T, ~TX_NEW.T, ~HTS_TST.KP.Pos.T, ~TX_NEW.KP.T, ~cop_year,
    "a", 1, NA_character_, NA_character_, "PWID", 0, 0, 0, 100, 94, 2024,
    "b", 2, NA_character_, NA_character_, "PWID", 0, 0, 0, 100, 95, 2024
  )

  foo <- analyze_linkage(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$HTS_TST.KP.Linkage.T, 0.94, tolerance = 1e-3)
})

test_that(" Test linkage > 100% expect message", {

  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~HTS_TST.ActiveOther.Pos.T,
    ~HTS.Index.Pos.T, ~TX_NEW.T, ~HTS_TST.KP.Pos.T, ~TX_NEW.KP.T, ~cop_year,
    "a", 1, "25-49", "F", NA, 50, 50, 100, 0, 0, 2024,
    "b", 2, "25-49", "M", NA, 50, 50, 101, 0, 0, 2024
  )

  foo <- analyze_linkage(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$HTS_TST.Linkage.T, 1.01, tolerance = 1e-3)
})

test_that(" Test KP linkage > 100% expect message", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~HTS_INDEX_COM.New.Pos.T,
     ~HTS_INDEX_FAC.New.Pos.T, ~TX_NEW.T, ~HTS_TST.KP.Pos.T, ~TX_NEW.KP.T, ~cop_year,
    "a", 1, NA_character_, NA, "PWID", 0, 0, 0, 100, 100, 2024,
    "b", 2, NA_character_, NA, "PWID", 0, 0, 0, 100, 101, 2024
  )

  foo <- analyze_linkage(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$HTS_TST.KP.Linkage.T, 1.01, tolerance = 1e-3)
})

test_that(" Test linkage = 98% expect NULL", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~HTS_INDEX_COM.New.Pos.T,
     ~HTS_INDEX_FAC.New.Pos.T, ~TX_NEW.T, ~HTS_TST.KP.Pos.T, ~TX_NEW.KP.T, ~cop_year,
    "a", 1, "25-49", "F", NA, 20, 20, 39, 0, 0, 2024,
    "b", 2, "25-49", "M", NA, 0, 0, 0, 0, 0, 2024
  )

  expect_null(analyze_linkage(data))

})

test_that(" Test KP linkage = 98% expect NULL", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~HTS_INDEX_COM.New.Pos.T,
     ~HTS_INDEX_FAC.New.Pos.T, ~TX_NEW.T, ~HTS_TST.KP.Pos.T, ~TX_NEW.KP.T, ~cop_year,
    "a", 1, NA_character_, NA, "PWID", 0, 0, 0, 100, 98, 2024,
    "b", 2, NA_character_, NA, "PWID", 0, 0, 0, 0, 0, 2024
  )

  expect_null(analyze_linkage(data))

})

test_that(" Test linkage all zeros expect NULL", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~HTS_INDEX_COM.New.Pos.T,
    ~HTS_INDEX_FAC.New.Pos.T, ~TX_NEW.T, ~HTS_TST.KP.Pos.T, ~TX_NEW.KP.T, ~cop_year,
    "a", 1, "25-49", "F", NA, 0, 0, 0, 0, 0, 2024,
    "b", 2, "25-49", "M", NA, 0, 0, 0, 0, 0, 2024
  )

  expect_null(analyze_linkage(data))

})

test_that(" Test index pos ratio missing data", {

  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~HTS_INDEX_COM.New.Pos.T,
    ~HTS_INDEX_FAC.New.Pos.T, ~HTS_TST.PostANC1.Pos.T, ~TX_CURR_SUBNAT.T_1,  ~cop_year,
    "a", 1, "25-49", "F", NA, 5, 5, 100, 5,  2024

  )

  foo <- analyze_indexpos_ratio(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$msg, "Missing data.")
})

test_that(" Test index pos ratio", {

  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,
      ~HTS.Index.Pos.T, ~HTS_TST.PostANC1.Pos.T, ~TX_CURR_SUBNAT.T, ~PLHIV.T, ~cop_year,
    "a", 1, "25-49", "F", NA, 10,  100, 5, 100, 2024

  )

  foo <- analyze_indexpos_ratio(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$HTS_TST_POS.index_rate, 10 / 110, tolerance = 1e-3)
})

test_that(" Test linkage with age <1", {
  data <- tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population, ~HTS_INDEX_COM.New.Pos.T,
    ~HTS_INDEX_FAC.New.Pos.T, ~TX_NEW.T, ~HTS_TST.KP.Pos.T, ~TX_NEW.KP.T, ~cop_year,
    "a", 1, "<01", "M", NA, 50, 50, 101, 100, 100, 2024,
    "b", 2, NA, NA, "PWID", 0, 0, 0, 100, 101, 2024
  )

  foo <- analyze_linkage(data)
  testthat::expect_equal(class(foo), "list")
  testthat::expect_setequal(names(foo), c("test_results", "msg"))
  testthat::expect_equal(NROW(foo$test_results), 1)
  expect_equal(foo$test_results$HTS_TST.KP.Linkage.T, 1.01, tolerance = 1e-3)
})
