context("test-check-analytics")

test_that("PMTCT_EID coverage by 2 months old < 90% expect message" , {
  data<-tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,~PMTCT_EID.N.2.T,~PMTCT_EID.N.12.T,
    "a",   1,         "<1",  "F",  NA,                         1,        100
  )
  
  foo<-analyze_eid_2mo(data)
  testthat::expect_equal(class(foo),"list")
  testthat::expect_setequal(names(foo),c("test_results","msg"))
  testthat::expect_equal(NROW(foo$test_results),1)
  expect_equal(foo$test_results$PMTCT_EID.2mo.rate,0.0099,tolerance=1e-3)

} )

test_that("PMTCT_EID coverage by 2 months old < 90% all zeros expect NULL", {
  data<-tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,~PMTCT_EID.N.2.T,~PMTCT_EID.N.12.T,
    "a",   1,         "<1",  "F",  NA,                         0,        0
  )
  
  foo<-analyze_eid_2mo(data)
  expect_null(foo)
  
} )

test_that("PMTCT_EID coverage by 2 months old > 90% expect NULL", {
  data<-tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,~PMTCT_EID.N.2.T,~PMTCT_EID.N.12.T,
    "a",   1,         "<1",  "F",  NA,                         100,        1
  )
  
  foo<-analyze_eid_2mo(data)
  expect_null(foo)
  
} )

test_that("VMMC_CIRC Indeterminate Rate < 5% expect message" , {
  data<-tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,~VMMC_CIRC.Pos.T,~VMMC_CIRC.Neg.T,~VMMC_CIRC.Unk.T,
    "a",   1,         "<1",  "M",  NA,                         1,        100, 100,
    "b",  2,          "<1", "M", NA,                         0,         0, 0
  )
  
  foo<-analyze_vmmc_indeterminate(data)
  testthat::expect_equal(class(foo),"list")
  testthat::expect_setequal(names(foo),c("test_results","msg"))
  testthat::expect_equal(NROW(foo$test_results),1)
  expect_equal(foo$test_results$VMMC_CIRC.indeterminateRate,0.498,tolerance=1e-3)
  
} )

test_that("VMMC_CIRC Indeterminate Rate > 5% expect NULL" , {
  data<-tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,~VMMC_CIRC.Pos.T,~VMMC_CIRC.Neg.T,~VMMC_CIRC.Unk.T,
    "a",   1,         "<1",  "M",  NA,                         1,        100, 1,
    "b",  2,          "<1", "M", NA,                         0,         0, 0
  )
  
  foo<-analyze_vmmc_indeterminate(data)
  expect_null(foo)
  
} )

test_that("VMMC_CIRC Indeterminate Rate all zeros expect NULL" , {
  data<-tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,~VMMC_CIRC.Pos.T,~VMMC_CIRC.Neg.T,~VMMC_CIRC.Unk.T,
    "a",   1,         "<1",  "M",  NA,                         0,        0, 0,
    "b",  2,          "<1", "M", NA,                         0,         0, 0
  )
  
  foo<-analyze_vmmc_indeterminate(data)
  expect_null(foo)
  
} )

test_that("PMTCT Known Pos > PMTCT Total  expect message" , {
  data<-tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,~PMTCT_STAT.N.New.Pos.T,~PMTCT_STAT.N.KnownPos.T,~PMTCT_STAT.N.New.Neg.T,
    "a",   1,         "<1",  "M",  NA,                         0,        100, 0,
    "b",  2,          "<1", "M", NA,                         0,         0, 0
  )
  
  foo<-analyze_pmtctknownpos(data)
  skip('DP-252')
  testthat::expect_equal(class(foo),"list")
  testthat::expect_setequal(names(foo),c("test_results","msg"))
  testthat::expect_equal(NROW(foo$test_results),1)
  expect_equal(foo$test_results$knownpos_ratio,1.11,tolerance=1e-3)
  

} )

test_that(" Test retention < 98% expect message", {
  data<-tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,~TX_CURR.T,~TX_CURR.T_1,~TX_NEW.T,
    "a",   1,         "<1",  "F",  NA,                    10,        10, 10,
    "b",  2,          "<1", "M", NA,                         0,         0, 0
  )
  
  foo<-analyze_retention(data)
  testthat::expect_equal(class(foo),"list")
  testthat::expect_setequal(names(foo),c("test_results","msg"))
  testthat::expect_equal(NROW(foo$test_results),1)
  expect_equal(foo$test_results$TX.Retention.T,0.5,tolerance=1e-3)
  
} )

test_that(" Test retention > 100% expect message", {
  data<-tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,~TX_CURR.T,~TX_CURR.T_1,~TX_NEW.T,
    "a",   1,         "<1",  "F",  NA,                    100,        10, 10,
    "b",  2,          "<1", "M", NA,                         0,         0, 0
  )
  
  foo<-analyze_retention(data)
  testthat::expect_equal(class(foo),"list")
  testthat::expect_setequal(names(foo),c("test_results","msg"))
  testthat::expect_equal(NROW(foo$test_results),1)
  expect_equal(foo$test_results$TX.Retention.T,5,tolerance=1e-3)
  
} )

test_that(" Test retention = 99% expect NULL", {
  data<-tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,~TX_CURR.T,~TX_CURR.T_1,~TX_NEW.T,
    "a",   1,         "<1",  "F",  NA,                    99,        100, 0,
    "b",  2,          "<1", "M", NA,                         0,         0, 0
  )
  
  expect_null(analyze_retention(data))
  
} )

test_that(" Test retention all zeros expect NULL", {
  data<-tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,~TX_CURR.T,~TX_CURR.T_1,~TX_NEW.T,
    "a",   1,         "<1",  "F",  NA,                    0,        0, 0,
    "b",  2,          "<1", "M", NA,                         0,         0, 0
  )
  
  expect_null(analyze_retention(data))
  
} )

test_that(" Test linkage < 95% expect message", {
  data<-tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,~HTS_INDEX_COM.New.Pos.T,~HTS_INDEX_FAC.New.Pos.T,~TX_NEW.T,~HTS_TST.KP.Pos.T,~TX_NEW.KP.T,
    "a",   1,         "25-49",  "F",  NA,                    100,          10,                                10,                 0,               0,
    "b",  2,          "25-49", "M",  NA,                         0,           0,                                0,                 0,               0
  )
  
  foo<-analyze_linkage(data)
  testthat::expect_equal(class(foo),"list")
  testthat::expect_setequal(names(foo),c("test_results","msg"))
  testthat::expect_equal(NROW(foo$test_results),1)
  expect_equal(foo$test_results$HTS_TST.Linkage.T,0.0909,tolerance=1e-3)
  
} )

test_that(" Test linkage > 100% expect message", {
  data<-tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,~HTS_INDEX_COM.New.Pos.T,~HTS_INDEX_FAC.New.Pos.T,~TX_NEW.T,~HTS_TST.KP.Pos.T,~TX_NEW.KP.T,
    "a",   1,         "25-49",  "F",  NA,                    1,          1,                                10,                 0,               0,
    "b",  2,          "25-49", "M",  NA,                         0,           0,                                0,                 0,               0
  )
  
  foo<-analyze_linkage(data)
  testthat::expect_equal(class(foo),"list")
  testthat::expect_setequal(names(foo),c("test_results","msg"))
  testthat::expect_equal(NROW(foo$test_results),1)
  expect_equal(foo$test_results$HTS_TST.Linkage.T,5,tolerance=1e-3)
  
} )

test_that(" Test linkage = 98% expect NULL", {
  data<-tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,~HTS_INDEX_COM.New.Pos.T,~HTS_INDEX_FAC.New.Pos.T,~TX_NEW.T,~HTS_TST.KP.Pos.T,~TX_NEW.KP.T,
    "a",   1,         "25-49",  "F",  NA,                    20,          20,                                39,                 0,               0,
    "b",  2,          "25-49", "M",  NA,                         0,           0,                                0,                 0,               0
  )
  
  expect_null(analyze_linkage(data))
  
} )

test_that(" Test linkage all zeros expect NULL", {
  data<-tribble(
    ~psnu, ~psnu_uid, ~age, ~sex, ~key_population,~HTS_INDEX_COM.New.Pos.T,~HTS_INDEX_FAC.New.Pos.T,~TX_NEW.T,~HTS_TST.KP.Pos.T,~TX_NEW.KP.T,
    "a",   1,         "25-49",  "F",  NA,                    0,          0,                                0,                 0,               0,
    "b",  2,          "25-49", "M",  NA,                         0,           0,                                0,                 0,               0
  )
  
  expect_null(analyze_linkage(data))
  
} )