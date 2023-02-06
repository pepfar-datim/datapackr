# library(datapackr)
# library(dplyr)
# library(tibble)
#
# psnuxim_model_data <- readRDS("/Users/faustolopezbao/Documents/psnuxim2023Models/PSNUxIM_COP22_2022-11-07 (1).rds")
# snuxim_model_data <- psnuxim_model_data
#


testthat::test_that("can test prepare_model_data.PSNUXIM...", {

  # data
  dummy_snuxim_data <-
    list(
    )

  # dsd row
  dummy_snuxim_data$Qh4XMQJhbk8 <-
    tibble::tribble(
      ~"indicator_code", ~"psnu_uid", ~"mechanism_uid", ~"mechanism_code", ~"type", ~"age_option_name",
      ~"age_option_uid", ~"sex_option_name", ~"sex_option_uid", ~"value", ~"kp_option_uid", ~"kp_option_name",
      ~"percent",
      "GEND_GBV.PE.T", "BDfJLRZI6JM", "qcVSyZ8L9HX", "AFR1", "DSD", "NA", "NA", "NA", "NA", NA, "NA", 89.00, 1,
      "GEND_GBV.PE.T", "bRAFS0BaYNS", "xDplpmZisgM", "81566", "DSD", "NA", "NA", "NA", "NA", NA, "NA", 41.00, 1,
      "GEND_GBV.PE.T", "bRAFS0BaYNS", "xDplpmZisgM", "81573", "DSD", "NA", "NA", "NA", "NA", NA, "NA", 43.00, 1,
      "PMTCT_EID.N.2.T", "BDfJLRZI6JM", "xDplpmZisgM", "18579", "DSD", "<= 02 months", "J4SQd7SnDi2", NA, "NA",
      16, NA, NA, 1, "PMTCT_EID.N.12.T", "BDfJLRZI6JM", "xDplpmZisgM", "18579", "DSD", "<= 02 months",
      "J4SQd7SnDi2", NA, "NA", 1, NA, NA, 1,
    )

  country_uids <- c("Qh4XMQJhbk8")

  # test filtering of mechanism codes and concatenation of the type ----
  dummy_snuxim_data_df <- dummy_snuxim_data[country_uids] %>%
    dplyr::bind_rows()
  res <- .prepareMechs(dummy_snuxim_data_df)
  testthat::expect_identical(unique(res$mechcode_supporttype), c("81566_DSD", "81573_DSD", "18579_DSD"))


  # test pivoting of data ----
  dummy_snuxim_data_df <- dummy_snuxim_data[country_uids] %>%
    dplyr::bind_rows() %>%
    dplyr::slice(1:3, )
  dummy_snuxim_data_df <-   .prepareMechs(snuxim_model_data = dummy_snuxim_data_df)
  res <- .pivotSnuximData(snuxim_model_data = dummy_snuxim_data_df)
  # expect these column names as output
  testthat::expect_identical(
    names(res),
    c("psnu_uid", "indicator_code", "Age", "Sex", "KeyPop", "value", "81566_DSD", "81573_DSD")
  )
  # expect pivoted data frame with percent followed by value
  testthat::expect_identical(
    res,
    tibble::tribble(
      ~"psnu_uid", ~"indicator_code", ~"Age", ~"Sex", ~"KeyPop", ~"value", ~"81566_DSD", ~"81573_DSD",
      "bRAFS0BaYNS", "GEND_GBV.PE.T",  "NA",    "NA",        41,    NA,           1,          NA,
      "bRAFS0BaYNS", "GEND_GBV.PE.T",  "NA",    "NA",        43,    NA,          NA,           1
      ) %>%
      dplyr::mutate(
        dplyr::across(value, as.numeric)
      )
  )

  # test treatment of NA's conditionally for age bands ----
  dummy_snuxim_data_df <- dummy_snuxim_data[country_uids] %>%
    dplyr::bind_rows()
  dummy_snuxim_data_df <-   .prepareMechs(snuxim_model_data = dummy_snuxim_data_df)
  res <- .pivotSnuximData(snuxim_model_data = dummy_snuxim_data_df) %>%
    .treatAgeBands(.)
  testthat::expect_equal(
    is.na(unique(res[res$indicator_code %in% c("PMTCT_EID.N.2.T", "PMTCT_EID.N.12.T"), ]$Age)),
    TRUE
  )

  # test adding dedupe columns ----
  dummy_snuxim_data_df <- dummy_snuxim_data[country_uids] %>%
    dplyr::bind_rows() %>%
    .prepareMechs(.) %>%
    .pivotSnuximData(.) %>%
    .treatAgeBands(.)

  # expect the columns were added
  res <- .addDedupeCols(dummy_snuxim_data_df)
  testthat::expect_true(
    all(c("DSD Dedupe",
      "TA Dedupe",
      "Crosswalk Dedupe") %in%
      names(res))
  )

  # expect they are of type numeric
  testthat::expect_equal(
    unique(lapply(res[, c("DSD Dedupe", "TA Dedupe", "Crosswalk Dedupe")], class))[[1]],
    "numeric"
  )

  # test that the columns are added as expected ----
  dummy_snuxim_data_df <- dummy_snuxim_data[country_uids] %>%
    dplyr::bind_rows() %>%
    .prepareMechs(.) %>%
    .pivotSnuximData(.) %>%
    .treatAgeBands(.) %>%
    .addDedupeCols(.)

  res <- .createDeduplicatedRollups(dummy_snuxim_data_df)

  testthat::expect_equal(
    all(
      c("Total Duplicated Rollup",   "DSD Duplicated Rollup",     "TA Duplicated Rollup",
        "Deduplicated DSD Rollup",   "Deduplicated TA Rollup",    "Total Deduplicated Rollup") %in%
        names(res)
    ),
    TRUE
  )

  # check the value of those columns are indeed conditional rowsums
  testthat::expect_equal(
    res$`Total Duplicated Rollup`,
    rowSums(dummy_snuxim_data_df %>% dplyr::select(., tidyselect::matches("\\d{4,}|HllvX50cXC0")), na.rm = TRUE)
  )
  testthat::expect_equal(
    res$`DSD Duplicated Rollup`,
    rowSums(dummy_snuxim_data_df %>% dplyr::select(., tidyselect::matches("\\d{4,}_DSD")), na.rm = TRUE)
  )
  testthat::expect_equal(
    res$`TA Duplicated Rollup`,
    rowSums(dummy_snuxim_data_df %>% dplyr::select(., tidyselect::matches("\\d{4,}_TA")), na.rm = TRUE)
  )

  # check that deduplicated totals are inddeed accurate rowsums
  testthat::expect_equal(
    res$`Deduplicated DSD Rollup`,
    rowSums(res %>% dplyr::select(., tidyselect::all_of(c("DSD Duplicated Rollup", "DSD Dedupe"))),
            na.rm = TRUE)
  )

  testthat::expect_equal(
    res$`Deduplicated TA Rollup`,
    rowSums(res %>% dplyr::select(., tidyselect::all_of(c("TA Duplicated Rollup", "TA Dedupe"))),
            na.rm = TRUE)
  )

  testthat::expect_equal(
    res$`Total Deduplicated Rollup`,
    rowSums(
      res %>%
        dplyr::select(.,
                      tidyselect::all_of(c("Deduplicated DSD Rollup",
                                           "Deduplicated TA Rollup",
                                           "Crosswalk Dedupe"))),
      na.rm = TRUE
    )
  )

  # test that max columns are added ----
  dummy_snuxim_data_df <- dummy_snuxim_data[country_uids] %>%
    dplyr::bind_rows() %>%
    .prepareMechs(.) %>%
    .pivotSnuximData(.) %>%
    .treatAgeBands(.) %>%
    .addDedupeCols(.) %>%
    .createDeduplicatedRollups(.)

  res <- .createMaxCols(dummy_snuxim_data_df)

  testthat::expect_equal(
    all(
      c("Max_TA.T_1", "Max_DSD.T_1", "Max_Crosswalk.T_1") %in%
        names(res)
    ),
    TRUE
  )

  testthat::expect_equal(
    res$Max_TA.T_1,
    (dummy_snuxim_data_df %>% datapackr::rowMax(cn = "Max_TA.T_1", regex = "\\d{4,}_TA"))$Max_TA.T_1
  )
  testthat::expect_equal(
    res$Max_DSD.T_1,
    (dummy_snuxim_data_df %>% datapackr::rowMax(cn = "Max_DSD.T_1", regex = "\\d{4,}_DSD"))$Max_DSD.T_1
  )
  testthat::expect_equal(
    res$Max_Crosswalk.T_1,
    pmax(dummy_snuxim_data_df$`Deduplicated DSD Rollup`, dummy_snuxim_data_df$`Deduplicated TA Rollup`, na.rm = TRUE)
  )

  # test im count columns are created properly ----
  dummy_snuxim_data_df <- dummy_snuxim_data[country_uids] %>%
    dplyr::bind_rows() %>%
    .prepareMechs(.) %>%
    .pivotSnuximData(.) %>%
    .treatAgeBands(.) %>%
    .addDedupeCols(.) %>%
    .createDeduplicatedRollups(.) %>%
    .createMaxCols(.)

  res <- .createImCounts(snuxim_model_data = dummy_snuxim_data_df)

  testthat::expect_equal(
    all(
      c("ta_im_count", "dsd_im_count") %in% names(res)
    ),
    TRUE
  )

  testthat::expect_equal(
    res$ta_im_count,
    c(0, 0, 0, 0)
  )

  testthat::expect_equal(
    res$dsd_im_count,
    c(1, 1, 1, 1)
  )

  # test resolution columns created and final selection ----
  dummy_snuxim_data_df <- dummy_snuxim_data[country_uids] %>%
    dplyr::bind_rows() %>%
    .prepareMechs(.) %>%
    .pivotSnuximData(.) %>%
    .treatAgeBands(.) %>%
    .addDedupeCols(.) %>%
    .createDeduplicatedRollups(.) %>%
    .createMaxCols(.) %>%
    .createImCounts(.)

  res <- .createResolutionCols(snuxim_model_data = dummy_snuxim_data_df)

  # these are the columns we expect based on our input
  testthat::expect_equal(
  all(c(
    "psnu_uid", "indicator_code", "Age", "Sex", "KeyPop", "81566_DSD", "81573_DSD",
    "18579_DSD", "Custom DSD Dedupe Allocation (FY22) (% of DataPackTarget)",
    "Custom TA Dedupe Allocation (FY22) (% of DataPackTarget)",
    "Custom Crosswalk Dedupe Allocation (FY22) (% of DataPackTarget)",
    "DSD Dedupe Resolution (FY22)", "TA Dedupe Resolution (FY22)",
    "Crosswalk Dedupe Resolution (FY22)", "DSD Dedupe", "TA Dedupe",
    "Crosswalk Dedupe"
  ) %in% names(res)
  ),
  TRUE
  )

  # test values of the columns
  testthat::expect_equal(
    res$`TA Dedupe Resolution (FY22)`,
    (dummy_snuxim_data_df %>%
       dplyr::mutate(
         x = dplyr::case_when(
           `TA Duplicated Rollup` == 0 | ta_im_count <= 1 ~ NA_character_,
           # or where count(TA IMs) == 1
           `Deduplicated TA Rollup` == `TA Duplicated Rollup` ~ "SUM",
           `Deduplicated TA Rollup` == `Max_TA.T_1` ~ "MAX",
           TRUE ~ "CUSTOM")
       ))$x
  )

  testthat::expect_equal(
    res$`DSD Dedupe Resolution (FY22)`,
    (dummy_snuxim_data_df %>%
        dplyr::mutate(
          x = dplyr::case_when(
            `DSD Duplicated Rollup` == 0 | dsd_im_count <= 1 ~ NA_character_,
            `Deduplicated DSD Rollup` == `DSD Duplicated Rollup` ~ "SUM",
            `Deduplicated DSD Rollup` == `Max_DSD.T_1` ~ "MAX",
            TRUE ~ "CUSTOM")
        ))$x
  )

  testthat::expect_equal(
    res$`Crosswalk Dedupe Resolution (FY22)`,
    (dummy_snuxim_data_df %>%
       dplyr::mutate(
         x = dplyr::case_when(
           `Total Duplicated Rollup` == 0 | `Deduplicated TA Rollup` == 0 | `Deduplicated DSD Rollup` == 0
           ~ NA_character_,
           `Total Deduplicated Rollup` == `Total Duplicated Rollup` ~ "SUM",
           `Total Deduplicated Rollup` == `Max_Crosswalk.T_1` ~ "MAX",
           TRUE ~ "CUSTOM")
       ))$x
  )

  testthat::expect_equal(
    res$`Custom DSD Dedupe Allocation (FY22) (% of DataPackTarget)`,
    dummy_snuxim_data_df$`DSD Dedupe`
    )
  testthat::expect_equal(
    res$`Custom TA Dedupe Allocation (FY22) (% of DataPackTarget)`,
    dummy_snuxim_data_df$`TA Dedupe`
    )
  testthat::expect_equal(
    res$`Custom Crosswalk Dedupe Allocation (FY22) (% of DataPackTarget)`,
    dummy_snuxim_data_df$`Crosswalk Dedupe`
    )

})
