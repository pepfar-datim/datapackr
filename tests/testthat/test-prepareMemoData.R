context("prepare-memo-data")

with_mock_api({
  test_that("We can prepare memo metadata", {
    d <- list()
    d$info$country_uids <- "cDGPF739ZZr"
    d$info$cop_year <- 2024
    d$data$analytics <-
      data.frame(psnu_uid = "uXwFHXCPYgj", prioritization = "Attained")
    d <- prepareMemoMetadata(d, "datapack", d2_session = training)
    expect_true(is.list(d$memo))
    expect_setequal(names(d$memo),
                    c("structure", "inds", "partners_agencies", "datapack"))

    expect_true(is.data.frame(d$info$psnus))
    expect_setequal(names(d$info$psnus),
                    c("ou", "country_name", "snu1", "psnu", "psnu_uid"))

    expect_true(is.list(d$memo$datapack$prios))
    expect_setequal(names(d$memo$datapack$prios),
                    c("orgUnit", "value"))
    expect_setequal(names(d$memo$partners_agencies),
                    c("Mechanism", "Partner", "Agency"))

    expect_true(is.list(d$memo$inds))
    expect_setequal(names(d$memo$inds),
                    c("name", "id", "numerator", "denominator"))
  })
})

with_mock_api({
  test_that("We can create Datapack memo data", {
    d <-
      loadDataPack(
        submission_path = test_sheet("COP24_sample_DataPack_Malawi.xlsx"),
        tool = "Data Pack",
        country_uids = NULL,
        cop_year = NULL,
        load_sheets = TRUE,
        d2_session = training)

    d %<>%
      unPackSheets(., check_sheets = FALSE) %>%
      packForDATIM(., type = "Undistributed MER") %>%
      packForDATIM(., type = "SUBNAT_IMPATT")

    expect_named(d,
                 c("keychain", "info", "sheets", "data", "datim"),
                 ignore.order = TRUE)

    d %<>% createAnalytics(d2_session = training)

    d %<>% prepareMemoData(
      memo_type = "datapack",
      include_no_prio = TRUE,
      d2_session = training,
      n_cores = 2L #Be nice to the CI ....
    )

    expect_setequal(names(d$memo$datapack),
                     c("prios", "by_psnu", "by_agency", "by_prio", "by_partner"))

    expect_setequal(names(d$memo$datapack$prios),
                    c("orgUnit",  "value"))
    #By PSNU
    expect_setequal(
      names(d$memo$datapack$by_psnu),
      c(
        "psnu_uid",
        "Mechanism",
        "Indicator",
        "Age",
        "value",
        "prioritization",
        "Partner",
        "Agency",
        "ou",
        "country_name",
        "snu1",
        "psnu"
      ))

    #By Prioritization
    expect_type(d$memo$datapack$by_prio, "list")
    expect_true(names(d$memo$datapack$by_prio)[[1]] == "Indicator")
    expect_true(names(d$memo$datapack$by_prio)[[2]] == "Age")
    expect_true(names(d$memo$datapack$by_prio)[[NCOL(d$memo$datapack$by_prio)]] == "Total")
    prio_cols_end <- NCOL(d$memo$datapack$by_prio) - 1

    #We have changed  the No prioritization label here....
    prio_dict_mod <- prioritization_dict() %>%
      dplyr::mutate(
        name = dplyr::case_when(
          name == "No Prioritization" ~ "No Prioritization - USG Only",
          TRUE ~ name
        )
      )

    expect_true(all(
      names(d$memo$datapack$by_prio[3:prio_cols_end]) %in% prio_dict_mod$name
    ))

    #Compare to confirm that the totals match
    totals_1 <- d$memo$datapack$by_psnu %>%
      dplyr::group_by(Indicator) %>%
      dplyr::summarise(value = sum(value), .groups = "drop") %>%
      dplyr::arrange(Indicator)


    totals_2 <- d$memo$datapack$by_prio %>%
      dplyr::filter(Age == "Total") %>%
      dplyr::select(Indicator, Total) %>%
      dplyr::mutate(Indicator = as.character(Indicator)) %>%
      dplyr::group_by(Indicator) %>%
      dplyr::summarise(value = sum(Total), .groups = "drop") %>%
      dplyr::arrange(Indicator)

    expect_identical(totals_1, totals_2)

    #By agency
    expect_type(d$memo$datapack$by_agency, "list")
    expect_true(names(d$memo$datapack$by_agency)[[1]] == "Indicator")
    expect_true(names(d$memo$datapack$by_agency)[[2]] == "Age")
    expect_true(names(d$memo$datapack$by_agency)[[NCOL(d$memo$datapack$by_agency)]] == "Total")

    totals_3 <- d$memo$datapack$by_agency %>%
      dplyr::filter(Age == "Total") %>%
      dplyr::select(Indicator, Total) %>%
      dplyr::mutate(Indicator = as.character(Indicator)) %>%
      dplyr::group_by(Indicator) %>%
      dplyr::summarise(value = sum(Total), .groups = "drop") %>%
      dplyr::arrange(Indicator)

    expect_identical(totals_1, totals_3)

    #By Partner
    #Only test the structure here....
    expect_identical(names(d$memo$datapack$by_partner)[1:3], c("Agency", "Partner", "Mechanism"))

    #Difficult to test the Word doc, but lets just confirm it works
    doc <- generateApprovalMemo(d, memo_type = "comparison", draft_memo = TRUE, d2_session = training)
    expect_equal(class(doc), "rdocx")

  })
})

with_mock_api({
  test_that("We can prepare existing data analytics", {
    d <-
      loadDataPack(
        submission_path = test_sheet("COP24_sample_DataPack_Malawi.xlsx"),
        tool = "Data Pack",
        country_uids = NULL,
        cop_year = 2024,
        load_sheets = TRUE,
        d2_session = training)

    d %<>%
      unPackSheets(., check_sheets = FALSE) %>%
      # unPackSNUxIM(.) %>%
      packForDATIM(., type = "Undistributed MER") %>%
      packForDATIM(., type = "SUBNAT_IMPATT") #%>%
      # packForDATIM(., type = "PSNUxIM")
    #Datapack analytics
    d <- createAnalytics(d, training)
    #DATIM analytics
    d <- prepareExistingDataAnalytics(d, training)


    expect_type(d$memo$datim$analytics, "list")
    expect_true(NROW(d$memo$datim$analytics) > 0)
    #They should have the same structure
    expect_setequal(names(d$data$analytics), names(d$memo$datim$analytics))
  })
})

with_mock_api({
  test_that("We can create DATIM/Comparison memo data", {
    d <-
      loadDataPack(
        submission_path = test_sheet("COP24_sample_DataPack_Malawi.xlsx"),
        tool = "Data Pack",
        country_uids = NULL,
        cop_year = 2024,
        load_sheets = TRUE,
        d2_session = training)

    d %<>%
      unPackSheets(., check_sheets = FALSE) %>%
      # unPackSNUxIM(.) %>%
      packForDATIM(., type = "Undistributed MER") %>%
      packForDATIM(., type = "SUBNAT_IMPATT") #%>%
      # packForDATIM(., type = "PSNUxIM")
    #Datapack analytics
    d <- createAnalytics(d, training)

    #DATIM analytics
    d <-
      prepareMemoData(d,
                      "comparison",
                      d2_session = training,
                      n_cores = 2L)

    expect_type(d$memo$datim$analytics, "list")
    expect_true(NROW(d$memo$datim$analytics) > 0)
    #They should have the same structure
    expect_setequal(names(d$data$analytics), names(d$memo$datim$analytics))

    expect_setequal(names(d$memo$datim),
                    c("prios", "by_psnu", "by_agency", "by_prio", "by_partner", "analytics"))

    expect_setequal(names(d$memo$datim$prios),
                    c("orgUnit", "value"))
    #By PSNU
    expect_setequal(
      names(d$memo$datim$by_psnu),
      c(
        "psnu_uid",
        "Mechanism",
        "Indicator",
        "Age",
        "value",
        "prioritization",
        "Partner",
        "Agency",
        "ou",
        "country_name",
        "snu1",
        "psnu"
      ))

    #By Prioritization
    expect_type(d$memo$datim$by_prio, "list")
    expect_true(names(d$memo$datim$by_prio)[[1]] == "Indicator")
    expect_true(names(d$memo$datim$by_prio)[[2]] == "Age")
    expect_true(names(d$memo$datim$by_prio)[[NCOL(d$memo$datim$by_prio)]] == "Total")
    prio_cols_end <- NCOL(d$memo$datim$by_prio) - 1
    expect_true(all(
      names(d$memo$datim$by_prio[3:prio_cols_end]) %in% prioritization_dict()$name
    ))

    d <- generateComparisonTable(d)
    expect_type(d$memo$comparison, "list")
    expect_true(NROW(d$memo$comparison) > 0)
    expect_named(
      d$memo$comparison,
      c(
        "Mechanism",
        "Indicator",
        "Age",
        "prioritization",
        "Partner",
        "Agency",
        "ou",
        "country_name",
        "snu1",
        "psnu",
        "Data Type",
        "value"
      ), ignore.order = TRUE
    )

    expect_setequal(unique(d$memo$comparison$`Data Type`), c("Current", "Proposed", "Percent diff", "Diff"))
    expect_type(d$memo$comparison$value, "double")

    #Test generating expanded comparison table
    d <- generateComparisonTable(d, expanded = TRUE)
    expect_type(d$memo$comparison, "list")
    expect_true(NROW(d$memo$comparison) > 0)
    expect_named(
      d$memo$comparison,
      c(
        "Mechanism",
        "Indicator",
        "Age",
        "prioritization",
        "Partner",
        "Agency",
        "ou",
        "country_name",
        "snu1",
        "psnu",
        "Data Type",
        "value",
        "Identical"
      ), ignore.order = TRUE
    )

    expect_setequal(unique(d$memo$comparison$`Data Type`), c("Current", "Proposed", "Percent diff", "Diff"))
    expect_type(d$memo$comparison$value, "double")

  })
})
# c("Proposed", "Current", "Identical", "Diff", "Percent diff")
