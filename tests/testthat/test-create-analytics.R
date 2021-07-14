context("Analytics creation tests") 


with_mock_api({
  test_that("We can create analytics", {
    
    d <-
      datapackr:::createKeychainInfo(
        submission_path = test_sheet('COP21_DP_random_with_psnuxim.xlsx'),
        tool = "Data Pack",
        country_uids = NULL,
        cop_year = NULL
      )
    d <-  d %>% unPackSheets(.) %>%
      separateDataSets(.) %>%
      unPackSNUxIM(.) %>%
      packForDATIM(., type = "Undistributed MER") %>%
      packForDATIM(., type = "SUBNAT_IMPATT") %>%
      packForDATIM(., type = "PSNUxIM") 
    
    expect_named(d,
                 c("keychain", "info", "tests", "data", "datim"),
                 ignore.order = TRUE)
    
    
    fy22_prioritizations<-getFY22Prioritizations(d)
    expect_type(fy22_prioritizations,"list")
    expect_true(NROW(fy22_prioritizations)>0)
    expect_named(fy22_prioritizations,c("orgUnit","value"),ignore.order = TRUE)
    
    d %<>% createAnalytics(d2_session = training) 
    

    analytics_column_names <-
      c(
        'ou','ou_id','country_name','country_uid','snu1','snu1_id','psnu',
        'psnu_uid','prioritization','mechanism_code','mechanism_desc','partner_id',
        'partner_desc','funding_agency','fiscal_year','dataelement_id','dataelement_name',
        'indicator','numerator_denominator','support_type','hts_modality',
        'categoryoptioncombo_id','categoryoptioncombo_name','age','sex',
        'key_population','resultstatus_specific','upload_timestamp',
        'disagg_type','resultstatus_inclusive','top_level',
        'target_value',
        'indicator_code'
      )
    expect_true(!is.null(d$data$analytics))
    expect_true(NROW(d$data$analytics) > 0)
    expect_named(d$data$analytics,analytics_column_names)
   numeric_columns<-c("fiscal_year","target_value")
   foo<-sapply(d$data$analytics,typeof)
   expect_true(all(foo[!(names(d$data$analytics) %in%  numeric_columns)] == "character"))
   expect_true(all(foo[(names(d$data$analytics) %in%  numeric_columns)] == "double"))
   expect_true(all(sapply(d$data$analytics$ou_id,is_uidish)))
   expect_true(all(sapply(d$data$analytics$country_uid,is_uidish)))
   expect_true(all(sapply(d$data$analytics$snu1_id,is_uidish)))
   expect_true(all(sapply(d$data$analytics$psnu_uid,is_uidish)))
   expect_true(all(sapply(d$data$analytics$dataelement_id,is_uidish)))
  })
})