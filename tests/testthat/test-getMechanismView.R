
context("Get a mechanism view when logged into DATIM")

  with_mock_api({
    # This part works.
    test_that("We can get a mechanism list when logged in", {
      test_mech_list <- datimutils::getSqlView(sql_view_uid = "fgUtV6e9YIX",
                                               d2_session = training)
      
      expect_type(test_mech_list, "list")
      
      mechs_names <- c("mechanism",
                     "code",
                     "uid",
                     "partner",
                     "primeid",
                     "agency",
                     "ou",
                     "startdate",
                     "enddate")
      
      expect_true(setequal(mechs_names, names(test_mech_list)))
      
      #Expect error on a faulty dataset UID
      expect_error(datimutils::getSqlView("foo"))
    })
  })
