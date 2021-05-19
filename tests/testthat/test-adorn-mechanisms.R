
context("Get a mechanism view when logged into DATIM")



  with_mock_api({
    test_that("We can get a warning when not logged in", {
      skip("Need to simulate a dead handle.")
      datimutils::loginToDATIM(config_path = test_config("test-config.json"))
      expect_true(exists("d2_default_session"))
      #Kill the handle to simulate a dead one
      #TODO: Not clear why the method expects a blank URL....
      d2_default_session$base_url<-NULL
      test_mech_list<-expect_warning(getMechanismViewFromDATIM(d2_session = d2_default_session))
      expect_type(test_mech_list,"list")
      #Should return an empty mechs list
      expect_equal(NROW(test_mech_list),0)
      mechs_names<-c("mechanism_desc",
                     "mechanism_code",
                     "attributeOptionCombo",
                     "partner_desc","partner_id",
                     "agency","ou",
                     "startdate","enddate")
      expect_true(setequal(mechs_names,names(test_mech_list)))
    })
  })
 
 
  with_mock_api({
    test_that("We can get a mechanism list when logged in", {
      datimutils::loginToDATIM(config_path = test_config("test-config.json"))
      expect_true(exists("d2_default_session"))
      datasets<-c("MqNLEXmzIzr","kkXf2zXqTM0")
      test_mech_list<-getMechanismViewFromDATIM(d2_session = d2_default_session)
      expect_type(test_mech_list,"list")
      mechs_names<-c("mechanism_desc",
                     "mechanism_code",
                     "attributeOptionCombo",
                     "partner_desc","partner_id",
                     "agency","ou",
                     "startdate","enddate")
      expect_true(setequal(mechs_names,names(test_mech_list)))
    })
  })