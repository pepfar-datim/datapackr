
context("Get a mechanism view when logged into DATIM")

with_mock_api({
  test_that("We can get a mechanism list when logged in", {
    datimutils::loginToDATIM(config_path = secrets)
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