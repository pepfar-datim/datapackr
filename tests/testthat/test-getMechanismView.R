context("Get a mechanism view")

  with_mock_api({

    test_that("We can get a mechanism list ", {
      mechs <- getMechanismView(d2_session = training)
      expect_named(mechs, c("mechanism_desc",
                           "mechanism_code",
                           "attributeOptionCombo",
                           "partner_desc",
                           "partner_id", "agency", "ou", "startdate", "enddate"))

      expect_true(NROW(mechs) > 0)

      expect_false(any(duplicated(mechs$mechanism_desc)))
      expect_false(any(duplicated(mechs$mechanism_code)))
      expect_false(any(duplicated(mechs$attributeOptionCombo)))
      expect_true(all(is_uidish(mechs$attributeOptionCombo)))


      zambia_mechs <- getMechanismView(country_uids = "f5RoebaDLMx",
                                       cop_year = 2024,
                                       include_dedupe = FALSE,
                                       include_default = FALSE,
                                       include_MOH = FALSE,
                                       d2_session = training)

      #Zambia mechs only
      expect_identical(unique(zambia_mechs$ou), "Zambia")

      #Should not include dedupe
      expect_false(any(zambia_mechs$mechanism_code %in% c("00000", "000001")))

      zambia_mechs <- getMechanismView(country_uids = "f5RoebaDLMx",
                                       include_dedupe = TRUE,
                                       include_default = FALSE,
                                       include_MOH = FALSE,
                                       d2_session = training)
      #Should include dedupe
      expect_equal(sum(zambia_mechs$mechanism_code %in% c("00000", "00001")), 2L)

      #Bogus country UIDS with no defaults produces no rows
      expect_warning(getMechanismView(country_uids = "abcdef12345",
                                      include_default = FALSE,
                                      d2_session = training))
      #Bogus cop year gives an error
      expect_error(getMechanismView(cop_year = 1999, d2_session = training))
    })
  })
