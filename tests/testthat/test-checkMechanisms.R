context("Check mechanism validity")


with_mock_api({
  test_that("Can flag invalid mechanisms", {

    d <- list()
    d$info$cop_year <- "2021"
    d$info$operating_unit$ou <- "Western Hemisphere Region"
    d$info$tool <- "Data Pack"
    d$info$messages <- MessageQueue()
    d$tests <- list()
    d$data$analytics <- data.frame(mechanism_code = c("100000", "10432"))
    d <- checkMechanisms(d, d2_session = training)
    expect_true(d$info$has_error)
    expect_true(!is.null(d$tests$bad_mechs))
    expect_equal(d$tests$bad_mechs$mechanism_code, "10432")
    expect_true(grepl("Invalid mechanisms", d$info$messages$message))
    expect_equal(d$info$messages$level, "ERROR")
  })
})
