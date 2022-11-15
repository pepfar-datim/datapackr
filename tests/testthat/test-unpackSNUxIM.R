context("Unpack a PSNUxIM file")

test_that("Can error if d$data$PSNUxIM is not a data frame", {

  d <- list()
  expect_error(checkHasPSNUxIM(d))

})

test_that("Can check empty PSNUxIM", {

  d <- list()
  d$info$messages <- MessageQueue()
  d$data$SNUxIM <- data.frame(foo = c(NA), bar = c(NA))
  d$info$tool <- "Data Pack"
  d <- checkHasPSNUxIM(d)
  expect_false(d$info$has_psnuxim)
  expect_true(d$info$needs_psnuxim)
  expect_true(stringr::str_detect(d$info$messages$message, "Your Data Pack needs a new PSNUxIM tab."))
})

test_that("Can check PSNUxIM with data exists", {

  d <- list()
  d$info$messages <- MessageQueue()
  d$data$SNUxIM <- data.frame(foo = c(1, 2), bar = c(3, 4))
  d$info$tool <- "Data Pack"
  d <- checkHasPSNUxIM(d)
  expect_true(d$info$has_psnuxim)
  expect_true(is.null(d$info$needs_psnuxim))

})

test_that("Can extract OPU SNUxIM combos", {

  d <- list()
  d$info$tool <- "OPU Data Pack"
  d$info$messages <- MessageQueue()

  test_data  <- tibble::tribble(
    ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop,
    "Cupcake District [NYWv44mLzDN] ", "TX_CURR.T", "25-49", "F", NA,
    "Pizza District [iS02nBlAiva]", "TX_NEW.T", "<1", "M", NA
  )

  d$data$SNUxIM <- test_data

  d <- extractSNUxIMCombos(d)

  expect_named(
    d$data$PSNUxIM_combos,
    c("PSNU", "psnuid", "indicator_code", "Age", "Sex", "KeyPop"),
    ignore.order = TRUE
  )


})


test_that("Can extract Datapack SNUxIM combos", {

  d <- list()
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()

  test_data  <- tibble::tribble(
    ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop,
    "Cupcake District [NYWv44mLzDN]", "TX_CURR.T", "25-49", "F", NA,
    "Pizza District [iS02nBlAiva]", "TX_NEW.T", "<1", "M", NA
  )

  d$data$MER <- test_data %>%
    dplyr::mutate(psnuid = c("NYWv44mLzDN", "iS02nBlAiva"))
  d$data$SNUxIM <- test_data

  d <- extractSNUxIMCombos(d)

  expect_named(
    d$data$PSNUxIM_combos,
    c("PSNU", "psnuid", "indicator_code", "Age", "Sex", "KeyPop"),
    ignore.order = TRUE
  )

  expect_equal(NROW(d$data$missingCombos), 0L)
  expect_named(
    d$tests$missing_combos,
    c("PSNU", "psnuid", "indicator_code", "Age", "Sex", "KeyPop"),
    ignore.order = TRUE
  )

  expect_false(d$info$missing_psnuxim_combos)

})

test_that("Can extract missing Datapack SNUxIM combos", {

  d <- list()
  d$info$tool <- "Data Pack"
  d$info$messages <- MessageQueue()

  test_data  <- tibble::tribble(
    ~PSNU, ~indicator_code, ~Age, ~Sex, ~KeyPop,
    "Cupcake District [NYWv44mLzDN]", "TX_CURR.T", "25-49", "F", NA,
    "Pizza District [iS02nBlAiva]", "TX_NEW.T", "<1", "M", NA
  )
  d$data$SNUxIM <- test_data

  d$data$MER <- tibble::tribble(
    ~PSNU, ~psnuid, ~indicator_code, ~Age, ~Sex, ~KeyPop,
    "Cupcake District [NYWv44mLzDN]", "NYWv44mLzDN", "TX_CURR.T", "25-49", "F", NA,
    "Pizza District [iS02nBlAiva]", "iS02nBlAiva", "TX_NEW.T", "<1", "M", NA,
    "Kebab District [kKmyE7NrhZb]", "kKmyE7NrhZb", "TX_CURR.T", "15-19", "F", NA
  )


  d <- extractSNUxIMCombos(d)

  expect_named(
    d$data$PSNUxIM_combos,
    c("PSNU", "psnuid", "indicator_code", "Age", "Sex", "KeyPop"),
    ignore.order = TRUE
  )

  expect_equal(NROW(d$data$missingCombos), 1L)
  expect_named(
    d$tests$missing_combos,
    c("PSNU", "psnuid", "indicator_code", "Age", "Sex", "KeyPop"),
    ignore.order = TRUE
  )

  expect_identical(d$data$missingCombos, d$data$MER[3, ])

  expect_true(d$info$missing_psnuxim_combos)

})
