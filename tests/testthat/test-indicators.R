
test_that("Can calculate an indicator with totals", {
  ind_uid <- "abc123"
  de1 <- "aFp9qn4JW5y"
  de2 <- "KvzDWWFtHFy"
  coc1 <- "TQPl2RZmz5b"
  coc2 <- "KLH5fjVMgKG"

  numerator_formula <- paste0("#{", de1, "}")
  denominator_formula <- paste0("#{", de1, "}", " + #{", de2, "}")

  inds <- data.frame(id = ind_uid,
                     name = "Test Indicator 1",
                     numerator = numerator_formula,
                     denominator = denominator_formula)

  combis <- c(paste0("#{", de1, ".", coc1, "}"), paste0("#{", de2, ".", coc2, "}"))
  values <- c(10, 20)

  test_values <- evaluateIndicators(combis, values, inds)
  expect_equal(class(test_values), "data.frame")
  expect_setequal(names(test_values), c("id", "name", "numerator", "denominator", "value"))
  expect_equal(NROW(test_values), 1)
  expect_equal(test_values$value, 0.33333, tolerance = 0.0001)
  expect_equal(test_values$numerator, 10)
  expect_equal(test_values$denominator, 30)
}


)




test_that("Can calculate an indicator with missing terms", {
  ind_uid <- "abc123"
  de1 <- "aFp9qn4JW5y"
  coc1 <- "TQPl2RZmz5b"
  coc2 <- "KLH5fjVMgKG"

  numerator_formula <- paste0(paste0("#{", de1, ".", coc1, "}"), "+", paste0("#{", de1, ".", coc2, "}"))

  inds <- data.frame(id = ind_uid,
                     name = "Test Indicator 1",
                     numerator = numerator_formula,
                     denominator = "1")

  combis <- c(paste0("#{", de1, ".", coc1, "}"))
  values <- c(10)

  test_values <- evaluateIndicators(combis, values, inds)
  expect_equal(class(test_values), "data.frame")
  expect_setequal(names(test_values), c("id", "name", "numerator", "denominator", "value"))
  expect_equal(NROW(test_values), 1)
  expect_equal(test_values$value, 10)
  expect_equal(test_values$numerator, 10)
  expect_equal(test_values$denominator, 1)
}


)


test_that("Can calculate a simple indicator", {
            ind_uid <- "abc123"
            de1 <- "aFp9qn4JW5y"
            coc1 <- "TQPl2RZmz5b"
            coc2 <- "KLH5fjVMgKG"

            numerator_formula <- paste0(paste0("#{", de1, ".", coc1, "}"), "+", paste0("#{", de1, ".", coc2, "}"))

            inds <- data.frame(id = ind_uid,
                             name = "Test Indicator 1",
                             numerator = numerator_formula,
                             denominator = "1")

            combis <- c(paste0("#{", de1, ".", coc1, "}"), c(paste0("#{", de1, ".", coc2, "}")))
            values <- c(10, 20)

            test_values <- evaluateIndicators(combis, values, inds)
            expect_equal(class(test_values), "data.frame")
            expect_setequal(names(test_values), c("id", "name", "numerator", "denominator", "value"))
            expect_equal(NROW(test_values), 1)
            expect_equal(test_values$value, 30)
            expect_equal(test_values$numerator, 30)
            expect_equal(test_values$denominator, 1)
          }


          )


test_that("Handle empty indicator calculations", {

            ind_uid <- "RGCW29Xx7ak"
            de1 <- "aFp9qn4JW5y"
            de2 <- "jDoke4i51J8"
            coc1 <- "TQPl2RZmz5b"
            coc2 <- "KLH5fjVMgKG"

            numerator_formula <- paste0(paste0("#{", de1, ".", coc1, "}"), "+", paste0("#{", de1, ".", coc2, "}"))

            inds <- data.frame(id = ind_uid,
                             name = "Test Indicator 1",
                             numerator = numerator_formula,
                             denominator = "1")

            combis <- c(paste0("#{", de2, ".", coc1, "}"), c(paste0("#{", de2, ".", coc2, "}")))
            values <- c(10, 20)

            test_values <- evaluateIndicators(combis, values, inds)
            expect_equal(class(test_values), "data.frame")
            expect_setequal(names(test_values), c("id", "name", "numerator", "denominator", "value"))
            expect_equal(NROW(test_values), 0)
          }
)
