
is_uidish<-function(x) {
  grepl("^[[:alpha:]][[:alnum:]]{10}$",x)
}



test_sheet <-
  function(fname)
    testthat::test_path("sheets", fname)
    #rprojroot::find_package_root_file("inst/extdata",fname)

library(httptest)
library(tibble)
test_config <- function(fname) rprojroot::find_testthat_root_file("config", fname)
