

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

# login object stubs sufficient for use in mocked api calls
# one needed for each server with mock calls
play2361 <- list(base_url = "https://play.dhis2.org/2.36.1/",
                 handle = httr::handle("https://play.dhis2.org/2.36.1/"))

training <- list(base_url = "https://training.datim.org/",
                 handle = httr::handle("https://training.datim.org/"))

cop21_datapack_template_path <- 
  system.file("extdata", "COP21_Data_Pack_Template.xlsx", package = "datapackr")

cop20_datapack_template_path <- 
  system.file("extdata", "COP20_Data_Pack_Template_vFINAL.xlsx", package = "datapackr")

cop20_opu_datapack_template_path <- 
  system.file("extdata", "COP20_OPU_Data_Pack_Template.xlsx", package = "datapackr")
