test_sheet <-
  function(fname)
    testthat::test_path("sheets", fname)
#rprojroot::find_package_root_file("inst/extdata",fname)


detach_all_instances <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}


cleanup <- function(){
  file_list<-list.files("~/datapackr/datapackr_test_files/", full.names = TRUE)
  file_list <- file_list[grepl(".xlsx|.csv", file_list)]
  do.call(file.remove, list(file_list))
}
