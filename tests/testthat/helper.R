
test_sheet <-
  function(fname)
    testthat::test_path("sheets", fname)
    #rprojroot::find_package_root_file("inst/extdata",fname)

datim <- list(base_url = "https://www.datim.org/",
                handle = httr::handle("https://www.datim.org/"))


getDEGSMap <- function(uid, d2_session = parent.frame()$d2_default_session) {

   r <-  datimutils::getDataElementGroupSets(values = uid,
                                 by = "id",
                                 fields = "id,name,dataElementGroups[name,dataElements[id]]",
                                          d2_session = d2_session
   )

  name <- r[["name"]]
  r <- r[["dataElementGroups"]][[1]]
  r <- tidyr::unnest(r, cols = c(dataElements))
  r <- dplyr::distinct(r)
  r$type <- name
  colnames(r)[colnames(r) == 'id'] <- 'dataElements'

  return(r)
}