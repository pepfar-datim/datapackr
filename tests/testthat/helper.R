library(datimutils)
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

getCOGSMap <-function(uid, d2_session = parent.frame()$d2_default_session) {

     r <-  datimutils::getCatOptionGroupSets(values = uid,
                                 by = "id",
                                 fields = "id,name,categoryOptionGroups[id,name,categoryOptions[id,name,categoryOptionCombos[id,name]]",
                                          d2_session = d2_session
   )

  cogs <- r %>% purrr::pluck(.,"categoryOptionGroups")
  cogs <- cogs[[1]]
  categoryOptions <- cogs[["categoryOptions"]]
  cogs <- dplyr::select(cogs, id,name)

  cogs_cocs_map<-list()

  for (i in 1:NROW(cogs) ) {

    cos_cocs <- categoryOptions[[i]] %>%
    purrr::pluck(.,"categoryOptionCombos") %>%
      do.call(rbind.data.frame,.) %>%
      dplyr::distinct() %>%
      dplyr::select("category_option_combo"=name,"coc_uid"=id)

    cos_cocs$category_option_group_name<-cogs[i,"name"]
    cos_cocs$category_option_group_uid<-cogs[i,"id"]
    cogs_cocs_map<-rlist::list.append(cogs_cocs_map,cos_cocs)
  }

  cogs_cocs_map %<>% do.call(rbind.data.frame,.)

  return(list(dimension_name=r$name,
              dimension_id=r$id,
              dimension_map= cogs_cocs_map))

}