secrets <- "/home/jason/.secrets/datim.json"
loginToDATIM(secrets)

cop_year = getCurrentCOPYear()

fullCodeList <- pullFullCodeList(FY = cop_year +1) %>%
  dplyr::left_join(
    api_call("categoryOptionCombos") %>% api_fields("id, categoryOptions") %>% api_get(),
    by = c("categoryoptioncombouid" = "id")) %>%
  dplyr::mutate(categoryOptions = purrr::map_chr(categoryOptions,~.x[["id"]] %>% 
                                                   sort() %>% paste(collapse = ".")))

map_DataPack_DATIM_DEs_COCs <- datapackr::cop20_data_pack_schema %>%
  dplyr::filter(col_type == "target") %>%
  dplyr::select(indicator_code, dataelement_dsd, dataelement_ta,
                categoryoption_specified, valid_ages, valid_sexes, valid_kps) %>%
  tidyr::unnest(cols = valid_ages, names_sep  = ".") %>%
  tidyr::unnest(cols = valid_sexes, names_sep  = ".") %>%
  tidyr::unnest(cols = valid_kps, names_sep  = ".") %>%
  dplyr::mutate_at(c("valid_sexes.name","valid_ages.name","valid_ages.id","valid_sexes.id"),
                   ~dplyr::case_when(indicator_code == "OVC_HIVSTAT.N.total.T"
                                     ~ NA_character_,
                                     TRUE ~ .)) %>%
  dplyr::distinct() %>%
  dplyr::mutate(categoryoption_specified = stringr::str_split(categoryoption_specified, "[.]")) %>%
  dplyr::mutate(
    valid_kps.id =
      dplyr::case_when(
        (indicator_code %in% c("KP_MAT_SUBNAT.N.Sex.T", "KP_MAT.N.Sex.T") 
          & valid_kps.id == "G6OYSzplF5a") ~ "Z1EnpTPaUfq",
        (indicator_code %in% c("KP_MAT_SUBNAT.N.Sex.T", "KP_MAT.N.Sex.T") 
         & valid_kps.id == "wyeCT63FkXB") ~ "Qn0I5FbKQOA",
        TRUE ~ valid_kps.id),
    categoryOptions.ids =
      purrr::pmap(list(valid_ages.id,
                       valid_sexes.id,
                       valid_kps.id,
                       categoryoption_specified),
                  c)) %>% 
  dplyr::mutate(categoryOptions.ids = purrr::map(categoryOptions.ids, sort)) %>%
  dplyr::mutate(categoryOptions.ids = purrr::map(categoryOptions.ids, na.omit)) %>% 
  dplyr::mutate(categoryOptions.ids = purrr::map_chr(categoryOptions.ids, paste, collapse = ".")) %>%
  dplyr::mutate(
    categoryOptions.ids =
      dplyr::case_when(
        categoryOptions.ids == "" ~ "xYerKDKCefk",
        TRUE ~ categoryOptions.ids)) %>%
  tidyr::pivot_longer(cols = dataelement_dsd:dataelement_ta,
                      names_to = "support_type",
                      values_to = "dataelement",
                      names_prefix = "dataelement_",
                      values_drop_na = TRUE) %>%
  dplyr::left_join(getHTSModality(cop_year = cop_year),
                   by = c("dataelement" = "dataElement")) %>%
  dplyr::left_join(getTechArea(),
                   by = c("dataelement" = "dataElement")) %>%
  dplyr::left_join(getNumeratorDenominator(),
                   by = c("dataelement" = "dataElement")) %>%
  dplyr::mutate(support_type = toupper(support_type)) %>%
  dplyr::left_join(fullCodeList,
                   by = c("dataelement" = "dataelementuid",
                          "categoryOptions.ids" = "categoryOptions"))



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


hiv_specific<-getCOGSMap("bDWsPYyXgWP") %>% #HIV Test Status (Specific)
  purrr::pluck("dimension_map") %>%
  dplyr::select("categoryoptioncombouid"=coc_uid,
                "resultstatus"=category_option_group_name) %>%
  dplyr::mutate(resultstatus = stringr::str_replace(resultstatus,"\\(Specific\\)","")) %>%
  dplyr::mutate(resultstatus = stringr::str_replace(resultstatus,"HIV","")) %>%
  dplyr::mutate(resultstatus = stringr::str_trim(resultstatus))


hiv_inclusive<-getCOGSMap("ipBFu42t2sJ") %>% # HIV Test Status (Inclusive)
  purrr::pluck("dimension_map") %>%
  dplyr::select("categoryoptioncombouid"=coc_uid,
                "resultstatus_inclusive"=category_option_group_name) %>%
  dplyr::mutate(resultstatus_inclusive = stringr::str_replace(resultstatus_inclusive,"\\(Inclusive\\)","")) %>%
  dplyr::mutate(resultstatus_inclusive = stringr::str_replace(resultstatus_inclusive,"HIV","")) %>%
  dplyr::mutate(resultstatus_inclusive = stringr::str_replace(resultstatus_inclusive,"Status","")) %>%
  dplyr::mutate(resultstatus_inclusive = stringr::str_trim(resultstatus_inclusive))


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

#Valid for COP20
data_element_dims <-
  c("HWPJnUTMjEq",
    "LxhLO68FcXm",
    "gIBfzXabKkt")

degs_map <- purrr::map_dfr(data_element_dims, getDEGSMap) %>%
  tidyr::spread(type, name, fill = NA)
#Remapping of column names
from <- c(
  "dataElements",
  "Disaggregation.Type",
  "Technical.Area",
  "Top.Level..USE.ONLY.for.FY20.Results.FY21.Targets."
)

to <- c("dataelement",
        "disagg_type",
        "technical_area",
        "top_level")

names(degs_map) <- plyr::mapvalues(names(degs_map),from,to)

map_DataPack_DATIM_DEs_COCs %<>% 
  dplyr::left_join(hiv_specific,by="categoryoptioncombouid") %>% 
  dplyr::left_join(hiv_inclusive,by="categoryoptioncombouid") %>% 
  dplyr::left_join(degs_map,by="dataelement")



new <- map_DataPack_DATIM_DEs_COCs %>%
  dplyr::select(-categoryoption_specified)

compare_diffs <- datapackr::map_DataPack_DATIM_DEs_COCs %>%
  dplyr::select(-categoryoption_specified) %>%
  dplyr::full_join(new, by = c("indicator_code",
                               "valid_ages.name","valid_ages.id","valid_sexes.name",
                               "valid_sexes.id","valid_kps.name","valid_kps.id",
                               "categoryOptions.ids","support_type","hiv_specific","hiv_inclusive")) %>%
  dplyr::filter(is.na(dataelement.x) | is.na(dataelement.y.y))



  save(map_DataPack_DATIM_DEs_COCs, file = "./data/map_DataPack_DATIM_DEs_COCs.rda", compress = "xz")


