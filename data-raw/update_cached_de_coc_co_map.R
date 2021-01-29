datapackr::loginToDATIM("~/.secrets/datim.json")

cop_year = getCurrentCOPYear()

fullCodeList <- pullFullCodeList(FY = cop_year +1,
                                 datastream = c("mer_targets", "subnat_targets", "impatt")) %>%
  dplyr::left_join(
    api_call("categoryOptionCombos") %>% api_fields("id, categoryOptions") %>% api_get(),
    by = c("categoryoptioncombouid" = "id")) %>%
  dplyr::mutate(categoryOptions = purrr::map_chr(categoryOptions,~.x[["id"]] %>%
                                                   sort() %>% paste(collapse = ".")))

map_DataPack_DATIM_DEs_COCs <- datapackr::cop21_data_pack_schema %>%
  dplyr::filter(col_type == "target" & dataset %in% c("mer", "subnat", "impatt")) %>%
  dplyr::select(indicator_code, dataelement_dsd, dataelement_ta,
                categoryoption_specified, valid_ages, valid_sexes, valid_kps) %>%
  tidyr::unnest(cols = valid_ages, names_sep  = ".") %>%
  tidyr::unnest(cols = valid_sexes, names_sep  = ".") %>%
  tidyr::unnest(cols = valid_kps, names_sep  = ".") %>%
  dplyr::mutate_at(c("valid_sexes.name","valid_ages.name","valid_ages.id","valid_sexes.id"),
                   ~dplyr::case_when(indicator_code == "OVC_HIVSTAT.T"
                                     ~ NA_character_,
                                     TRUE ~ .)) %>%
  dplyr::distinct() %>%
  dplyr::mutate(categoryoption_specified = stringr::str_split(categoryoption_specified, "[.]"),
                dataelement_dsd = dplyr::case_when(
                  indicator_code %in% c("OVC_SERV.Active.T","OVC_SERV.Grad.T")
                    & valid_ages.id == "Q6xWcyHDq6e"
                  ~ stringr::str_extract(dataelement_dsd, "(?<=\\.)([A-Za-z][A-Za-z0-9]{10})$"),
                  indicator_code %in% c("OVC_SERV.Active.T","OVC_SERV.Grad.T")
                    & valid_ages.id != "Q6xWcyHDq6e"
                  ~ stringr::str_extract(dataelement_dsd, "^([A-Za-z][A-Za-z0-9]{10})(?=\\.)"),
                  TRUE ~ dataelement_dsd),
                dataelement_ta = dplyr::case_when(
                  indicator_code %in% c("OVC_SERV.Active.T","OVC_SERV.Grad.T")
                  & valid_ages.id == "Q6xWcyHDq6e"
                  ~ stringr::str_extract(dataelement_ta, "(?<=\\.)([A-Za-z][A-Za-z0-9]{10})$"),
                  indicator_code %in% c("OVC_SERV.Active.T","OVC_SERV.Grad.T")
                  & valid_ages.id != "Q6xWcyHDq6e"
                  ~ stringr::str_extract(dataelement_ta, "^([A-Za-z][A-Za-z0-9]{10})(?=\\.)"),
                  TRUE ~ dataelement_ta)
                ) %>%
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



getCOGSMap <- function(uid,
                       d2_session = dynGet("d2_default_session",
                                           inherits = TRUE)) {

  r <- paste0(d2_session$base_url,"api/categoryOptionGroupSets/",uid,
            "?fields=id,name,categoryOptionGroups[id,name,categoryOptions[id,name,categoryOptionCombos[id,name]]") %>%
    URLencode(.) %>%
    httr::GET(., httr::timeout(180), handle = d2_session$handle) %>%
    httr::content(.,"text") %>%
    jsonlite::fromJSON(.,flatten = TRUE)

  dim_name<- r$name
  dim_id<- r$id

  cogs <- r %>% purrr::pluck(.,"categoryOptionGroups") %>% dplyr::select(id,name)

  cogs_cocs_map<-list()

  for (i in 1:NROW(cogs) ) {

    cos_cocs <- r %>%
      purrr::pluck(.,"categoryOptionGroups") %>%
      purrr::pluck(.,"categoryOptions") %>%
      purrr::pluck(., i) %>%
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


hiv_specific <- getCOGSMap("bDWsPYyXgWP") %>% #HIV Test Status (Specific)
  purrr::pluck("dimension_map") %>%
  dplyr::select("categoryoptioncombouid"=coc_uid,
                "resultstatus"=category_option_group_name) %>%
  dplyr::mutate(resultstatus = stringr::str_replace(resultstatus,"\\(Specific\\)","")) %>%
  dplyr::mutate(resultstatus = stringr::str_replace(resultstatus,"HIV","")) %>%
  dplyr::mutate(resultstatus = stringr::str_trim(resultstatus))


hiv_inclusive <- getCOGSMap("ipBFu42t2sJ") %>% # HIV Test Status (Inclusive)
  purrr::pluck("dimension_map") %>%
  dplyr::select("categoryoptioncombouid"=coc_uid,
                "resultstatus_inclusive"=category_option_group_name) %>%
  dplyr::mutate(resultstatus_inclusive = stringr::str_replace(resultstatus_inclusive,"\\(Inclusive\\)","")) %>%
  dplyr::mutate(resultstatus_inclusive = stringr::str_replace(resultstatus_inclusive,"HIV","")) %>%
  dplyr::mutate(resultstatus_inclusive = stringr::str_replace(resultstatus_inclusive,"Status","")) %>%
  dplyr::mutate(resultstatus_inclusive = stringr::str_trim(resultstatus_inclusive))


getDEGSMap <- function(uid,
                       d2_session = dynGet("d2_default_session",
                                           inherits = TRUE)) {

  r <- paste0(d2_session$base_url,
              "api/dataElementGroupSets/",
              uid,
              "?fields=id,name,dataElementGroups[name,dataElements[id]]&paging=false") %>%
    URLencode(.) %>%
    httr::GET(., httr::timeout(180), handle = d2_session$handle) %>%
    httr::content(.,"text") %>%
    jsonlite::fromJSON(.,flatten = TRUE)

  r %>%
    purrr::pluck(.,"dataElementGroups") %>%
    dplyr::mutate_if(is.list, purrr::simplify_all) %>%
    tidyr::unnest(cols = c(dataElements)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(type=make.names(r$name))

}

#Valid for COP21
data_element_dims <-
  c("HWPJnUTMjEq",
    "LxhLO68FcXm",
    "NLZgRe4FuQJ")

degs_map <- purrr::map_dfr(data_element_dims, getDEGSMap) %>%
  tidyr::spread(type, name, fill = NA)
#Remapping of column names
from <- c(
  "dataElements",
  "Disaggregation.Type",
  "Technical.Area",
  "Top.Level..USE.ONLY.for.FY21.Results.FY22.Targets."
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
                               "categoryOptions.ids","support_type","resultstatus","resultstatus_inclusive")) %>%
  dplyr::filter(is.na(dataelement.x) | is.na(dataelement.y.y))



  save(map_DataPack_DATIM_DEs_COCs, file = "./data/map_DataPack_DATIM_DEs_COCs.rda", compress = "xz")
