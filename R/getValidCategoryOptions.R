#' @export
#' @title getValidCategoryOptions
#'
#' @description
#' Returns full list of all valid categoryOptions for specified FY.
#'
#' @param cop_year Specifies COP year for dating as well as selection of
#' templates.
#'
#' @return Dataframe of all valid categoryOptions for specified FY
#'
getValidCategoryOptions <- function(cop_year = getCurrentCOPYear()) {

  ## Pull full Code List from DATIM ####
  fullCodeList <- datapackr::pullFullCodeList(FY = cop_year + 1,
                                              datastream = c("mer_targets",
                                                             "subnat_targets",
                                                             "impatt"))

  ## Map COCs from Code List to COs
  valid_COCs_COs <- map_COCs_to_COs() %>%
    dplyr::filter(id %in% unique(fullCodeList$categoryoptioncombouid))

  ## All valid COs for COP FY grouped by category ####
  pad <- function(digit) {
    padded <- paste0("0", digit)
  }

  five_year_age_names <- c("<01","01-04","05-09","10-14","15-19","20-24",
                           "25-29","30-34","35-39","40-44","45-49","50+")

  valid_category_options <- valid_COCs_COs %>%
    dplyr::select(categoryOptions) %>%
    tidyr::unnest(cols = categoryOptions) %>%
    dplyr::distinct() %>%
    dplyr::arrange(name) %>%
    dplyr::left_join(
      map_Cs_to_COs() %>% dplyr::select(-categoryoption),
      by = c("id" = "categoryoptionuid")) %>%
    dplyr::mutate(
      datapack_disagg = dplyr::case_when(
        categoryoptiongroup == "Age"
        ~ stringr::str_replace_all(name, "(?<!\\d)\\d(?!\\d)", pad),
        TRUE ~ name),

    ## five_year_ages ####
      datapack_schema_group = ifelse(
        test = categoryoptiongroup == "Age" & datapack_disagg %in% five_year_age_names,
        yes = "5yr",
        no =  ""),

    ## 1plus ####
      datapack_schema_group = ifelse(
        test = stringr::str_detect(datapack_schema_group,"5yr") & datapack_disagg != "<01",
        yes = paste(datapack_schema_group,"01+",sep = ","),
        no =  datapack_schema_group),

    ## <01 ####
      datapack_schema_group = ifelse(
        test = datapack_disagg == "<01",
        yes = paste(datapack_schema_group,"<01",sep = ","),
        no =  datapack_schema_group),

    ## 01-04 ####
      datapack_schema_group = ifelse(
        test = datapack_disagg == "01-04",
        yes = paste(datapack_schema_group,"01-04",sep = ","),
        no =  datapack_schema_group),

    ## 10plus ####
      datapack_schema_group = ifelse(
        test = stringr::str_detect(datapack_schema_group,"5yr")
          & (stringr::str_extract(datapack_disagg,"\\d\\d") %>% as.numeric()) >= 10,
        yes = paste(datapack_schema_group,"10+",sep = ","),
        no =  datapack_schema_group),

    ## 15plus ####
      datapack_schema_group = ifelse(
        test = stringr::str_detect(datapack_schema_group,"5yr")
          & (stringr::str_extract(datapack_disagg,"\\d\\d") %>% as.numeric()) >= 15,
        yes = paste(datapack_schema_group,"15+",sep = ","),
        no =  datapack_schema_group),

    ## 25-49 ####
      datapack_schema_group = ifelse(
        test = stringr::str_detect(datapack_schema_group,"5yr")
          & (stringr::str_extract(datapack_disagg,"\\d\\d") %>% as.numeric) >= 25
          & datapack_disagg != "50+",
        yes = paste(datapack_schema_group,"25-49",sep = ","),
        no =  datapack_schema_group),

    ## 10-29 ####
      datapack_schema_group = ifelse(
        test = stringr::str_detect(datapack_schema_group,"5yr")
        & (stringr::str_extract(datapack_disagg,"\\d\\d") %>% as.numeric) >= 10
        & (stringr::str_extract(datapack_disagg,"\\d\\d") %>% as.numeric) <= 29,
        yes = paste(datapack_schema_group,"10-29",sep = ","),
        no =  datapack_schema_group),

    ## ovcServAges ####
    datapack_schema_group = ifelse(
      test = categoryoptiongroup == "Age"
      & datapack_disagg %in% c(five_year_age_names[1:4],"15-17","18+","18-20"),
      yes = paste(datapack_schema_group,"ovc_serv",sep = ","),
      no =  datapack_schema_group),

    ## ovcHIVStatAges ####
      datapack_schema_group = ifelse(
        test = stringr::str_detect(datapack_schema_group, "ovc_serv")
          & !datapack_disagg %in% c(five_year_age_names[1:4],"15-17"),
        yes = paste(datapack_schema_group,"ovc_hiv_stat",sep = ","),
        no =  datapack_schema_group),



    ## coarseAges ####
      datapack_schema_group = ifelse(
        test = categoryoptiongroup == "Age" & datapack_disagg %in% c("<15","15+"),
        yes = paste(datapack_schema_group,"15s",sep = ","),
        no = datapack_schema_group),

    ## EID ####
      datapack_schema_group = ifelse(
        test = categoryoptiongroup == "Age"
          & datapack_disagg %in% c("<= 02 months","02 - 12 months"),
        yes = paste(datapack_schema_group,"eid",sep = ","),
        no = datapack_schema_group),

    ## coarseKPs ####
      datapack_schema_group = ifelse(
        test = categoryoptiongroup == "Key Population"
          & datapack_disagg %in% c("FSW","MSM","PWID","TG",
                                   "People in prisons and other enclosed settings"),
        yes = paste(datapack_schema_group,"coarseKPs",sep = ","),
        no = datapack_schema_group),

    # ## fineKPs ####
    #   datapack_schema_group = ifelse(
    #     test = categoryoptiongroup == "Key Population"
    #       & datapack_disagg %in% c("Female PWID","FSW","Male PWID","MSM not SW",
    #                               "MSM SW","TG not SW","TG SW",
    #                               "People in prisons and other enclosed settings"),
    #     yes = paste(datapack_schema_group,"fineKPs",sep = ","),
    #     no = datapack_schema_group),

    # ## pwidKPs ####
    #   datapack_schema_group = ifelse(
    #     test = categoryoptiongroup == "Key Population"
    #       & datapack_disagg %in% c("Female PWID","Male PWID"),
    #     yes = paste(datapack_schema_group,"pwidKPs",sep = ","),
    #     no = datapack_schema_group),

    ## M/F
      datapack_schema_group = ifelse(
        test = categoryoptiongroup == "Sex"
          & datapack_disagg %in% c("Female","Male"),
        yes = paste(datapack_schema_group,"M/F",sep = ","),
        no = datapack_schema_group),
      datapack_schema_group = stringr::str_replace(datapack_schema_group, "^,","")
    )

  return(valid_category_options)
}
