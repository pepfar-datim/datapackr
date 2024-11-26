#' @export
#'
#' @importFrom stats na.omit
#' @title Generate a map from Data Pack indicators to Data Pack DEs, COCs, & COs
#'
#' @description For a given COP Year, produces a dataframe mapping from Data Pack
#' indicators to DATIM dataElements, categoryOptionCombos, & categoryOptions.
#'
#' @inheritParams datapackr_params
#'
#' @return dp_map
#'
update_de_coc_co_map <- function(cop_year = NULL,
                                 d2_session = dynGet("d2_default_session",
                                                     inherits = TRUE)) {

  # This script begins with the Full Code List from DATIM, then combines categoryOption
  # metadata, then combines this with the Data Pack schema to create a full map between
  # Data Packs and DATIM for the purpose of generating import and analytics tables.

  # For new `Year 2` data, this file assumes:
  #
  # - All positives within modalities have already been multiplied against total
  #   positives to make integers.
  #
  # - EID is already split into separate rows for each of <2 & 2-12 months
  #
  # - EID & KP indicator_codes first use their overarching Age/Sex indicator code
  #   equivalents, then are suffixed with `.EID.2`, `.EID.12`, or `.KP`.
  #
  # - OVC_SERV have already been tagged with correct dataElements (Caregivers, vs. OVC)
  #
  # - OVC_HIVSTAT has already been aggregated to have no age/sex disaggregation.

  # Check params ####
  params <- check_params(
    cop_year = cop_year)

  for (p in names(params)) {
    assign(p, purrr::pluck(params, p))
  }

  rm(params, p)

  # List all datasets and correct FY mapping (not always captured in DATIM) ####
  datasets_to_pull <- tibble::tribble(
    ~dataset_uid, ~dataset_name, ~FY, ~targets_results, ~datastream, ~org_unit,
    "dA9C5bL44NX", "FY24 MER Targets", 2024, "targets", "mer", "psnu",
    "lHUEzkjkij1", "FY25 MER Targets", 2025, "targets", "mer", "psnu",
    "NNDYxUGDHa9", "FY26 MER Targets", 2026, "targets", "mer", "psnu",

    "vpDd67HlZcT", "FY24 DREAMS Targets", 2024, "targets", "dreams", "dsnu",
    "tNbhYbrKbnk", "FY25 DREAMS Targets", 2025, "targets", "dreams", "dsnu",
    "jpn49OHYT7e", "FY26 DREAMS Targets", 2025, "targets", "dreams", "dsnu",

    "WA0oJsUDw9U", "FY26 IMPATT", 2026, "targets", "impatt", "psnu",
    "jgp20ElKCMD", "FY25 IMPATT", 2025, "targets", "impatt", "psnu",
    "kWKJQYP1uT7", "FY24 IMPATT", 2024, "targets", "impatt", "psnu",
    # Fri Jul 26 15:42:02 2024 -- Touch base with Christian, but believe the belwo is COP23 specific
    # For all FY23 SUBNAT/IMPATT, remap to FY24 disaggs, as these won't go to
    #   DATIM, but must go to PAW alongside FY24.
    "kWKJQYP1uT7", "FY23 IMPATT", 2023, "targets", "impatt", "psnu",

    "BYwMgcnR0ZD", "FY26 SUBNAT Targets", 2026, "targets", "subnat", "psnu",
    "CMJtVW4ecLn", "FY25 SUBNAT Targets", 2025, "targets", "subnat", "psnu",
    "bKSmkDP5YTc", "FY24 SUBNAT Targets", 2024, "targets", "subnat", "psnu",
    "bKSmkDP5YTc", "FY23 SUBNAT Targets", 2023, "targets", "subnat", "psnu",
  )

  # Test that all dataSet uids are valid ----
  dataSetUIDs <- datimutils::getMetadata(dataSets, d2_session = d2_default_session)

  if (!all(datasets_to_pull$dataset_uid %in% dataSetUIDs$id)) {
    invalid_ds_uids <-
      datasets_to_pull$dataset_uid[!datasets_to_pull$dataset_uid %in% dataSetUIDs$id]

    interactive_warning(
      paste0(
        "The following dataSet UIDs could not be found and will be excluded: ",
        paste(invalid_ds_uids, collapse = ", ")))

    datasets_to_pull <-
      datasets_to_pull[datasets_to_pull$dataset_uid %in% dataSetUIDs$id, ]
  }

  rm(dataSetUIDs)

  # Compile all DATIM code lists ----
  fullCodeList <-
    purrr::map_dfr(
      unique(datasets_to_pull$dataset_uid),
      function(x) {
        cl <- datimutils::getSqlView(sql_view_uid = "DotdxKrNZxG",
                                     variable_keys = "dataSets",
                                     variable_values = x) %>%
          dplyr::mutate(dataset_uid = x)
      }) %>%
    dplyr::left_join(datasets_to_pull, by = c("dataset_uid" = "dataset_uid")) %>%
    dplyr::select(-dataset, -dataset_uid, -dataset_name) %>%
    dplyr::distinct()

  rm(datasets_to_pull)

  ## Add DATIM periods & FYs to code lists ####
  fullCodeList %<>%
    dplyr::mutate(
      period_dataset =
        paste0(
          "FY", FY - 2000, " ", toupper(datastream),
          dplyr::if_else(datastream == "impatt", "",
                         paste0(" ", stringr::str_to_title(targets_results)))),
      period = dplyr::case_when(
        targets_results == "targets" ~ paste0(FY - 1, "Oct"),
        targets_results == "results" ~ paste0(FY, "Q3")))

  ## Add metadata for categoryOptions ####
  categoryoptions <-
    datimutils::getMetadata(
      end_point = "categoryOptionCombos",
      "categoryCombo.id:!eq:wUpfppgjEza",
      fields = "id, categoryOptions")

  fullCodeList %<>%
    dplyr::left_join(categoryoptions, by = c("categoryoptioncombouid" = "id")) %>%
    dplyr::mutate(categoryOptions = purrr::map_chr(categoryOptions, ~.x[["id"]] %>%
                                                     sort() %>%
                                                     paste(collapse = ".")))

  ## Standardize some column names ####
  fullCodeList %<>%
    dplyr::rename(
      dataset = datastream,
      dataelementname = dataelement,
      categoryOptions.ids = categoryOptions,
      categoryoptioncomboname = categoryoptioncombo)

  # Thu Feb  1 14:47:16 2024 ------------------------------
  # DP-1195 PATCH
  # P2XNbiNnIqV was not included in forms
  #Setup dataframe to append back to fullCodeList
  Patch <- fullCodeList[fullCodeList$dataelementuid == "P2XNbiNnIqV" &
                         fullCodeList$period == "2024Oct", ]
  Patch$FY <- 2024
  Patch$period <- "2023Oct"
  Patch$period_dataset <- "FY24 SUBNAT Targets"

  #Append back to full code list
  fullCodeList <- rbind(fullCodeList, Patch)


  # Prep Data Pack schema for mapping ####
  dp_map <- pick_schema(cop_year, "Data Pack") %>%
    dplyr::filter((col_type == "target" & dataset %in% c("mer", "subnat", "impatt"))
                  | dataset == "subnat" & col_type == "result",
                  !is.na(FY)) %>%
    dplyr::select(sheet_name, indicator_code, dataset, col_type, value_type,
                  dataelement_dsd, dataelement_ta,
                  categoryoption_specified, valid_ages, valid_sexes, valid_kps,
                  FY, period)

  # Prep Year 2 data ----
  Year2 <- dp_map %>%
    dplyr::filter(sheet_name == "Year 2") %>%

    ## Handle unique KP & EID data ----
    # KP
    tidyr::separate_wider_regex(
      dataelement_dsd,
      c(dataelement_dsd = ".*",
        "\\{KP\\}",
        dataelement_dsd_KP = ".*"),
      too_few = "align_start") %>%
    tidyr::separate_wider_regex(
      categoryoption_specified,
      c(categoryoption_specified = ".*",
        "\\{KP\\}",
        categoryoption_specified_KP = ".*"),
      too_few = "align_start") %>%

    # EID
    tidyr::separate_wider_regex(
      dataelement_dsd,
      c(dataelement_dsd = ".*",
        "\\{EID\\}",
        dataelement_dsd_EID = ".*"),
      too_few = "align_start") %>%
    tidyr::separate_wider_regex(
      categoryoption_specified,
      c(categoryoption_specified = ".*",
        "\\{EID\\}",
        categoryoption_specified_EID = ".*"),
      too_few = "align_start")

  # Remove empty string written where nothing left after removing KP & EID
  Year2$dataelement_dsd[Year2$dataelement_dsd == ""] <- NA
  Year2$categoryoption_specified[Year2$categoryoption_specified == ""] <- NA

  # Split KP, Age/Sex, EID into separate rows
  empty <- list(tibble::tribble(
    ~name, ~id,
    NA_character_, NA_character_))

  ## KP
  Year2_KP <- Year2 %>%
    dplyr::filter(!is.na(dataelement_dsd_KP)) %>%
    dplyr::select(-dataelement_dsd, -dataelement_dsd_EID,
                  -categoryoption_specified, -categoryoption_specified_EID) %>%
    dplyr::rename(dataelement_dsd = dataelement_dsd_KP,
                  categoryoption_specified = categoryoption_specified_KP) %>%
    dplyr::mutate(valid_ages = empty,
                  valid_sexes = empty,
                  indicator_code = paste0(indicator_code, ".KP"))

  ## EID
  Year2_EID <- Year2 %>%
    dplyr::filter(!is.na(dataelement_dsd_EID)) %>%
    dplyr::select(-dataelement_dsd, -dataelement_dsd_KP,
                  -categoryoption_specified, -categoryoption_specified_KP) %>%
    dplyr::rename(dataelement_dsd = dataelement_dsd_EID,
                  categoryoption_specified = categoryoption_specified_EID) %>%
    dplyr::mutate(valid_ages = empty,
                  valid_sexes = empty,
                  valid_kps = empty,
                  indicator_code = paste0(indicator_code, ".EID")) %>%
    tidyr::separate_longer_delim(categoryoption_specified, "/") %>%
    dplyr::mutate(
      indicator_code =
        dplyr::case_when(
          categoryoption_specified == "J4SQd7SnDi2.fELDwOVs9TV" ~ paste0(indicator_code, ".2"),
          categoryoption_specified == "pbXCUjm50XK.fELDwOVs9TV" ~ paste0(indicator_code, ".12"),
          TRUE ~ categoryoption_specified))

  ## Recombine
  Year2 %<>%
    dplyr::select(-dataelement_dsd_EID, -dataelement_dsd_KP,
                  -categoryoption_specified_EID,
                  -categoryoption_specified_KP) %>%
    dplyr::filter(!is.na(dataelement_dsd)) %>%
    dplyr::mutate(
      valid_kps = dplyr::case_when(
        indicator_code == "KP_PREV.T2" ~ valid_kps,
        TRUE ~ empty)) %>%
    dplyr::bind_rows(Year2_KP, Year2_EID)

  dp_map %<>%
    dplyr::filter(sheet_name != "Year 2") %>%
    dplyr::bind_rows(Year2)


  ## Unfold Age, Sex, KP disaggs ----
  dp_map %<>%
    tidyr::unnest(cols = valid_ages, names_sep  = ".") %>%
    tidyr::unnest(cols = valid_sexes, names_sep  = ".") %>%
    tidyr::unnest(cols = valid_kps, names_sep  = ".") %>%
    dplyr::distinct()

  ## Correctly tag OVC_SERV 18+ Caregivers ####
  dp_map %<>%
    dplyr::mutate(
      dataelement_dsd =
        dplyr::case_when(
          indicator_code %in% c("OVC_SERV.Active.T", "OVC_SERV.Grad.T",
                                "OVC_SERV.Active.T2", "OVC_SERV.Grad.T2")
          & valid_ages.name == "18+"
          ~ stringr::str_extract(dataelement_dsd, "(?<=\\.)([A-Za-z][A-Za-z0-9]{10})$"),
          indicator_code %in% c("OVC_SERV.Active.T", "OVC_SERV.Grad.T",
                                "OVC_SERV.Active.T2", "OVC_SERV.Grad.T2")
          & valid_ages.name != "18+"
          ~ stringr::str_extract(dataelement_dsd, "^([A-Za-z][A-Za-z0-9]{10})(?=\\.)"),
          TRUE ~ dataelement_dsd),
      dataelement_ta = dplyr::case_when(
        indicator_code %in% c("OVC_SERV.Active.T", "OVC_SERV.Grad.T")
        & valid_ages.name == "18+"
        ~ stringr::str_extract(dataelement_ta, "(?<=\\.)([A-Za-z][A-Za-z0-9]{10})$"),
        indicator_code %in% c("OVC_SERV.Active.T", "OVC_SERV.Grad.T")
        & valid_ages.name != "18+"
        ~ stringr::str_extract(dataelement_ta, "^([A-Za-z][A-Za-z0-9]{10})(?=\\.)"),
        TRUE ~ dataelement_ta),
      categoryoption_specified = stringr::str_split(categoryoption_specified, "[.]"))

  ## Allow aggregation of OVC_HIVSTAT ####
  dp_map %<>%
    dplyr::mutate_at(
      c("valid_ages.name", "valid_ages.id", "valid_sexes.name", "valid_sexes.id"),
      ~dplyr::case_when(indicator_code %in% c("OVC_HIVSTAT.T", "OVC_HIVSTAT.T2")
                        ~ NA_character_,
                        TRUE ~ .)) %>%
    dplyr::distinct()

  ## Combine all categoryOptions into a joinable list. ####
  dp_map %<>%
    dplyr::mutate(
      categoryOptions.ids =
        purrr::pmap(list(valid_ages.id,
                         valid_sexes.id,
                         valid_kps.id,
                         categoryoption_specified),
                    c)) %>%
    dplyr::mutate(categoryOptions.ids = purrr::map(categoryOptions.ids, sort)) %>%
    dplyr::mutate(categoryOptions.ids = purrr::map(categoryOptions.ids, na.omit)) %>%
    dplyr::mutate(categoryOptions.ids = purrr::map_chr(categoryOptions.ids, paste, collapse = ".")) %>%

    ## Apply default categoryOption against dataElements with no assigned COs. ####
  dplyr::mutate(
    categoryOptions.ids =
      dplyr::case_when(
        categoryOptions.ids == "" ~ "xYerKDKCefk",
        TRUE ~ categoryOptions.ids))

  ## Stack DSD and TA ####
  dp_map %<>%
    tidyr::pivot_longer(cols = dataelement_dsd:dataelement_ta,
                        names_to = "support_type",
                        values_to = "dataelementuid",
                        names_prefix = "dataelement_",
                        values_drop_na = TRUE) %>%
    dplyr::mutate(support_type = toupper(support_type)) %>%
    dplyr::mutate(
      support_type = dplyr::case_when(
        stringr::str_detect(indicator_code, "^AGYW_PREV") ~ "No Support Type",
        dataset %in% c("impatt", "subnat") ~ "Sub-National",
        TRUE ~ support_type))

  # Join Full Code List with Schema ####
  # Mon Nov 25 15:20:51 2024 ------------------------------
  # FY and Period do not match for TbG5gCQH78Q data element for HTS_TST.Pos.T
  # I THINK WE NEED A FY26 dataset
  dp_map %<>%
    dplyr::select(-dataset) %>%
    dplyr::inner_join(fullCodeList,
                      by = c("dataelementuid" = "dataelementuid",
                             "categoryOptions.ids" = "categoryOptions.ids",
                             "period" = "period",
                             "FY" = "FY"))

  # Readjust PMTCT SUBNAT
  # dp_map %<>%
  #   dplyr::mutate(
  #     FY = dplyr::case_when(
  #       stringr::str_detect(indicator_code, "PMTCT_(.*)_SUBNAT(.*)\\.T_1$") ~ FY-1,
  #       TRUE ~ FY),
  #     period = dplyr::case_when(
  #       stringr::str_detect(indicator_code, "PMTCT_(.*)_SUBNAT(.*)\\.T_1$") ~ paste0(FY-1,"Oct"),
  #       TRUE ~ period),
  #     period_dataset = dplyr::case_when(
  #       stringr::str_detect(indicator_code, "PMTCT_(.*)_SUBNAT(.*)\\.T_1$") ~
  #         stringr::str_replace(period_dataset, "(?<=FY)\\d{2}", stringr::str_sub(FY,-2,-1)),
  #       TRUE ~ period_dataset))
  #

  # Add additional metadata for use in analytics ####
  dp_map %<>%
    dplyr::left_join(getHTSModality(cop_year = cop_year),
                     by = c("dataelementuid" = "dataElement"))

  getCOGSMap <- function(uid,
                         d2_session = dynGet("d2_default_session",
                                             inherits = TRUE)) {

    r <-
      datimutils::getCatOptionGroupSets(
        values = uid,
        by = "id",
        fields = "categoryOptionGroups[id,name,categoryOptions[categoryOptionCombos[id,name]]]",
        d2_session = d2_session) %>%
      dplyr::rename(categoryOptionGroupSets.name = name,
                    categoryOptionGroupSets.id = id) %>%
      tidyr::unnest(categoryOptionGroups) %>%
      dplyr::rename(categoryOptionGroups.name = name,
                    categoryOptionGroups.id = id) %>%
      tidyr::unnest(categoryOptions) %>%
      tidyr::unnest(categoryOptionCombos) %>%
      dplyr::rename(categoryOptionCombos.name = name,
                    categoryOptionCombos.id = id)

    return(r)
  }


  getHIVSpecific <- function(d2_session = dynGet("d2_default_session",
                                                 inherits = TRUE)) {

    hiv_specific <- getCOGSMap("bDWsPYyXgWP",
                               d2_session = d2_session) %>% #HIV Test Status (Specific)
      dplyr::select("categoryoptioncombouid" = categoryOptionCombos.id,
                    "resultstatus" = categoryOptionGroups.name) %>%
      dplyr::mutate(
        resultstatus = stringr::str_replace(resultstatus, "\\(Specific\\)", ""),
        resultstatus = stringr::str_replace(resultstatus, "HIV", ""),
        resultstatus = stringr::str_trim(resultstatus))

    return(hiv_specific)

  }

  getHIVInclusive <- function(d2_session = dynGet("d2_default_session",
                                                  inherits = TRUE)) {

    hiv_inclusive <- getCOGSMap("ipBFu42t2sJ",
                                d2_session = d2_session) %>% # HIV Test Status (Inclusive)
      dplyr::select("categoryoptioncombouid" = categoryOptionCombos.id,
                    "resultstatus_inclusive" = categoryOptionGroups.name) %>%
      dplyr::mutate(
        resultstatus_inclusive = stringr::str_replace(resultstatus_inclusive, "\\(Inclusive\\)", ""),
        resultstatus_inclusive = stringr::str_replace(resultstatus_inclusive, "HIV", ""),
        resultstatus_inclusive = stringr::str_replace(resultstatus_inclusive, "Status", ""),
        resultstatus_inclusive = stringr::str_trim(resultstatus_inclusive))

    return(hiv_inclusive)
  }


  getDEGSMap <- function(uid,
                         d2_session = dynGet("d2_default_session",
                                             inherits = TRUE)) {

    r <- datimutils::getDataElementGroupSets(values = uid,
                                             by = "id",
                                             fields = "id,name,dataElementGroups[name,id,dataElements[name,id]]",
                                             d2_session = d2_session) %>%
      dplyr::rename(dataElementGroupSets.name = name,
                    dataElementGroupSets.id = id) %>%
      tidyr::unnest(dataElementGroups) %>%
      dplyr::rename(dataElementGroups.name = name,
                    dataElementGroups.id = id) %>%
      tidyr::unnest(dataElements) %>%
      dplyr::rename(dataElements.name = name,
                    dataElements.id = id) %>%
      dplyr::distinct()

    return(r)

  }

  degs_map <- getDEGSMap(c("HWPJnUTMjEq",
                           "LxhLO68FcXm",
                           "RUkVjD3BsS1",
                           "TWXpUVE2MqL",
                           "lD2x0c8kywj")) %>%
    dplyr::select(dataElementGroupSets.name, dataElementGroupSets.id,
                  dataElementGroups.name, dataelementuid = dataElements.id) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      dataElementGroupSets.name =
        dplyr::case_when(
          dataElementGroupSets.id == "HWPJnUTMjEq" ~ "disagg_type",
          dataElementGroupSets.id == "LxhLO68FcXm" ~ "technical_area",
          dataElementGroupSets.id == "RUkVjD3BsS1" ~ "top_level",
          dataElementGroupSets.id == "TWXpUVE2MqL" ~ "support_type",
          dataElementGroupSets.id == "lD2x0c8kywj" ~ "numerator_denominator",
          TRUE ~ dataElementGroupSets.name
        )
    ) %>%
    dplyr::select(-dataElementGroupSets.id) %>%
    tidyr::pivot_wider(names_from = dataElementGroupSets.name,
                       values_from = dataElementGroups.name,
                       values_fill = NA) %>%
    datapackr::addcols(c("disagg_type", "technical_area", "top_level",
                         "support_type", "numerator_denominator"),
                       type = "character")

  dp_map %<>%
    dplyr::left_join(getHIVSpecific(), by = "categoryoptioncombouid") %>%
    dplyr::left_join(getHIVInclusive(), by = "categoryoptioncombouid") %>%
    dplyr::rename(support_type_dp = support_type) %>%
    dplyr::left_join(degs_map, by = "dataelementuid") %>%
    dplyr::mutate(
      support_type = dplyr::if_else(is.na(support_type), support_type_dp, support_type)
    ) %>%
    dplyr::select(-support_type_dp)

  # Get columns all in order ####
  dp_map %<>%
    dplyr::select(
      indicator_code, col_type, value_type, categoryoption_specified,
      valid_ages.name, valid_ages.id, valid_sexes.name, valid_sexes.id,
      valid_kps.name, valid_kps.id, FY, period, categoryOptions.ids,
      dataelementuid, hts_modality, period_dataset, dataelementname,
      categoryoptioncomboname, categoryoptioncombouid, targets_results,
      dataset, resultstatus, resultstatus_inclusive, disagg_type, technical_area,
      top_level, support_type, numerator_denominator
    )

  return(dp_map)
}
