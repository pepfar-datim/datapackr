#Populate necessary metadata for the memo
#' Title
#'
#' @param d Datapackr d object
#' @param d2_session Datimutils d2_session object
#' @param memo_type One of the following:
#' datapack: Create the memo based on the data in the datapack or OPU datapack
#' datim: Create the memo based on data currently in DATIM
#' comparison: Create a comparison memo with data from both DATIM and the datapack
#'
#' @return Datapackr d object
#' @export
#'
prepareMemoMetadata <- function(d, d2_session, memo_type) {

  #This is still not sensitive to the COP year
  #TODO: maybe valid_PSNUs a function of the COP year
  d$info$psnus <- datapackr::valid_PSNUs %>%
    dplyr::filter(country_uid %in% d$info$country_uids) %>%
    dplyr::filter(!is.na(psnu_type)) %>%
    dplyr::select(ou, country_name, snu1, psnu, psnu_uid)

  #Get the memo structure
  d <- memoStructure(d, d2_session)

  if (is.null(d$memo$partners_agencies)) {
    d$memo$partners_agencies <- getMechanismView(country_uids = d$info$country_uids,
                                                 cop_year = d$info$cop_year,
                                                 include_dedupe = TRUE,
                                                 include_MOH = FALSE,
                                                 d2_session = d2_session) %>%
      dplyr::select("Mechanism" = mechanism_code,
                    "Partner" = partner_desc,
                    "Agency" = agency)
  }

  if (memo_type == "datapack") {

    d$memo$datapack$prios <- d$data$analytics %>%
      dplyr::select(psnu_uid, prioritization) %>%
      dplyr::distinct() %>%
      dplyr::left_join(datapackr::prioritization_dict(),by = c("prioritization" = "name")) %>%
      dplyr::select(-Prioritization) %>%
      dplyr::mutate(prioritization = dplyr::case_when(
        is.na(prioritization) ~ "No Prioritization",
        TRUE ~ prioritization
      ))
  }

  if (memo_type %in% c("datim","comparison")) {
    #Get the existing prioritizations
    d$memo$datim$prios <- fetchPrioritizationTable(d$info$psnus$psnu_uid,d$info$cop_year,d2_session)
  }


  d
}


#' Title
#'
#' @param d Datapackr d object
#' @param d2_session Datimutils d2_session object
#' @description Get existing PSNUxIM  raw data from DATIM and store in d$memo$datim$analytics
#' @return  Datapackr d object
#' @export
#'
prepareExistingDataAnalytics <- function(d, d2_session) {


  #Fetch the raw data from DATIM
  d$memo$datim$analytics <- getCOPDataFromDATIM(d$info$country_uids,
                             d$info$cop_year,
                             streams = "mer_targets",
                             d2_session = d2_session) %>%

    adorn_import_file(
      .,
      cop_year = d$info$cop_year,
      psnu_prioritizations =  dplyr::select(d$memo$datim$prios,"orgUnit" = psnu_uid, value),
      d2_session = d2_session
    )

  d

}

#
#' Title
#'
#' @param analytics Data frame consisting of at least psnu_uid,categoryoptioncombo_id,
#' mechanism_code and target value
#' @param memo_type Type of memo
#' @param inds Data frame of indicators from getMemoIndicators
#' @param prios Data frame of prioritization levels depending on the memo type
#' @param partners_agencies Result of getMechanismView
#' @psnus List of PSNUs
#' @description This function calculates COP memo indicators at the PSNU level. If the parallel
#' library is installed, very significant processing times can be achieved through
#' parallel processing.
#'
#' @return A dataframe of COP indicators aggregated to the PSNU level.
#' @export
#'
prepareMemoDataByPSNU <- function(analytics, memo_type, inds, prios, partners_agencies, psnus) {
   #Now we need to calculate the indicators

   df <-  analytics %>%
    dplyr::select(dataelement_id, psnu_uid, categoryoptioncombo_id, mechanism_code, value = target_value) %>%
    dplyr::mutate(combi = paste0("#{", dataelement_id, ".", categoryoptioncombo_id, "}")) %>%
    dplyr::select(-dataelement_id,-categoryoptioncombo_id) %>%
    dplyr::group_by(psnu_uid,mechanism_code) %>%
    tidyr::nest()

  #Evaluate the indicators in parallel if possible
  if ("parallel" %in% rownames(installed.packages()) == TRUE) {
    df$indicator_results <-
      parallel::mclapply(df$data, function(x)
        evaluateIndicators(x$combi, x$value, inds), mc.cores = parallel::detectCores())
  } else {
    df$indicator_results <-
      lapply(df$data, function(x)
        evaluateIndicators(x$combi, x$value, inds))
  }


  df %>%
    dplyr::select(-data) %>%
    tidyr::unnest(indicator_results) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(name = stringr::str_replace_all(name, "^COP\\d\\d Targets ", "")) %>%
    dplyr::mutate(name = stringr::str_trim(name)) %>%
    tidyr::separate("name", into = c("Indicator", "N_OR_D", "Age"), sep = " ") %>%
    dplyr::mutate(Indicator = dplyr::case_when(Indicator == "GEND_GBV" & N_OR_D == "Physical" ~
                                          "GEND_GBV Physical and Emotional Violence",
                                        Indicator == "GEND_GBV" & N_OR_D == "Sexual" ~
                                          "GEND_GBV Sexual Violence",
                                        TRUE ~ Indicator)) %>%
    dplyr::select(-"N_OR_D") %>%
    dplyr::mutate(Age = dplyr::case_when(Age == "15-" ~ "<15",
                                  Age == "15+" ~ "15+",
                                  Age == "18-" ~"<18",
                                  Age == "18+" ~ "18+",
                                  TRUE ~ "Total")) %>%
    dplyr::mutate(Age = dplyr::case_when(Indicator %in% c("CXCA_SCRN",
                                                   "OVC_HIVSTAT",
                                                   "KP_PREV",
                                                   "PMTCT_EID",
                                                   "KP_MAT",
                                                   "VMMC_CIRC",
                                                   "PrEP_NEW",
                                                   "PrEP_CURR",
                                                   "GEND_GBV") ~ "Total",
                                  TRUE ~ Age)) %>%
    dplyr::select(-id,-numerator,-denominator) %>%
    dplyr::left_join(dplyr::select(prios,psnu_uid,prioritization), by = c("psnu_uid")) %>%
    dplyr::mutate(prioritization = dplyr::case_when(is.na(prioritization) ~ "No Prioritization",
                                                    TRUE ~ prioritization)) %>%
    dplyr::left_join(partners_agencies,by = c(mechanism_code = "Mechanism")) %>%
    dplyr::inner_join(psnus, by = c("psnu_uid")) %>%
    dplyr::rename("Mechanism" = mechanism_code)

}

#' Title
#'
#' @param df An analytics table, either d$memo$datim$analytics or d$memo$datapack$analytics
#' @param memo_structure Structure of the memo d$memo$structure
#' @param indicators A dataframe of indicators d$memo$inds
#' @param partners_table Data frame in d$memo$partners_agencies
#'
#' @return A dataframe of COP indicators aggregated to the partner level
#' @export
#'
prepareMemoDataByPartner <- function(df, memo_structure, indicators, partners_table) {


  if (is.null(df) | NROW(df) == 0) {
    return(NULL)
  }

  d_partners <- df   %>%
    dplyr::group_by(Indicator, Age, Agency, Partner, Mechanism) %>%
    dplyr::summarise(Value = sum(value), .groups = "drop")

  #We need to pad for zeros
  df_rows <- memo_structure %>%
    purrr::pluck("row_order") %>%
    dplyr::filter(!is.na(partner_chunk)) %>%
    dplyr::select(ind, options)

  d_base <- tidyr::crossing(df_rows, dplyr::distinct(unique(d_partners[, c("Agency", "Partner", "Mechanism")]))) %>%
    dplyr::mutate(Value = 0) %>%
    dplyr::rename("Indicator" = ind,
                  Age = options)

  #Calculate totals

  d_totals <- dplyr::bind_rows(d_base, d_partners) %>%
    dplyr::group_by(`Indicator`, `Age`) %>%
    dplyr::summarise(`Value` = sum(`Value`),.groups = "drop") %>%
    dplyr::mutate(`Partner` = "Total", `Mechanism` = "Total", Agency = "Total")

  #Remove dedupe
  d_partners <- dplyr::filter(d_partners, !(`Mechanism` %in% c("00001", "00000"))) #nolint

  d_indicators <- memo_structure %>%
    purrr::pluck("row_order") %>%
    dplyr::filter(!is.na(partner_chunk)) %>%
    dplyr::select(ind, options) %>%
    dplyr::mutate(indicator_name = factor(paste(ind, options)))

  #Put totals at the bottom of the table
  partner_levels <- c(sort(unique(d_partners$Partner)), "Total")
  agency_levels <- c(sort(unique(d_partners$Agency)), "Total")

  #Return the final data frame
    dplyr::bind_rows(d_totals, d_partners) %>%
    dplyr::mutate(indicator_name = paste(`Indicator`, `Age`)) %>%
    #dplyr::mutate(indicator_name = factor(indicator_name, levels=unique(d_indicators$indicator_name))) %>%
    dplyr::mutate(`Label` = indicator_name) %>%
    dplyr::select(`Agency`, `Partner`, `Mechanism`, `Label`, `Value`) %>%
    tidyr::pivot_wider(names_from = `Label`, values_from = `Value`, values_fill = 0) %>%
    dplyr::select(`Agency`, `Partner`, `Mechanism`, d_indicators$indicator_name) %>%
    dplyr::mutate(`Partner` = factor(Partner, levels = partner_levels),
                  `Agency` = factor(Agency, levels = agency_levels)) %>%
    dplyr::arrange(`Agency`, `Partner`, `Mechanism`)


}


#' Title
#'
#' @param df An analytics table, either d$memo$datim$analytics or d$memo$datapack$analytics
#' @param memo_structure Structure of the memo d$memo$structure
#' @param include_no_prio
#'
#' @return
#' @exportA dataframe of COP indicators aggregated to the prioritization level
#'
prepareMemoDataByPrio <- function(df, memo_structure,include_no_prio = TRUE) {

  df_rows <- memo_structure %>%
    purrr::pluck("row_order") %>%
    dplyr::select(ind, options)

  df_cols <- memo_structure %>%
    purrr::pluck("col_order") %>%
    dplyr::select(name)

  df_base <- tidyr::crossing(df_rows, dplyr::select(df_cols, col_name = name)) %>%
    dplyr::arrange(ind, options, col_name) %>%
    dplyr::mutate(Value = 0) %>%
    dplyr::rename("Indicator" = ind,
                  Age = options)


  df <- df %>%
    dplyr::group_by(`Indicator`, `Age`, `prioritization`) %>%
    dplyr::summarise(Value = sum(value), .groups = "drop") %>%
    dplyr::rename("col_name" = "prioritization")

  df_totals <- df %>%
    dplyr::filter(Age != "Total") %>%
    dplyr::group_by(Indicator, col_name) %>%
    dplyr::summarise(Value = sum(Value),.groups = "drop") %>%
    dplyr::mutate(Age = "Total") %>%
    dplyr::select(names(df))

  df_final <- dplyr::bind_rows(df, df_totals) %>%
    dplyr::mutate(col_name = factor(col_name, levels = df_cols$name)) %>%
    dplyr::mutate(Indicator = factor(Indicator, levels = unique(df_rows$ind))) %>%
    dplyr::arrange(Indicator, col_name) %>%
    tidyr::pivot_wider(names_from = col_name, values_from = "Value", values_fill = 0) %>%
    suppressWarnings()

  df_final %<>%
    dplyr::mutate("Total" = rowSums(dplyr::across(where(is.numeric)), na.rm = TRUE)) %>%
    dplyr::select("Indicator", "Age", 3:dim(.)[2])

  if (!include_no_prio & any("No Prioritization" %in% names(df_final))) {
    df_final <- dplyr::select(df_final, -`No Prioritization`) # nolint
  }


  df_final %>%  dplyr::select(where(~ any(. != 0))) # Remove all columns which are completely zero

  }


#' Title
#'
#' @param d
#' @param d2_session
#' @param memo_type One of the following:
#' datapack: Create the memo based on the data in the datapack or OPU datapack
#' datim: Create the memo based on data currently in DATIM
#' comparison: Create a comparison memo with data from both DATIM and the datapack
#'
#' A list of tables is returned in d$memo$datapack or d$memo$datim depending on the
#' requested type.
#'
#' by_psnu: Dataframe of indicators aggregated to the PSNU level
#' by_prio: Dataframe of indicators aggregated to the prioritization level
#' by_partner: Dataframe of indicators aggregate to the partner level
#'
#'
#' @param include_no_prio If TRUE include no prioritization as a column in the output
#' @return
#' @export
#'
prepareMemoData <- function(d,d2_session, memo_type,include_no_prio = TRUE) {

  if (!(memo_type %in% c("datapack","datim","comparison"))) {
    stop("Memo type must be one of datapack,datim,comparison")
  }

  d <- prepareMemoMetadata(d, d2_session,memo_type)

  if (memo_type %in% c("datim", "comparison")) {

    d <- prepareExistingDataAnalytics(d, d2_session)

    d$memo$datim$by_psnu <-
      prepareMemoDataByPSNU(d$memo$datim$analytics,
      "datim",
       d$memo$inds,
       d$memo$datim$prios,
       d$memo$partners_agencies,
       d$info$psnus)

    d$memo$datim$by_partner <-
      prepareMemoDataByPartner(
        d$memo$datapack$by_psnu,
        d$memo$structure,
        d$memo$inds,
        d$memo$partners_agencies
      )

    d$memo$datim$by_prio <- prepareMemoDataByPrio(d$memo$datim$by_psnu,d$memo$structure,include_no_prio)
  }


  if (memo_type %in% c("datapack", "comparison")) {

    d$memo$datapack$by_psnu <- prepareMemoDataByPSNU(d$data$analytics,
    "datapack",
    d$memo$inds,
    d$memo$datapack$prios,
    d$memo$partners_agencies,
    d$info$psnus)

    d$memo$datapack$by_partner <-
      prepareMemoDataByPartner(
        d$memo$datapack$by_psnu,
        d$memo$structure,
        d$memo$inds,
        d$memo$partners_agencies
      )

    d$memo$datapack$by_prio <-
      prepareMemoDataByPrio(d$memo$datapack$by_psnu, d$memo$structure,include_no_prio)
  }

  d

}
