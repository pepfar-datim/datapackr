#' @title Separate Indicator Metadata
#'
#' @param x Data frame containing indicator names which have not been separated
#'
#' @return Data frame with indicator names separated into
#' two columns: Indicator & Age
#' @export
#'
separateIndicatorMetadata <- function(x) {

  x %>%
    dplyr::mutate(name = stringr::str_replace_all(name, "^COP\\d\\d Targets ", "")) %>%
    dplyr::mutate(name = stringr::str_trim(name)) %>%
    tidyr::separate("name", into = c("Indicator", "N_OR_D", "Age"), sep = " ",) %>%
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
                                         TRUE ~ Age))
}


#' @title Prepare COP Memo Prioritization Target Table Data
#'
#' @inheritParams datapackr_params
#'
#' @return Datapackr d object with d$data$memo table
#' @export
#'
preparePrioTable <- function(d, d2_session = dynGet("d2_default_session",
                                                    inherits = TRUE)) {

  d <- memoStructure(d)

  df_cols <- purrr::pluck(d$memo$structure, "col_order")

  df_rows <- purrr::pluck(d$memo$structure, "row_order") %>%
    dplyr::select(ind, options) %>%
    dplyr::mutate(row_order = dplyr::row_number())

  df_base <- tidyr::crossing(df_rows, dplyr::select(df_cols, name)) %>%
    dplyr::arrange(ind, options, name) %>%
    dplyr::mutate(value = 0) %>%
    dplyr::select("Indicator" = ind,
                  Age = options,
                  prioritization = name,
                  value)

  inds <- datapackr::getMemoIndicators(d$info$cop_year, d2_session)
  #Calculate the indicators by prioritization level
  df <- d %>%
    purrr::pluck("data") %>%
    purrr::pluck("analytics") %>%
    dplyr::filter(!is.na(target_value)) %>%
    dplyr::select(dataelement_id,
                  categoryoptioncombo_id,
                  prioritization,
                  value = target_value) %>%
    dplyr::group_by(dataelement_id, categoryoptioncombo_id, prioritization) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(combi = paste0("#{", dataelement_id, ".", categoryoptioncombo_id, "}")) %>%
    dplyr::select(-dataelement_id,-categoryoptioncombo_id) %>%
    dplyr::group_by(prioritization) %>%
    tidyr::nest()

  #Likely not worth the overhead to process  this in parallel since the number of groups is small
  df$indicator_results <-
    lapply(df$data, function(x)
      evaluateIndicators(x$combi, x$value, inds = inds))


  if (NROW(df) == 0) {
    return(d)
  }

  df <- df %>%
    dplyr::select(-data) %>%
    tidyr::unnest(indicator_results) %>%
    dplyr::select(-id, -numerator, -denominator) %>%
    tidyr::complete(., prioritization, name, fill = list(value = 0)) %>%
    separateIndicatorMetadata(.) %>%
    dplyr::group_by(Age, Indicator, prioritization) %>%
    dplyr::summarise(value = sum(value),.groups = "drop") %>%
    dplyr::mutate(prioritization = dplyr::case_when(is.na(prioritization) ~ "No Prioritization",
                                                    TRUE ~ prioritization))

  df_total <- df %>%
    dplyr::filter(Age != "Total") %>%
    dplyr::select(-Age) %>%
    dplyr::group_by(prioritization, Indicator) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Age = "Total") %>%
    dplyr::select(names(df))

  d$data$prio_table <- dplyr::bind_rows(df, df_total, df_base) %>%
    dplyr::group_by(Indicator, Age, prioritization) %>%
    dplyr::summarise(value = sum(value),.groups = "drop") %>%
    dplyr::mutate(Age = factor(Age, levels = (unique(Age)))) %>%
    dplyr::left_join(df_rows, by = c("Indicator" = "ind", "Age" = "options")) %>%
    dplyr::left_join((df_cols %>%
                        dplyr::select(name, col_order)), by = c("prioritization" = "name")) %>%
    dplyr::select(Indicator, Age, prioritization, value, row_order, col_order) %>%
    dplyr::arrange(col_order, row_order, Age) %>%
    dplyr::select(-row_order, -col_order) %>%
    tidyr::pivot_wider(names_from = prioritization, values_from = "value") %>%
    dplyr::mutate("Total" = rowSums(across(where(is.numeric)))) %>%
    dplyr::filter(Total != 0) %>% # Remove all rows which are completely zero
    dplyr::select("Indicator", "Age", 3:dim(.)[2]) %>%
    dplyr::select(where(~ any(. != 0))) # Remove all columns which are completely zero

  d
}
