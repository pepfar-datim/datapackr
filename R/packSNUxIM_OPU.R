#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom stats complete.cases
#' @title packSNUxIM_OPU(d)
#'
#' @description Packs SNUxIM tab for OPU Data Packs.
#'
#' @param d Datapackr sidecar
#' 
#' @return d
#' 
packSNUxIM_OPU <- function(d) {
  if (d$info$cop_year != 2020) {
    stop("Sorry! We're only set up to run this for COP20 OPUs for right now. Check back later please. Stay safe!")
  }
  
  if (NROW(d$data$snuxim_model_data) == 0) {
    warning("No SNUxIM model data supplied.")
  }
  
  # Map PSNUs ####
  data <- d$data$snuxim_model_data %>%
    dplyr::left_join(d$data$PSNUs, by = c("psnu_uid" = "psnu_uid")) %>%
  
  # Pivot wider ####
  tidyr::unite(col = mechcode_supporttype, attribute_option, support_type) %>%  
  dplyr::select(PSNU, indicator_code, Age, Sex, KeyPop,
                  mechcode_supporttype, value) %>%
    dplyr::mutate(
      mechcode_supporttype = dplyr::case_when(
        mechcode_supporttype == "00000_DSD" ~ "DSD Dedupe",
        mechcode_supporttype == "00000_TA" ~ "TA Dedupe",
        mechcode_supporttype == "00001_TA" ~ "Crosswalk Dedupe",
        TRUE ~ mechcode_supporttype
      )
    ) %>%
    tidyr::pivot_wider(names_from = mechcode_supporttype,
                       values_from = value) %>%
    
  # Double check that Dedupe cols all exist as expected
    datapackr::addcols(cnames = c("DSD Dedupe",
                                  "TA Dedupe",
                                  "Crosswalk Dedupe"),
                       type = "numeric") %>%
  
  # Create calculated columns ####
    dplyr::mutate(
      `Total Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}")), na.rm = TRUE),
      `DSD Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}_DSD")), na.rm = TRUE),
      `TA Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}_TA")), na.rm = TRUE)) %>%
    dplyr::mutate(
      `Deduplicated DSD Rollup` =
        rowSums(dplyr::select(., tidyselect::all_of(c("DSD Duplicated Rollup","DSD Dedupe"))),
                na.rm = T),
      `Deduplicated TA Rollup` =
        rowSums(dplyr::select(., tidyselect::all_of(c("TA Duplicated Rollup","TA Dedupe"))),
                na.rm = T)) %>%
    dplyr::mutate(
      `Total Deduplicated Rollup` =
        rowSums(
          dplyr::select(.,
                        tidyselect::all_of(c("Deduplicated DSD Rollup",
                                             "Deduplicated TA Rollup",
                                             "Crosswalk Dedupe"))),
          na.rm = TRUE
        )
    ) %>%
    dplyr::select(PSNU, indicator_code, Age, Sex, KeyPop,
                  `Total Deduplicated Rollup`, `Deduplicated DSD Rollup`, `Deduplicated TA Rollup`,
                  `Total Duplicated Rollup`, `DSD Duplicated Rollup`, `TA Duplicated Rollup`,
                  `DSD Dedupe`, `TA Dedupe`, `Crosswalk Dedupe`,
                  dplyr::everything()) %>%
    dplyr::arrange(PSNU, indicator_code, Age, Sex, KeyPop) %>%
  
  # Duplicate columns for Updated Section ####
    dplyr::left_join(x = ., y = ., by = c("PSNU",
                                        "indicator_code",
                                        "Age",
                                        "Sex",
                                        "KeyPop"),
                     suffix = c(".original", ".updated")) %>%
    dplyr::select(
      -`Total Duplicated Rollup.updated`,
      -`DSD Duplicated Rollup.updated`,
      -`TA Duplicated Rollup.updated`,
      -`DSD Dedupe.updated`,
      -`TA Dedupe.updated`,
      -`Crosswalk Dedupe.updated`
    )
  
  # Create all Max/Min columns ####
  # rowMax <- function(df, cn, regex) {
  #   df[[cn]] <- df %>%
  #     dplyr::select(tidyselect::matches(match = regex)) %>%
  #     purrr::pmap(pmax, na.rm = T) %>%
  #     as.numeric
  #   
  #   return(df)
  # }
  
  # data %<>%
  #   rowMax(cn = "Min Deduplicated TA Rollup.updated",
  #          regex = "\\d{4,}_TA\\.updated") %>%
  #   rowMax(cn = "Min Deduplicated DSD Rollup.updated",
  #          regex = "\\d{4,}_DSD\\.updated") %>%
    # rowMax(cn = "Min Total Deduplicated Rollup.updated",
    #        regex = "\\d{4,}_TA\\.updated") %>%
    # dplyr::mutate(
      # `Min Total Deduplicated Rollup.updated` =
      #   pmax(`Deduplicated DSD Rollup.updated`, `Deduplicated TA Rollup.updated`, na.rm = T),
      # `Max Deduplicated TA Rollup.updated` = `TA Duplicated Rollup.updated`,
      # `Max Deduplicated DSD Rollup.updated` = `DSD Duplicated Rollup.updated`,
      # `Max Total Deduplicated Rollup.updated` =
      #   rowSums(
      #     dplyr::select(., 
      #                   `Deduplicated DSD Rollup.updated`,
      #                   `Deduplicated TA Rollup.updated`),
      #       na.rm = T)
    # )
  
  # Grab formula and blank columns ####
  schema_plus <- d$info$schema %>%
    dplyr::filter(sheet_name == "PSNUxIM") %>%
    dplyr::arrange(col) %>%
    dplyr::filter(!is.na(indicator_code) & indicator_code != "12345_DSD") %>%
    dplyr::mutate(
      column_names = dplyr::case_when(
        is.na(indicator_code) ~ paste0("col_",col),
        col < 90 & col_type != "row_header" ~ paste0(indicator_code, ".original"),
        col >= 90 ~ paste0(indicator_code, ".updated"),
        TRUE ~ indicator_code
      )
    ) %>%
    dplyr::select(col, indicator_code, column_names, formula)
  
  fx_cols <- schema_plus %>%
    `row.names<-`(.[, "column_names"]) %>%
    dplyr::filter(!is.na(formula)) %>%
    dplyr::select(formula) %>%
    t() %>%
    tibble::as_tibble() %>%
    dplyr::slice(rep(1:dplyr::n(), times = NROW(data))) %>%
    dplyr::mutate_if(
      is.character,
      stringr::str_replace_all,
      pattern = paste0("(?<=[:upper:])", headerRow(tool = "OPU Data Pack Template",
                                                   cop_year = d$info$cop_year)
                       +1),
      replacement = as.character(1:NROW(data)
                                 + headerRow(tool = "OPU Data Pack Template",
                                             cop_year = d$info$cop_year)))
  
  # Classify formula columns as formulas ####
  ## TODO: Improve approach - Use apply form instead
  for (i in 1:length(fx_cols)) {
    if (!all(any(is.na(fx_cols[[i]])))) {
      class(fx_cols[[i]]) <- c(class(fx_cols[[i]]), "formula")
    }
  }
  
  # Compile Original Cols and write in ####
  original_cols_order <- schema_plus %>%
    dplyr::filter(!stringr::str_detect(column_names, "\\.updated$")) %>%
    dplyr::pull(column_names)
  
  data_original <- data %>%
    dplyr::select(
      tidyselect::all_of(original_cols_order),
      tidyselect::matches("\\.original")
    ) #%>%
    #dplyr::mutate_all(as.character)
  
  labels <- stringr::str_replace(names(data_original), "\\.original$", "")
  ids <- names(data_original)
  
  col_labels <- data.frame(labels)
  row.names(col_labels) <- ids
  col_labels <- t(col_labels) %>% as.data.frame()
  
  openxlsx::writeData(wb = d$tool$wb,
                      sheet = "PSNUxIM",
                      x = data_original,
                      xy = c(1, headerRow("OPU Data Pack Template", d$info$cop_year)+1),
                      colNames = F, rowNames = F, withFilter = FALSE)
  
  openxlsx::writeData(wb = d$tool$wb,
                      sheet = "PSNUxIM",
                      x = col_labels,
                      xy = c(1, headerRow("OPU Data Pack Template", d$info$cop_year)),
                      colNames = F, rowNames = F, withFilter = FALSE)
  
  # Compile updated data and write in ####
  updated_cols_order <- schema_plus %>%
    dplyr::filter(stringr::str_detect(column_names, "\\.updated$")) %>%
    dplyr::pull(column_names)
  
  data_updated <- data %>%
    dplyr::bind_cols(fx_cols) %>%
    dplyr::select(
      -tidyselect::all_of(names(data_original))) %>%
    dplyr::select(
      tidyselect::all_of(updated_cols_order),
      dplyr::everything()
    )
  
  labels <- stringr::str_replace(names(data_updated), "\\.updated$", "")
  ids <- names(data_updated)
  
  col_labels <- data.frame(labels)
  row.names(col_labels) <- ids
  col_labels <- t(col_labels) %>% as.data.frame()
  
  openxlsx::writeData(wb = d$tool$wb,
                      sheet = "PSNUxIM",
                      x = data_updated,
                      xy = c(90, headerRow("OPU Data Pack Template", d$info$cop_year)+1),
                      colNames = F, rowNames = F, withFilter = FALSE)
  
  openxlsx::writeData(wb = d$tool$wb,
                      sheet = "PSNUxIM",
                      x = col_labels,
                      xy = c(90, headerRow("OPU Data Pack Template", d$info$cop_year)),
                      colNames = F, rowNames = F, withFilter = FALSE)
  
  # Make sure column borders show up as expected ####
  #TODO: Make this more dynamic
  thin_border_cols <- c(6, 9, 12, 15, 93, 96, 99, 102, 105)
    
  openxlsx::addStyle(wb = d$tool$wb,
                     sheet = "PSNUxIM",
                     style = datapackr::styleGuide$cop21_opu$thin_border,
                     rows = 1:(headerRow("OPU Data Pack Template", d$info$cop_year)+NROW(data_original)),
                     cols = thin_border_cols,
                     gridExpand = TRUE,
                     stack = TRUE)
  
  openxlsx::addStyle(wb = d$tool$wb,
                     sheet = "PSNUxIM",
                     style = datapackr::styleGuide$cop21_opu$thick_border,
                     rows = 1:(headerRow("OPU Data Pack Template", d$info$cop_year)+NROW(data_original)),
                     cols = 90,
                     gridExpand = TRUE,
                     stack = TRUE)
  
  # Confirm no hidden cols by default ####
  openxlsx::setColWidths(wb = d$tool$wb,
                         sheet = "PSNUxIM",
                         cols = (length(original_cols_order)+1):89,
                         widths = 8.17)
  
  # Confirm all Col widths as expected
  openxlsx::setColWidths(wb = d$tool$wb,
                         sheet = "PSNUxIM",
                         cols = (length(original_cols_order)+1):89,
                         widths = 8.17)
  
  #TODO: Also check for other columns
  
  # Then hide cols ####
  unused_mech_cols.original <- (length(data_original)+1):89
  
  dedupe_cols <- schema_plus %>%
    dplyr::filter(stringr::str_detect(column_names,"(DSD|TA|Crosswalk) Dedupe\\.")) %>%
    dplyr::pull(col)
  
  cols_to_hide <- sort(c(dedupe_cols, unused_mech_cols.original))
  
  openxlsx::setColWidths(wb = d$tool$wb,
                         sheet = "PSNUxIM",
                         cols = c(cols_to_hide),
                         widths = 8.17,
                         hidden = rep(TRUE,length(cols_to_hide)))
  
  # Hide rows ####
  openxlsx::setRowHeights(d$tool$wb,
                          sheet = "PSNUxIM",
                          rows = 4:headerRow("OPU Data Pack Template", d$info$cop_year)-1,
                          heights = 0)
  
  # Make sure number formats are consistent ####
  openxlsx::addStyle(wb = d$tool$wb,
                     sheet = "PSNUxIM",
                     style = datapackr::styleGuide$cop21_opu$numeric_format,
                     rows = (1:NROW(data_original))+headerRow("OPU Data Pack Template", d$info$cop_year),
                     cols = 6:178,
                     gridExpand = TRUE,
                     stack = TRUE)
  
  
  return(d)
  
}
