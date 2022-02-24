#' @export
#' @title packPSNUxIM
#'
#' @description Packs the PSNUxIM tab in either a COP or OPU Data Pack.
#'
#' @param data Dataset containing totals for allocation within PSNUxIM tab,
#' formatted as a standard DHIS2 import file.
#' @inheritParams datapackr_params
#'
#' @return r Sidecar object containing both an openxlsx Workbook and alert messages
#'
packPSNUxIM <- function(wb,
                        data,
                        snuxim_model_data,
                        cop_year = NULL,
                        tool = "OPU Data Pack",
                        schema = NULL,
                        d2_session = dynGet("d2_default_session",
                                            inherits = TRUE)) {

  # Check/Fill in parameters ####
  params <- check_params(cop_year = cop_year,
                         tool = tool,
                         schema = schema)

  for (p in names(params)) {
    assign(p, purrr::pluck(params, p))
  }

  rm(params, p)

  if (!cop_year %in% c(2021, 2022)) {
    stop(paste0("Packing PSNU x IM tabs is not supported for COP", cop_year, " Data Packs."))
  }

  # Create data sidecar to eventually compile and return ####
  r <- list(
    wb = wb,
    info = list(messages = MessageQueue(),
    has_error = FALSE))

  # Prep model data ####
  ## Check if empty ####
  empty_snuxim_model_data <- snuxim_model_data %>%
    dplyr::filter(rowSums(is.na(.)) != ncol(.))
  # TODO: Consider replacing this with something more straightforward like:
  # all(is.na(snuxim_model_data))

  if (NROW(empty_snuxim_model_data) == 0 | is.null(snuxim_model_data)) {
    interactive_warning(paste0("Provided SNUxIM model data seems empty or ",
      "fatally flawed. Please provide acceptable model data."))
    snuxim_model_data <- NULL
  }

  ## Filter to match targets data ####
  snuxim_model_data %<>%
    dplyr::right_join(
      data %>% dplyr::select(-value, -attributeOptionCombo) %>% dplyr::distinct(),
      by = c("dataElement" = "dataElement",
             "period" = "period",
             "orgUnit" = "orgUnit",
             "categoryOptionCombo" = "categoryOptionCombo"))

  ## Translate from import format ####
  snuxim_model_data %<>%
    datapackr::adorn_import_file(cop_year = cop_year,
                                 filter_rename_output = FALSE,
                                 d2_session = d2_session) %>%
    dplyr::select(indicator_code, psnu_uid = orgUnit, mechanism_code,
                  type = support_type,
                  age_option_name = Age, age_option_uid = valid_ages.id,
                  sex_option_name = Sex, sex_option_uid = valid_sexes.id,
                  kp_option_name = KeyPop, kp_option_uid = valid_kps.id,
                  value) %>%
    dplyr::group_by(dplyr::across(c(-mechanism_code, -type, -value))) %>%
    dplyr::mutate(
      percent = value / sum(value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(indicator_code, psnu_uid, age_option_name, sex_option_name,
                   kp_option_name, mechanism_code, type)

  ## Drop data that can't be allocated across mech & DSD/TA ####
  interactive_print("Getting data about your Mechanism Allocations from DATIM...")

  snuxim_model_data %<>%
    dplyr::filter(stringr::str_detect(mechanism_code, "\\d{4,}"),
                  stringr::str_detect(type, "DSD|TA"))

  ## Pivot mechs/type wider ####
  snuxim_model_data %<>%
    tidyr::unite(col = mechcode_supporttype, mechanism_code, type) %>%
    dplyr::select(psnu_uid, indicator_code, Age = age_option_name,
                  Sex = sex_option_name, KeyPop = kp_option_name,
                  mechcode_supporttype, percent, value) %>%
    dplyr::mutate(
      mechcode_supporttype = dplyr::case_when(
        mechcode_supporttype == "00000_DSD" ~ "DSD Dedupe",
        mechcode_supporttype == "00000_TA" ~ "TA Dedupe",
        mechcode_supporttype == "00001_TA" ~ "Crosswalk Dedupe",
        TRUE ~ mechcode_supporttype
      )
    )

  percents <- snuxim_model_data %>%
    dplyr::select(-value) %>%
    tidyr::pivot_wider(names_from = mechcode_supporttype,
                       values_from = percent)

  values <- snuxim_model_data %>%
    dplyr::select(-percent, -mechcode_supporttype) %>%
    dplyr::group_by(dplyr::across(c(-value))) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()

  if (NROW(percents) != NROW(values)) {
    stop("Aggregating values and percents led to different row counts!")
  }

  snuxim_model_data <- values %>%
    dplyr::left_join(percents,
                     by = c("psnu_uid", "indicator_code", "Age", "Sex", "KeyPop"))

  ## Align EID age bands with Data Pack ####
  snuxim_model_data %<>%
    dplyr::mutate(
      Age = dplyr::if_else(
        indicator_code %in% c("PMTCT_EID.N.2.T", "PMTCT_EID.N.12.T"),
        NA_character_,
        Age
      )
    )

  ## Check Dedupe cols ####
  snuxim_model_data %<>%
    datapackr::addcols(cnames = c("DSD Dedupe",
                                  "TA Dedupe",
                                  "Crosswalk Dedupe"),
                       type = "numeric")

  ## Create Deduplicated Rollups ####
  snuxim_model_data %<>%
    dplyr::mutate(
      `Total Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}|HllvX50cXC0")), na.rm = TRUE),
      `DSD Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}_DSD")), na.rm = TRUE),
      `TA Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}_TA")), na.rm = TRUE))

  ## Create Duplicated Rollups ####
  snuxim_model_data %<>%
    dplyr::mutate(
      `Deduplicated DSD Rollup` =
        rowSums(dplyr::select(., tidyselect::all_of(c("DSD Duplicated Rollup", "DSD Dedupe"))),
                na.rm = T),
      `Deduplicated TA Rollup` =
        rowSums(dplyr::select(., tidyselect::all_of(c("TA Duplicated Rollup", "TA Dedupe"))),
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
      )

  ## Create Max columns ####
  snuxim_model_data %<>%
    datapackr::rowMax(cn = "Max_TA.T_1", regex = "\\d{4,}_TA") %>% # nolint
    datapackr::rowMax(cn = "Max_DSD.T_1", regex = "\\d{4,}_DSD") %>% # nolint
    dplyr::mutate(
      `Max_Crosswalk.T_1` =
        pmax(`Deduplicated DSD Rollup`, `Deduplicated TA Rollup`, na.rm = T))

  ## Create Dedupe Resolution columns ####
  interactive_print("Studying your deduplication patterns...")

  snuxim_model_data %<>%
    dplyr::rowwise() %>%
    dplyr::mutate(ta_im_count = sum(!is.na(dplyr::c_across(tidyselect::matches("\\d{4,}_TA")))), # nolint
                  dsd_im_count = sum(!is.na(dplyr::c_across(tidyselect::matches("\\d{4,}_DSD"))))) %>% # nolint
    dplyr::ungroup() %>%
    dplyr::mutate(
      `TA Dedupe Resolution` = dplyr::case_when(
        `TA Duplicated Rollup` == 0 | ta_im_count <= 1 ~ NA_character_,
        # or where count(TA IMs) == 1
        `Deduplicated TA Rollup` == `TA Duplicated Rollup` ~ "SUM",
        `Deduplicated TA Rollup` == `Max_TA.T_1` ~ "MAX",
        TRUE ~ "CUSTOM"),
      `DSD Dedupe Resolution` = dplyr::case_when(
        `DSD Duplicated Rollup` == 0 | dsd_im_count <= 1 ~ NA_character_,
        `Deduplicated DSD Rollup` == `DSD Duplicated Rollup` ~ "SUM",
        `Deduplicated DSD Rollup` == `Max_DSD.T_1` ~ "MAX",
        TRUE ~ "CUSTOM"),
      `Crosswalk Dedupe Resolution` = dplyr::case_when(
        `Total Duplicated Rollup` == 0 | `Deduplicated TA Rollup` == 0 | `Deduplicated DSD Rollup` == 0
        ~ NA_character_,
        `Total Deduplicated Rollup` == `Total Duplicated Rollup` ~ "SUM",
        `Total Deduplicated Rollup` == `Max_Crosswalk.T_1` ~ "MAX",
        TRUE ~ "CUSTOM"),
      `Custom DSD Dedupe Allocation (% of DataPackTarget)` = `DSD Dedupe`,
      `Custom TA Dedupe Allocation (% of DataPackTarget)` = `TA Dedupe`,
      `Custom Crosswalk Dedupe Allocation (% of DataPackTarget)` = `Crosswalk Dedupe`
    ) %>%
    dplyr::select(psnu_uid, indicator_code, Age, Sex, KeyPop,
                  tidyselect::matches("\\d{4,}"), # nolint
                  `Custom DSD Dedupe Allocation (% of DataPackTarget)`,
                  `Custom TA Dedupe Allocation (% of DataPackTarget)`,
                  `Custom Crosswalk Dedupe Allocation (% of DataPackTarget)`,
                  `DSD Dedupe Resolution`,
                  `TA Dedupe Resolution`,
                  `Crosswalk Dedupe Resolution`,
                  `DSD Dedupe`, `TA Dedupe`, `Crosswalk Dedupe`)

  # Prep Targets dataset ####
  data %<>%
    adorn_import_file(cop_year = cop_year, filter_rename_output = FALSE,d2_session = d2_session) %>%
    dplyr::select(PSNU = dp_psnu, orgUnit, indicator_code, Age, Sex, KeyPop,
                  DataPackTarget = value) %>%
    dplyr::group_by(dplyr::across(c(-DataPackTarget))) %>%
    dplyr::summarise(DataPackTarget = sum(DataPackTarget)) %>%
    dplyr::ungroup()

  ## Drop AGYW_PREV (Not allocated to IMs) ####
  data %<>%
    dplyr::filter(!indicator_code %in% c("AGYW_PREV.N.T", "AGYW_PREV.D.T"))

  # Filter model dataset to only those data needed in tab ####
  interactive_print("Focusing on patterns relevant to your submitted tool...")

  if (NROW(snuxim_model_data) > 0) {
    snuxim_model_data <- data %>%
      dplyr::left_join(
        snuxim_model_data,
        by = c("orgUnit" = "psnu_uid",
               "indicator_code" = "indicator_code",
               "Age" = "Age",
               "Sex" = "Sex",
               "KeyPop" = "KeyPop"))
  } else {
    snuxim_model_data <- data %>%
      datapackr::addcols(cnames = c("Custom DSD Dedupe Allocation (% of DataPackTarget)",
                                    "Custom TA Dedupe Allocation (% of DataPackTarget)",
                                    "Custom Crosswalk Dedupe Allocation (% of DataPackTarget)"),
                         type = "numeric") %>%
      datapackr::addcols(cnames = c("DSD Dedupe Resolution",
                                    "TA Dedupe Resolution",
                                    "Crosswalk Dedupe Resolution"),
                         type = "character")
  }

  # TODO: Filter to see if we're trying to write data that's already there
  # TODO: Check whether we need to proceed at all, based on whether `data` is duplicated in PSNUxIM tab already
  # TODO: Then move all these checks up to avoid wasting time processing snuxim_model_data

  # Document existing state of PSNUxIM tab ####
  header_row <- headerRow(tool = tool, cop_year = cop_year)
  header_cols <- schema %>%
    dplyr::filter(sheet_name == "PSNUxIM"
                  & col_type == "row_header") %>%
    dplyr::pull(indicator_code)

  existing_data <- openxlsx::read.xlsx(r$wb,
                                       sheet = "PSNUxIM",
                                       skipEmptyRows = FALSE,
                                       startRow = header_row,
                                       cols = seq_len(NROW(header_cols)),
                                       colNames = TRUE)

  first_blank_row <- NROW(existing_data) + header_row + 1

  initial_psnuxim <- first_blank_row == (header_row + 1)

  # Add DataPackTarget to non-OPU Data Pack ####
  if (tool == "Data Pack") {
  ## Get ID & target col letters ####
    interactive_print("Analyzing targets set across your Data Pack...")

    sheets <- schema %>%
      dplyr::filter(
        data_structure == "normal", !sheet_name %in% c("PSNUxIM", "KP Validation")) %>%
      dplyr::pull(sheet_name) %>%
      unique()

    col_ltrs <- tibble::tribble(~sheet_name, ~indicator_code, ~target_col)

    for (sheet in sheets) {
      subm_cols <-
        openxlsx::read.xlsx(
          wb,
          sheet = sheet,
          rows = header_row,
          colNames = TRUE) %>%
        names(.) %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(indicator_code = value) %>%
        dplyr::mutate(sheet_name = sheet,
                      submission_order = 1:dplyr::n(),
                      col_ltr = openxlsx::int2col(submission_order)) %>%
        dplyr::left_join(schema %>% dplyr::select(indicator_code, sheet_name, dataset, col_type),
                         by = c("indicator_code", "sheet_name"))

      id <- ifelse("ID" %in% subm_cols$indicator_code, "ID", "PSNU")
      id_cols <- subm_cols[subm_cols$indicator_code == id,] %>%
        dplyr::select(sheet_name, id_col = col_ltr)

      col_ltrs <- subm_cols %>%
        dplyr::filter(dataset == "mer" & col_type == "target") %>%
        dplyr::select(sheet_name, indicator_code, target_col = col_ltr) %>%
        dplyr::left_join(id_cols, by = "sheet_name") %>%

  ## Accommodate OGAC request to aggregate OVC_HIVSTAT.T across age/sex ####
        dplyr::mutate(
          id_col = dplyr::if_else(indicator_code == "OVC_HIVSTAT.T", "B", id_col)) %>%
        dplyr::bind_rows(col_ltrs, .)

    }

  ## Add DataPackTarget column as formula ####
    snuxim_model_data %<>%
      dplyr::left_join(
        col_ltrs, by = "indicator_code") %>%
      dplyr::mutate(
        row = as.integer((1:dplyr::n()) + first_blank_row - 1),

    # nolint start
        DataPackTarget =
          dplyr::case_when(
            (Age == "50+" & sheet_name %in% c("Cascade", "PMTCT", "TB", "VMMC"))
              ~ paste0(
                'SUM(SUMIFS(', sheet_name, '!$', target_col, ':$', target_col,
                ',', sheet_name, '!$B:$B,$A', row,
                ',', sheet_name, '!$C:$C,{"50-54","55-59","60-64","65+"}',
                ',', sheet_name, '!$D:$D,$D', row, '))'),
            TRUE ~ paste0('SUMIF(', sheet_name, '!$', id_col, ':$', id_col,
                       ',$F', row, ',', sheet_name, '!$', target_col, ':$', target_col, ')'))
      ) %>%
      dplyr::select(-id_col, -sheet_name, -target_col, -row)
    # nolint end

    class(snuxim_model_data[["DataPackTarget"]]) <- c(class(snuxim_model_data[["DataPackTarget"]]), "formula")
  }

  # Get formulas & column order from schema ####
  interactive_print("Building your custom PSNUxIM tab...")

  data_structure <- schema %>%
    dplyr::filter(sheet_name == "PSNUxIM")

  start_col <- ifelse(cop_year == 2021, "12345_DSD", "Not PEPFAR")

  col.im.targets <- data_structure %>%
    dplyr::filter(col_type == "target",
                  indicator_code %in% c("Not PEPFAR", "12345_DSD", "")) %>%
    dplyr::filter(
      indicator_code == start_col | col == max(col)) %>%
    dplyr::pull(col)

  col.im.percents <- data_structure %>%
    dplyr::filter(col_type == "allocation"
                  & (indicator_code %in% c("12345_DSD", "Not PEPFAR")
                     | is.na(indicator_code))) %>%
    dplyr::filter(
      indicator_code == start_col | col == max(col)) %>%
    dplyr::pull(col)

  count.im.datim <- names(snuxim_model_data)[stringr::str_detect(names(snuxim_model_data), "\\d{4,}_(DSD|TA)")] %>%
    length()

  col.formulas <- data_structure %>%
    dplyr::filter(
      !is.na(formula),
      col < (col.im.targets[1])) %>%
    dplyr::pull(col)

  ## TODO: Improve this next piece to be more efficient instead of using str_replace_all

  data_structure %<>%
    dplyr::arrange(col) %>%
    dplyr::mutate(
      column_names = dplyr::case_when(
        col >= col.im.percents[1] & col <= col.im.percents[2] ~ paste0("percent_col_", col),
        col >= col.im.targets[1] & col <= (col.im.targets[1] + count.im.datim - 1) ~ paste0("target_col_", col),
        #col >= col.im.targets[1] & col <= col.im.targets[2] ~ paste0("target_col_", col),
        TRUE ~ indicator_code)
    ) %>%
    dplyr::filter(col < col.im.targets[1]) %>%
    tibble::column_to_rownames(var = "column_names") %>%
    dplyr::select(formula) %>%
    t() %>%
    tibble::as_tibble() %>%
    ## Setup formulas
    dplyr::slice(rep(1:dplyr::n(), times = NROW(snuxim_model_data))) %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(col.formulas),
                    ~stringr::str_replace_all(., pattern = paste0("(?<=[:upper:])", header_row + 1),
                      replacement = as.character(seq_len(NROW(snuxim_model_data)) + first_blank_row - 1))))

  # Classify formula columns as formulas
  ## TODO: Improve approach
  for (i in seq_along(data_structure)) {
    if (!all(any(is.na(data_structure[[i]])))) {
      class(data_structure[[i]]) <- c(class(data_structure[[i]]), "formula")
    }
  }

  # Combine schema with SNU x IM model dataset ####
  #TODO: Fix this to not re-add mechanisms removed by the Country Team
  #(filter snuxim_model_data to only columns with not all NA related to data in missing combos)
  data_structure <- datapackr::swapColumns(data_structure, snuxim_model_data) %>%
    dplyr::bind_cols(
      snuxim_model_data %>%
        dplyr::select(tidyselect::matches("\\d{4,}")) # nolint
    ) %>%
    dplyr::mutate(`Not PEPFAR` = as.double(NA_integer_))

  header_cols <- schema %>%
    dplyr::filter(sheet_name == "PSNUxIM"
                  & col < col.im.percents[1]) %>%
    dplyr::pull(indicator_code)

  IM_cols <- data_structure %>%
    dplyr::select(tidyselect::matches("\\d{4,}")) %>% # nolint
    names() %>%
    sort()

  left_side <- data_structure %>%
    dplyr::select(
      tidyselect::all_of(header_cols),
      `Not PEPFAR`,
      tidyselect::all_of(IM_cols)
    )

  right_side <- data_structure %>%
    dplyr::select(
      -tidyselect::all_of(names(left_side)),
      -tidyselect::matches("percent_col_\\d{1,3}") # nolint
    )

  # Write data to sheet ####
  interactive_print("Writing your new PSNUxIM data to your Data Pack...")
  # Have to remove filters to accommodate bug in openxlsx
  r$wb %<>% openxlsx::removeFilter(names(.))

  ## Right Side ----
  openxlsx::writeData(wb = r$wb,
                      sheet = "PSNUxIM",
                      x = right_side,
                      xy = c(col.im.percents[2] + 1, first_blank_row),
                      colNames = F, rowNames = F, withFilter = FALSE)

  # Document new and existing mech cols ####
  existing_im_cols <-
    openxlsx::read.xlsx(r$wb,
                        sheet = "PSNUxIM",
                        skipEmptyRows = FALSE,
                        rows = header_row,
                        cols = col.im.percents[1]:col.im.percents[2],
                        colNames = FALSE) %>%
    as.character()

  existing_im_cols <- existing_im_cols[!existing_im_cols %in% c("", "Not PEPFAR", "12345_DSD")]

  complete_cols <- c(existing_im_cols, IM_cols) %>% unique()
  new_mech_cols <- IM_cols[!IM_cols %in% existing_im_cols]

  ## Left Side ----
  if (initial_psnuxim) {
    openxlsx::writeData(wb = r$wb,
                        sheet = "PSNUxIM",
                        x = left_side,
                        xy = c(1, first_blank_row - 1),
                        colNames = T, rowNames = F, withFilter = FALSE)

  } else {
    left_side %<>%
      addcols(complete_cols) %>%
      dplyr::select(tidyselect::all_of(c(header_cols)),
                    tidyselect::any_of("Not PEPFAR"),
                    tidyselect::all_of(c(complete_cols)))

    openxlsx::writeData(wb = r$wb,
                        sheet = "PSNUxIM",
                        x = left_side,
                        xy = c(1, first_blank_row),
                        colNames = F, rowNames = F, withFilter = FALSE)

  ## Add additional col_names if any ----
    if (length(new_mech_cols) > 0) {
      openxlsx::writeData(wb = r$wb,
                          sheet = "PSNUxIM",
                          x = new_mech_cols %>% as.matrix() %>% t(),
                          xy = c(col.im.percents[1] + length(existing_im_cols) + 1,
                                 header_row),
                          colNames = F, rowNames = F, withFilter = FALSE)
    }

  ## Add green highlights to appended rows, if any
    newRowStyle <- openxlsx::createStyle(fontColour = "#006100", fgFill = "#C6EFCE")

    openxlsx::addStyle(
      wb = r$wb,
      sheet = "PSNUxIM",
      style = newRowStyle,
      rows = (first_blank_row):(first_blank_row - 1 + NROW(left_side)),
      cols = 1:5,
      gridExpand = TRUE,
      stack = FALSE)
  }

  # Formatting ####
  interactive_print("Tidying up...")

  ## Format percent columns
  interactive_print("Stylizing percent columns...")

  percentCols <- schema %>%
    dplyr::filter(sheet_name == "PSNUxIM",
                  value_type == "percentage") %>%
    dplyr::pull(col)

  percentStyle <- openxlsx::createStyle(numFmt = "0%")

  openxlsx::addStyle(wb = r$wb,
                     sheet = "PSNUxIM",
                     style = percentStyle,
                     rows = first_blank_row:(first_blank_row - 1 + NROW(left_side)),
                     cols = percentCols,
                     gridExpand = TRUE,
                     stack = FALSE)

  ## Format integers
  integerStyle = openxlsx::createStyle(numFmt = "#,##0") # nolint

  integerCols <- schema %>%
    dplyr::filter(sheet_name == "PSNUxIM",
                  value_type == "integer") %>%
    dplyr::pull(col)

  openxlsx::addStyle(
    wb = r$wb,
    sheet = "PSNUxIM",
    style = integerStyle,
    rows = (first_blank_row):(first_blank_row - 1 + NROW(left_side)),
    cols = integerCols,
    gridExpand = TRUE,
    stack = TRUE)

  ## Consider adding errorStyling here to emphasize where incorrect disaggs entered.
  # errorStyle <- openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  # warningStyle <- openxlsx::createStyle(fontColour = "#9C5700", bgFill = "#FFEB9C")
  # normalStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FFFFFF")

  ## Hide rows 5-13
  openxlsx::setRowHeights(wb = r$wb,
                          sheet = "PSNUxIM",
                          rows = 4:(header_row - 1),
                          heights = 0)

  ## Hide unused columns in left section ####
  openxlsx::setColWidths(wb = r$wb,
                         sheet = "PSNUxIM",
                         cols = col.im.percents[1]:col.im.percents[2],
                         hidden = FALSE)

  hiddenCols <- schema %>%
    dplyr::filter(sheet_name == "PSNUxIM",
                  indicator_code %in% c("ID", "sheet_num", "DSD Dedupe",
                                        "TA Dedupe", "Crosswalk Dedupe")) %>%
    dplyr::pull(col) %>%
    c(.,
      (length(left_side) + 1):col.im.percents[2])

  openxlsx::setColWidths(wb = r$wb,
                         sheet = "PSNUxIM",
                         cols = hiddenCols,
                         hidden = TRUE)

  # Tab generation date ####
  openxlsx::writeData(r$wb,
                      sheet = "PSNUxIM",
                      x = paste("Last Updated on:", Sys.time()),
                      xy = c(1, 2),
                      colNames = F)

  # Package Version ####
  openxlsx::writeData(r$wb,
                      sheet = "PSNUxIM",
                      x = paste("Package version:",
                                as.character(utils::packageVersion("datapackr"))),
                      xy = c(2, 2),
                      colNames = F)

  # Warning Messages ####
  interactive_print("Compiling alert messages...")
  warning_msg <-
    paste0(
      "INFO: Based on your submission, we have ",
      ifelse(!initial_psnuxim,
             paste0("added ", NROW(left_side), " rows to your PSNUxIM tab.",
                    " These have been highlighted green for your reference."),
             "populated your PSNUxIM tab for the first time."),
      " An updated copy of your Data Pack is now available for download.",
      " Please review your PSNUxIM tab, and carefully review the Data Pack User Guide",
      " for detailed guidance on how to use this tab.",
      "\n\n",
      "NOTE: Upon opening your updated PSNUxIM tab, please be sure to drag down",
      " all formulas from column CW to the right.",
      "\n\n",
      "NOTE: DO NOT delete any columns in this tool, and do not add any new columns",
      " between existing columns.",
      "\n\n",
      "NOTE: Any external references used in cell formulas will now be corrupt and",
      " cause '#N/A' errors. Please review your Data Pack for these cases and correct.",
      "\n\n",
      "If you have any questions, please submit a Help Desk ticket at DATIM.Zendesk.com.",
      "\n")

  r$info$messages <- appendMessage(r$info$messages, warning_msg, "INFO")

  return(r)

}
