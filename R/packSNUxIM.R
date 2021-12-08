#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom stats complete.cases
#' @title packSNUxIM(data)
#'
#' @description Packs SNUxIM data prepared from unPackSNUxIM for import to DATIM.
#'
#' @param d Datapackr object
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return d
#'
packSNUxIM <- function(d,
                       d2_session = dynGet("d2_default_session",
                                           inherits = TRUE)) {

  if (!d$info$cop_year %in% c(2021)) {
    stop(paste0("Packing SNU x IM tabs is not supported for COP ", d$info$cop_year, " Data Packs."))
  }

  # Check if SNUxIM data already exists ####
  if (NROW(d$data$SNUxIM) == 1 & is.na(d$data$SNUxIM$PSNU[1])) {
    d$info$has_psnuxim <- FALSE
  } else {
    d$info$has_psnuxim <- TRUE
  }

  # If does exist, extract missing combos ####
  if (d$info$has_psnuxim) {
    d$data$missingCombos <- d$data$MER %>%
      # TODO: Create this here rather than upstream
      dplyr::anti_join(d$data$PSNUxIM_combos)

    d$info$missing_psnuxim_combos <- (NROW(d$data$missingCombos) > 0)
  }

  # Proceed IFF no PSNU x IM tab exists, or exists but with missing combos ####
  if (d$info$has_psnuxim & !d$info$missing_psnuxim_combos) {
    return(d)
  }

  # Prepare SNU x IM model dataset ####
  if (d$info$has_psnuxim & d$info$missing_psnuxim_combos) {
    targets_data <- d$data$missingCombos
  } else {
    targets_data <- d$data$MER
  }

    #TODO: Consider preparing this ahead of time for all OUs
  snuxim_model_data <- readRDS(d$keychain$snuxim_model_data_path) %>%
    prepare_model_data.PSNUxIM(snuxim_model_data = .,
                               country_uids = d$info$country_uids)

  # Filter SNU x IM model dataset to only those data needed in tab ####
  interactive_print("Focusing on patterns relevant to your submitted tool...")

  # Do not include AGYW_PREV -- These are not allocated to IMs
  targets_data %<>%
    dplyr::filter(!indicator_code %in% c("AGYW_PREV.N.T", "AGYW_PREV.D.T"))

  if (NROW(snuxim_model_data) > 0) {
    snuxim_model_data %<>%
      dplyr::right_join(
        targets_data,
        by = c("psnu_uid" = "psnuid",
               "indicator_code" = "indicator_code",
               "Age" = "Age",
               "Sex" = "Sex",
               "KeyPop" = "KeyPop")) %>%
      dplyr::select(-value)
  } else {
    snuxim_model_data <- targets_data %>%
      datapackr::addcols(cnames = c("Custom DSD Dedupe Allocation (FY22) (% of DataPackTarget)",
                                    "Custom TA Dedupe Allocation (FY22) (% of DataPackTarget)",
                                    "Custom Crosswalk Dedupe Allocation (FY22) (% of DataPackTarget)"),
                         type = "numeric") %>%
      datapackr::addcols(cnames = c("DSD Dedupe Resolution (FY22)",
                                    "TA Dedupe Resolution (FY22)",
                                    "Crosswalk Dedupe Resolution (FY22)"),
                         type = "character")
  }

  # Add DataPackTarget ####
  interactive_print("Analyzing targets set across your Data Pack...")

  top_rows <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)

  if (d$info$has_psnuxim) {
    existing_rows <-
      readxl::read_excel(
        path = d$keychain$submission_path,
        sheet = "PSNUxIM",
        range = readxl::cell_limits(c(1, 2), c(NA, 2)),
        col_names = F,
        .name_repair = "minimal"
      ) %>%
      NROW()
  } else {
    existing_rows <- top_rows
  }

  get_ID_col <- function(data) {
    col_letter <- data %>%
      dplyr::filter(indicator_code == "ID")

    if (NROW(col_letter) == 0) {
      col_letter <- data %>%
        dplyr::filter(indicator_code == "PSNU")
    }

    col_letter %<>%
      dplyr::pull(submission_order) %>%
      openxlsx::int2col()

    return(col_letter)
  }

  id_cols <- lapply(d$info$col_check, get_ID_col) %>%
    dplyr::bind_rows() %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::rename(id_col = V1) %>%
    tibble::rownames_to_column("sheet_name")

  target_cols <- datapackr::cop21_data_pack_schema %>%
    dplyr::filter(dataset == "mer" & col_type == "target" & (!sheet_name %in% c("PSNUxIM", "AGYW"))) %>%
    dplyr::mutate(
      target_col = openxlsx::int2col(col)
    ) %>%
    dplyr::select(sheet_name, indicator_code, target_col)

  snuxim_model_data %<>%
    dplyr::left_join(
      id_cols, by = c("sheet_name" = "sheet_name")) %>%
    dplyr::left_join(
      target_cols, by = c("indicator_code" = "indicator_code",
                          "sheet_name" = "sheet_name")) %>%
    dplyr::mutate(
      row = as.integer((1:dplyr::n()) + existing_rows),

  # Accommodate OGAC request to aggregate OVC_HIVSTAT.T across age/sex ####
      id_col = dplyr::case_when(
        indicator_code == "OVC_HIVSTAT.T" ~ "B",
        TRUE ~ id_col),

  # Add DataPackTarget column & classify just that col as formula ####
      DataPackTarget = paste0(
        "SUMIF(",
        sheet_name, "!$", id_col, ":$", id_col,
        ",$F", row,
        ",", sheet_name, "!$", target_col, ":$", target_col, ")")
    ) %>%
    dplyr::select(-id_col, -sheet_name, -target_col, -row)

  class(snuxim_model_data[["DataPackTarget"]]) <- c(class(snuxim_model_data[["DataPackTarget"]]), "formula")

  # Get formulas & column order from schema ####
  interactive_print("Building your custom PSNUxIM tab...")

  data_structure <- datapackr::cop21_data_pack_schema %>%
    dplyr::filter(sheet_name == "PSNUxIM")

  col.im.targets <- data_structure %>%
    dplyr::filter(col_type == "target" & indicator_code %in% c("12345_DSD", "")) %>%
    dplyr::filter(
      indicator_code == "12345_DSD" | col == max(col)) %>%
    dplyr::pull(col)

  col.im.percents <- data_structure %>%
    dplyr::filter(col_type == "allocation" & (indicator_code == "12345_DSD" | is.na(indicator_code))) %>%
    dplyr::filter(
      indicator_code == "12345_DSD" | col == max(col)) %>%
    dplyr::pull(col)

  count.im.datim <- names(snuxim_model_data)[stringr::str_detect(names(snuxim_model_data), "\\d{4,}_(DSD|TA)")] %>%
    length()

  col.formulas <- data_structure %>%
    dplyr::filter(
      !is.na(formula)
      & stringr::str_detect(
        formula,
        paste0("(?<=[:upper:])", top_rows + 1)),
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
        ~stringr::str_replace_all(
          .,
          pattern = paste0("(?<=[:upper:])", top_rows + 1),
          replacement = as.character(seq_len(NROW(snuxim_model_data)) + existing_rows)
          )
        )
      )

  # Classify formula columns as formulas
  ## TODO: Improve approach
  for (i in seq_along(data_structure)) {
    if (!all(any(is.na(data_structure[[i]])))) {
      class(data_structure[[i]]) <- c(class(data_structure[[i]]), "formula")
    }
  }

  # Combine schema with SNU x IM model dataset ####
  #TODO: Fix this to not re-add mechanisms removed by the Country Team (filter snuxim_model_data
  #to only columns with not all NA related to data in missing combos)
  data_structure <- datapackr::swapColumns(data_structure, snuxim_model_data) %>%
    dplyr::bind_cols(
      snuxim_model_data %>%
        dplyr::select(tidyselect::matches("\\d{4,}"))
      )

  header_cols <- datapackr::cop21_data_pack_schema %>%
    dplyr::filter(sheet_name == "PSNUxIM"
                  & col < col.im.percents[1]) %>%
    dplyr::pull(indicator_code)

  IM_cols <- data_structure %>%
    dplyr::select(tidyselect::matches("\\d{4,}")) %>%
    names() %>%
    sort()

  left_side <- data_structure %>%
    dplyr::select(
      tidyselect::all_of(header_cols),
      tidyselect::all_of(IM_cols)
    )

  right_side <- data_structure %>%
    dplyr::select(
      -tidyselect::all_of(names(left_side)),
      -tidyselect::matches("percent_col_\\d{1,3}") #nolint
    )

  # Write data to sheet ####
  interactive_print("Writing your new PSNUxIM data to your Data Pack...")
  d$tool$wb <- openxlsx::loadWorkbook(d$keychain$submission_path)
  openxlsx::removeFilter(d$tool$wb, names(d$tool$wb))

  # Write data to new PSNUxIM tab ####
  openxlsx::writeData(wb = d$tool$wb,
                      sheet = "PSNUxIM",
                      x = right_side,
                      xy = c(col.im.percents[2] + 1, existing_rows + 1),
                      colNames = F, rowNames = F, withFilter = FALSE)

  if (!d$info$has_psnuxim) {
    openxlsx::writeData(wb = d$tool$wb,
                        sheet = "PSNUxIM",
                        x = left_side,
                        xy = c(1, existing_rows),
                        colNames = T, rowNames = F, withFilter = FALSE)
  } else if (d$info$has_psnuxim & d$info$missing_psnuxim_combos) {

  # OR, Append rows to bottom of existing PSNUxIM tab ####
    SNUxIM_cols <-
      readxl::read_excel(
        path = d$keychain$submission_path,
        sheet = "PSNUxIM",
        range = readxl::cell_limits(c(top_rows, 9), c(top_rows, 83)),
        .name_repair = "minimal"
      ) %>%
      names() %>%
      magrittr::extract(., stringr::str_detect(., "\\d{4,}_(DSD|TA)"))

    complete_cols <- c(IM_cols, SNUxIM_cols) %>% unique()

    left_side %<>%
      addcols(complete_cols) %>%
      dplyr::select(tidyselect::all_of(c(header_cols, complete_cols)))

    openxlsx::writeData(wb = d$tool$wb,
                        sheet = "PSNUxIM",
                        x = left_side,
                        xy = c(1, existing_rows + 1),
                        colNames = F, rowNames = F, withFilter = FALSE)

  # Add additional col_names if any
    new_mech_cols <- IM_cols[!IM_cols %in% SNUxIM_cols]
    if (length(new_mech_cols) > 0) {
      openxlsx::writeData(wb = d$tool$wb,
                          sheet = "PSNUxIM",
                          x = new_mech_cols %>% as.matrix() %>% t(),
                          xy = c(8 + length(SNUxIM_cols) + 1, top_rows),
                          colNames = F, rowNames = F, withFilter = FALSE)
    }

  # Add green highlights to appended rows, if any ####
    newRowStyle <- openxlsx::createStyle(fontColour = "#006100", fgFill = "#C6EFCE")

    openxlsx::addStyle(
      wb = d$tool$wb,
      sheet = "PSNUxIM",
      newRowStyle,
      rows = (existing_rows + 1):(existing_rows + NROW(left_side)),
      cols = 1:5,
      gridExpand = TRUE,
      stack = FALSE)
  } else {
   stop("Cannot write data where there seems to be no new data needed.")
  }

  d$info$newSNUxIM <- TRUE

  # Format percent columns ####
  interactive_print("Stylizing percent columns...")

  percentCols <- datapackr::cop21_data_pack_schema %>%
    dplyr::filter(sheet_name == "PSNUxIM",
                  value_type == "percentage") %>%
    dplyr::pull(col)

  percentStyle <- openxlsx::createStyle(numFmt = "0%")

  openxlsx::addStyle(wb = d$tool$wb,
                    sheet = "PSNUxIM",
                    percentStyle,
                    rows = (top_rows + 1):(existing_rows + NROW(data_structure)),
                    cols = percentCols,
                    gridExpand = TRUE,
                    stack = FALSE)

  # Format integers ####
  # integerStyle = openxlsx::createStyle(numFmt = "#,##0")
  #
  # integerCols <- grep("DataPackTarget", final_snuxim_cols)
  #
  # openxlsx::addStyle(
  #   wb = d$tool$wb,
  #   sheet = "PSNUxIM",
  #   integerStyle,
  #   rows = (top_rows + 1):(existing_rows + NROW(data_structure)),
  #   cols = integerCols,
  #   gridExpand = TRUE,
  #   stack = TRUE)


  # Consider adding errorStyling here to emphasize where incorrect disaggs entered.
  # errorStyle <- openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  # warningStyle <- openxlsx::createStyle(fontColour = "#9C5700", bgFill = "#FFEB9C")
  # normalStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FFFFFF")

  # Hide rows 5-13 ####
  interactive_print("Tidying up...")
  openxlsx::setRowHeights(wb = d$tool$wb,
                          sheet = "PSNUxIM",
                          rows = 4:(top_rows - 1),
                          heights = 0)

  # Hide columns ####
  hiddenCols <- datapackr::cop21_data_pack_schema %>%
    dplyr::filter(sheet_name == "PSNUxIM",
                  indicator_code %in% c("ID", "sheet_num", "DSD Dedupe",
                                        "TA Dedupe", "Crosswalk Dedupe")) %>%
    dplyr::pull(col)

  openxlsx::setColWidths(wb = d$tool$wb,
                         sheet = "PSNUxIM",
                         cols = hiddenCols,
                         hidden = TRUE)

  # Tab generation date ####
  openxlsx::writeData(d$tool$wb, "PSNUxIM",
                      paste("Last Updated on:", Sys.time()),
                      xy = c(1, 2),
                      colNames = F)

  # Package Version ####
  openxlsx::writeData(d$tool$wb, "PSNUxIM",
                      paste("Package version:",
                            as.character(utils::packageVersion("datapackr"))),
                      xy = c(2, 2),
                      colNames = F)

  # Warning Messages ####
  interactive_print("Compiling alert messages...")
  warning_msg <-
    paste0(
      "INFO: Based on your submission, we have ",
      ifelse(d$info$has_psnuxim,
             paste0("added ", NROW(data_structure), " rows to your PSNUxIM tab.",
                     " These have been highlighted green for your reference."),
             "populated your PSNUxIM tab for the first time."),
      " An updated copy of your Data Pack is available for download from this app.",
      " Please review your PSNUxIM tab and carefully review the Data Pack User Guide",
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

  d$info$messages <- appendMessage(d$info$messages, warning_msg, "INFO")

  return(d)

}
