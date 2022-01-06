#' @export
#' @importFrom magrittr %>% %<>%
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
packPSNUxIM <- function(wb,# Workbook object
                        data,
                        snuxim_model_data,
                        cop_year = NULL, # Cop year based on the file
                        tool = "OPU Data Pack",
                        schema = NULL,
                        d2_session = dynGet("d2_default_session",
                                            inherits = TRUE)) {

  # Check/Fill in parameters ####
  params <- check_params(cop_year = cop_year,
                         tool = tool,
                         schema = schema)

  ps <- c("cop_year", "tool", "schema")

  for (p in ps) {
    assign(p, purrr::pluck(params, p)) # Allows indexing similar to [[x]]
  }

  if (!cop_year %in% c(2021)) {# if the cop year is not 2021, stops and throws message. #Ask Scott, do we need to add 2022 or change? I know the plan is to spin this off.
    stop(paste0("Packing PSNU x IM tabs is not supported for COP", cop_year, " Data Packs."))
  }

  if (tool != "OPU Data Pack") {
    stop("Sorry, this function currently only works for COP21 OPU Data Packs.")
  }

  # Create data sidecar to eventually compile and return ####
  r <- list(
    wb = wb,#Workbook object xlsx
    info = list(messages = MessageQueue(), #Found in messagesQueue.R
    has_error = FALSE))

  #TODO: Test/write this part to be compatible with COP Data Pack

  # # Check if SNUxIM data already exists ####
  # if (NROW(d$data$SNUxIM) == 1 & is.na(d$data$SNUxIM$PSNU[1])) {
  #   d$info$has_psnuxim <- FALSE
  # } else {d$info$has_psnuxim <- TRUE}
  #
  # # If does exist, extract missing combos ####
  # if (d$info$has_psnuxim) {
  #   d$data$missingCombos <- d$data$MER %>%
  #     # TODO: Create this here rather than upstream
  #     dplyr::anti_join(d$data$PSNUxIM_combos)
  #
  #   d$info$missing_psnuxim_combos <- (NROW(d$data$missingCombos) > 0)
  # }
  #
  # # Proceed IFF no PSNU x IM tab exists, or exists but with missing combos ####
  # if (d$info$has_psnuxim & !d$info$missing_psnuxim_combos) {
  #   return(d)
  # }
  #
  # # Prepare SNU x IM model dataset ####
  # if (d$info$has_psnuxim & d$info$missing_psnuxim_combos) {
  #   targets_data <- d$data$missingCombos
  # } else {
  #   targets_data <- d$data$MER
  # }

  # Prepare SNUxIM model data
  snuxim_model_data %<>%
    datapackr::adorn_import_file(cop_year = cop_year, #adorn_import_file.R
                                 # Final data in the new, more complete format?
                                 filter_rename_output = FALSE) %>% 
    # Select columns wanted and rename where necessary
    dplyr::select(indicator_code, psnu_uid = orgUnit, mechanism_code, 
                  type = support_type,
                  age_option_name = Age, age_option_uid = valid_ages.id,
                  sex_option_name = Sex, sex_option_uid = valid_sexes.id,
                  kp_option_name = KeyPop, kp_option_uid = valid_kps.id,
                  value) %>%
    dplyr::group_by(dplyr::across(c(-mechanism_code, -type, -value))) %>%
    dplyr::mutate(
      percent = value / sum(value) #Creates percent column
    ) %>%
    dplyr::ungroup() %>% #Opposite of group_by. Ungroups the data
    dplyr::arrange(indicator_code, psnu_uid, age_option_name, sex_option_name,
                   kp_option_name, mechanism_code, type) #Put columns in desired order
  # Prints during execution to inform the user. 
  interactive_print("Getting data about your FY21 Mechanism Allocations from DATIM...") #Ask Scott or Jason is this okay to stay 21?

  # Drop data that can't be allocated across mechanism codes & DSD/TA
  snuxim_model_data %<>%
    dplyr::filter(stringr::str_detect(mechanism_code, "\\d{4,}"), # Regex digits
                  stringr::str_detect(type, "DSD|TA")) #Regex for DSDITA

  # Pivot mechs/type wider
  snuxim_model_data %<>%
    tidyr::unite(col = mechcode_supporttype, mechanism_code, type) %>% #Merges the 2 columns into 1 named mechcode_supporttype
    dplyr::select(psnu_uid, indicator_code, Age = age_option_name,
                  Sex = sex_option_name, KeyPop = kp_option_name,
                  mechcode_supporttype, percent, value) %>% #Only keeps these columns
    dplyr::mutate(
      mechcode_supporttype = dplyr::case_when( #converts certain mech codes. 
        mechcode_supporttype == "00000_DSD" ~ "DSD Dedupe",
        mechcode_supporttype == "00000_TA" ~ "TA Dedupe",
        mechcode_supporttype == "00001_TA" ~ "Crosswalk Dedupe",
        TRUE ~ mechcode_supporttype
      )
    )

  percents <- snuxim_model_data %>%
    dplyr::select(-value) %>% #Drops value column
    tidyr::pivot_wider(names_from = mechcode_supporttype, #pivots data to be wide with more columns
                       values_from = percent)

  values <- snuxim_model_data %>%
    dplyr::select(-percent, -mechcode_supporttype) %>% #Drops these columns
    dplyr::group_by(dplyr::across(c(-value))) %>%
    dplyr::summarise(value = sum(value)) %>% #Summarize based upon values
    dplyr::ungroup()

  #Throws a warning to the user if the number rows do not match after munging.
  if (NROW(percents) != NROW(values)) {
    stop("Aggregating values and percents led to different row counts!")
  }

  snuxim_model_data <- values %>% #Joins percents to values 
    dplyr::left_join(percents,
                     by = c("psnu_uid", "indicator_code", "Age", "Sex", "KeyPop"))

  # EID: Align model data age bands with Data Pack
  snuxim_model_data %<>%
    dplyr::mutate(
      Age = dplyr::if_else( #If age contains the below values place NA. 
        indicator_code %in% c("PMTCT_EID.N.2.T", "PMTCT_EID.N.12.T"),
        NA_character_,
        Age
      )
    )

  # Double check that Dedupe cols all exist as expected
  snuxim_model_data %<>% #Adds the below columns to snuxim_model_data
    datapackr::addcols(cnames = c("DSD Dedupe",
                                  "TA Dedupe",
                                  "Crosswalk Dedupe"),
                       type = "numeric")

  # Create Deduplicated Rollups
  snuxim_model_data %<>%
    dplyr::mutate(
      #Regex looks for 4 digits or the string "HllvX50cXC0"
      `Total Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}|HllvX50cXC0")), na.rm = TRUE),
      #Regex looks for 4digits followed by _DSD
      `DSD Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}_DSD")), na.rm = TRUE),
      #Regex looks for 4digits followed by _TA
      `TA Duplicated Rollup` = rowSums(dplyr::select(., tidyselect::matches("\\d{4,}_TA")), na.rm = TRUE))

  # Create Duplicated Rollups
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

  # Create Max columns
  snuxim_model_data %<>% #rowMax found in utilities.R
    datapackr::rowMax(cn = "Max_TA.T_1", regex = "\\d{4,}_TA") %>% # nolint
    datapackr::rowMax(cn = "Max_DSD.T_1", regex = "\\d{4,}_DSD") %>% # nolint
    dplyr::mutate(
      `Max_Crosswalk.T_1` =
        pmax(`Deduplicated DSD Rollup`, `Deduplicated TA Rollup`, na.rm = T))

  # Create Dedupe Resolution columns. Prints for user to see what is occurring
  interactive_print("Studying your deduplication patterns...")

  snuxim_model_data %<>%
    dplyr::rowwise() %>%
    dplyr::mutate(ta_im_count = sum(!is.na(dplyr::c_across(tidyselect::matches("\\d{4,}_TA")))), # nolint
                  dsd_im_count = sum(!is.na(dplyr::c_across(tidyselect::matches("\\d{4,}_DSD"))))) %>% # nolint
    dplyr::ungroup() %>%
    dplyr::mutate(
      `TA Dedupe Resolution (FY22)` = dplyr::case_when(
        `TA Duplicated Rollup` == 0 | ta_im_count <= 1 ~ NA_character_,
        # or where count(TA IMs) == 1
        `Deduplicated TA Rollup` == `TA Duplicated Rollup` ~ "SUM",
        `Deduplicated TA Rollup` == `Max_TA.T_1` ~ "MAX",
        TRUE ~ "CUSTOM"),
      `DSD Dedupe Resolution (FY22)` = dplyr::case_when(
        `DSD Duplicated Rollup` == 0 | dsd_im_count <= 1 ~ NA_character_,
        `Deduplicated DSD Rollup` == `DSD Duplicated Rollup` ~ "SUM",
        `Deduplicated DSD Rollup` == `Max_DSD.T_1` ~ "MAX",
        TRUE ~ "CUSTOM"),
      `Crosswalk Dedupe Resolution (FY22)` = dplyr::case_when(
        `Total Duplicated Rollup` == 0 | `Deduplicated TA Rollup` == 0 | `Deduplicated DSD Rollup` == 0
        ~ NA_character_,
        `Total Deduplicated Rollup` == `Total Duplicated Rollup` ~ "SUM",
        `Total Deduplicated Rollup` == `Max_Crosswalk.T_1` ~ "MAX",
        TRUE ~ "CUSTOM"),
      `Custom DSD Dedupe Allocation (FY22) (% of DataPackTarget)` = `DSD Dedupe`,
      `Custom TA Dedupe Allocation (FY22) (% of DataPackTarget)` = `TA Dedupe`,
      `Custom Crosswalk Dedupe Allocation (FY22) (% of DataPackTarget)` = `Crosswalk Dedupe`
    ) %>%
    dplyr::select(psnu_uid, indicator_code, Age, Sex, KeyPop,
                  tidyselect::matches("\\d{4,}"), # nolint
                  `Custom DSD Dedupe Allocation (FY22) (% of DataPackTarget)`,
                  `Custom TA Dedupe Allocation (FY22) (% of DataPackTarget)`,
                  `Custom Crosswalk Dedupe Allocation (FY22) (% of DataPackTarget)`,
                  `DSD Dedupe Resolution (FY22)`,
                  `TA Dedupe Resolution (FY22)`,
                  `Crosswalk Dedupe Resolution (FY22)`,
                  `DSD Dedupe`, `TA Dedupe`, `Crosswalk Dedupe`)

  # Prep dataset of targets to allocate ####
  data %<>% #function found in adorn_import_file.R
    adorn_import_file(cop_year = cop_year, filter_rename_output = FALSE) %>%
    dplyr::select(PSNU = dp_psnu, orgUnit, indicator_code, Age, Sex, KeyPop,
                  DataPackTarget = value) %>%
    dplyr::group_by(dplyr::across(c(-DataPackTarget))) %>%
    dplyr::summarise(DataPackTarget = sum(DataPackTarget)) %>%
    dplyr::ungroup()

  # Do not include AGYW_PREV -- These are not allocated to IMs
  data %<>%
    dplyr::filter(!indicator_code %in% c("AGYW_PREV.N.T", "AGYW_PREV.D.T"))

  # Filter SNU x IM model dataset to only those data needed in tab ####
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
      datapackr::addcols(cnames = c("Custom DSD Dedupe Allocation (FY22) (% of DataPackTarget)",
                                    "Custom TA Dedupe Allocation (FY22) (% of DataPackTarget)",
                                    "Custom Crosswalk Dedupe Allocation (FY22) (% of DataPackTarget)"),
                         type = "numeric") %>%
      datapackr::addcols(cnames = c("DSD Dedupe Resolution (FY22)",
                                    "TA Dedupe Resolution (FY22)",
                                    "Crosswalk Dedupe Resolution (FY22)"),
                         type = "character")
  }

# Found in packageSetup.R
  top_rows <- headerRow(tool = tool, cop_year = cop_year)
  existing_rows <- top_rows

  # Add DataPackTarget to non-OPU Data Pack ####
  # TODO: Test and write for COP Data Packs
  # if (tool == "Data Pack") {
    # if (d$info$has_psnuxim) {
    #   existing_rows <-
    #     readxl::read_excel(
    #       path = d$keychain$submission_path,
    #       sheet = "PSNUxIM",
    #       range = readxl::cell_limits(c(1, 2), c(NA, 2)),
    #       col_names = F,
    #       .name_repair = "minimal"
    #     ) %>%
    #     NROW()
    # }

  #   interactive_print("Analyzing targets set across your Data Pack...")
  #
  #
  #
  #   get_ID_col <- function(data) {
  #     col_letter <- data %>%
  #       dplyr::filter(indicator_code == "ID")
  #
  #     if (NROW(col_letter) == 0) {
  #       col_letter <- data %>%
  #         dplyr::filter(indicator_code == "PSNU")}
  #
  #     col_letter %<>%
  #       dplyr::pull(submission_order) %>%
  #       openxlsx::int2col()
  #
  #     return(col_letter)
  #   }
  #
  #   id_cols <- lapply(d$info$col_check, get_ID_col) %>%
  #     dplyr::bind_rows() %>%
  #     t() %>%
  #     as.data.frame(stringsAsFactors = FALSE) %>%
  #     dplyr::rename(id_col = V1) %>%
  #     tibble::rownames_to_column("sheet_name")
  #
  #   target_cols <- datapackr::cop21_data_pack_schema %>%
  #     dplyr::filter(dataset == "mer" & col_type == "target" & (!sheet_name %in% c("PSNUxIM", "AGYW"))) %>%
  #     dplyr::mutate(
  #       target_col = openxlsx::int2col(col)
  #     ) %>%
  #     dplyr::select(sheet_name, indicator_code, target_col)
  #
  #   snuxim_model_data %<>%
  #     dplyr::left_join(
  #       id_cols, by = c("sheet_name" = "sheet_name")) %>%
  #     dplyr::left_join(
  #       target_cols, by = c("indicator_code" = "indicator_code",
  #                           "sheet_name" = "sheet_name")) %>%
  #     dplyr::mutate(
  #       row = as.integer((1:dplyr::n()) + existing_rows),
  #
  #       # Accommodate OGAC request to aggregate OVC_HIVSTAT.T across age/sex ####
  #       id_col = dplyr::case_when(
  #         indicator_code == "OVC_HIVSTAT.T" ~ "B",
  #         TRUE ~ id_col),
  #
  #       # Add DataPackTarget column & classify just that col as formula ####
  #       DataPackTarget = paste0(
  #         'SUMIF(',
  #         sheet_name, '!$', id_col, ':$', id_col,
  #         ', $F', row,
  #         ', ', sheet_name, '!$', target_col, ':$', target_col, ')')
  #     ) %>%
  #     dplyr::select(-id_col, -sheet_name, -target_col, -row)
  #
  #   class(snuxim_model_data[["DataPackTarget"]]) <- c(class(snuxim_model_data[["DataPackTarget"]]), "formula")
  # }

  # Get formulas & column order from schema ####
  interactive_print("Building your custom PSNUxIM tab...")

  data_structure <- schema %>%
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
      !is.na(formula),
      col < (col.im.targets[1])) %>%
    dplyr::pull(col)

  ## TODO: Improve this next piece to be more efficient instead of using str_replace_all.
  ## #We could use map, but I don't think a performance boost will be realized? 

  data_structure %<>%
    dplyr::arrange(col) %>% #Arrange rows based upon col values
    dplyr::mutate(#Sets column names based upon col.im.percents values
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
    tibble::as_tibble() %>% #make tibble
    ## Setup formulas
    dplyr::slice(rep(1:dplyr::n(), times = NROW(snuxim_model_data))) %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(col.formulas),
                    ~stringr::str_replace_all(., pattern = paste0("(?<=[:upper:])", top_rows + 1),
                      replacement = as.character(seq_len(NROW(snuxim_model_data)) + existing_rows))))

  # Classify formula columns as formulas
  ## Not sure if my approach is better, but is more readable. 
  for (i in seq_along(data_structure)) {#Iterates over each column
    # checks the values of each column to see if any NA's exist in them,
    # Then adds the trues up. 
    # TLDR; If it contains any NA's skip and go to the next column. 
    if (sum(is.na(data_structure[[i]])) < 1) {
      # IF so set the class of the column to (col value, formula)
      class(data_structure[[i]]) <- c(class(data_structure[[i]]),
                                      "formula")
    }
  }

  # Combine schema with SNU x IM model dataset ####
  #TODO: Fix this to not re-add mechanisms removed by the Country Team
  #(filter snuxim_model_data to only columns with not all NA related to data in missing combos)
  data_structure <- datapackr::swapColumns(data_structure, snuxim_model_data) %>% 
    #swapColumns found in utilities.R
    dplyr::bind_cols(
      snuxim_model_data %>%
        # Regex matches string that start with 4 digits. Note this can mean
        # more than 4, just has to start with ####
        dplyr::select(tidyselect::matches("\\d{4,}")) # nolint
    )

  header_cols <- schema %>%
    dplyr::filter(sheet_name == "PSNUxIM"
                  & col < col.im.percents[1]) %>%
    dplyr::pull(indicator_code)

  IM_cols <- data_structure %>%
    # Regex matches string that start with 4 digits. Note this can mean
    # more than 4, just has to start with ####
    dplyr::select(tidyselect::matches("\\d{4,}")) %>% # nolint
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
      # Regex matches string that start with 1 to 3 digits. Note this can mean
      # 1 will be matched and 111, but 1111 will be considered two matches.
      -tidyselect::matches("percent_col_\\d{1,3}") # nolint
    )

  # Write data to sheet ####
  interactive_print("Writing your new PSNUxIM data to your Data Pack...")
  r$wb %<>% openxlsx::removeFilter(names(.))

  # Write data to new PSNUxIM tab
  openxlsx::writeData(wb = r$wb,
                      sheet = "PSNUxIM",
                      x = right_side,
                      xy = c(col.im.percents[2] + 1, existing_rows + 1),
                      colNames = F, rowNames = F, withFilter = FALSE)

  # if (!d$info$has_psnuxim) {
    openxlsx::writeData(wb = r$wb,
                        sheet = "PSNUxIM",
                        x = left_side,
                        xy = c(1, existing_rows),
                        colNames = T, rowNames = F, withFilter = FALSE)

    #TODO: Test/write for COP Data Pack appending
  # } else if (d$info$has_psnuxim & d$info$missing_psnuxim_combos) {
  #
  #   # OR, Append rows to bottom of existing PSNUxIM tab ####
  #   SNUxIM_cols <-
  #     readxl::read_excel(
  #       path = d$keychain$submission_path,
  #       sheet = "PSNUxIM",
  #       range = readxl::cell_limits(c(top_rows, 9), c(top_rows, 83)),
  #       .name_repair = "minimal"
  #     ) %>%
  #     names() %>%
  #     magrittr::extract(., stringr::str_detect(., "\\d{4,}_(DSD|TA)"))
  #
  #   complete_cols <- c(IM_cols, SNUxIM_cols) %>% unique()
  #
  #   left_side %<>%
  #     addcols(complete_cols) %>%
  #     dplyr::select(tidyselect::all_of(c(header_cols, complete_cols)))
  #
  #   openxlsx::writeData(wb = d$tool$wb,
  #                       sheet = "PSNUxIM",
  #                       x = left_side,
  #                       xy = c(1, existing_rows+1),
  #                       colNames = F, rowNames = F, withFilter = FALSE)
  #
  #   # Add additional col_names if any
  #   new_mech_cols <- IM_cols[!IM_cols %in% SNUxIM_cols]
  #   if (length(new_mech_cols) > 0) {
  #     openxlsx::writeData(wb = d$tool$wb,
  #                         sheet = "PSNUxIM",
  #                         x = new_mech_cols %>% as.matrix() %>% t(),
  #                         xy = c(8+length(SNUxIM_cols)+1, top_rows),
  #                         colNames = F, rowNames = F, withFilter = FALSE)
  #   }
  #
  #   # Add green highlights to appended rows, if any
  #   newRowStyle <- openxlsx::createStyle(fontColour = "#006100", fgFill = "#C6EFCE")
  #
  #   openxlsx::addStyle(
  #     wb = d$tool$wb,
  #     sheet = "PSNUxIM",
  #     newRowStyle,
  #     rows = (existing_rows + 1):(existing_rows + NROW(left_side)),
  #     cols = 1:5,
  #     gridExpand = TRUE,
  #     stack = FALSE)
  # } else {
  #   stop("Cannot write data where there seems to be no new data needed.")
  # }

  # d$info$newSNUxIM <- TRUE

  # Formatting ####

    # Format percent columns
  interactive_print("Stylizing percent columns...")

  percentCols <- schema %>%
    dplyr::filter(sheet_name == "PSNUxIM",
                  value_type == "percentage") %>%
    dplyr::pull(col)

  percentStyle <- openxlsx::createStyle(numFmt = "0%")

  openxlsx::addStyle(wb = r$wb,
                     sheet = "PSNUxIM",
                     percentStyle,
                     rows = (top_rows + 1):(existing_rows + NROW(data_structure)),
                     cols = percentCols,
                     gridExpand = TRUE,
                     stack = FALSE)

    # Format integers
  # integerStyle = openxlsx::createStyle(numFmt = "#,##0") # nolint
  #
  # integerCols <- grep("DataPackTarget", final_snuxim_cols)
  #q
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

    # Hide rows 5-13 in the workbook
  interactive_print("Tidying up...")
  openxlsx::setRowHeights(wb = r$wb,
                          sheet = "PSNUxIM",
                          rows = 4:(top_rows - 1),
                          heights = 0)

  # Hide columns
  #TODO: Hide columns in percentage section being unused by IMs
  hiddenCols <- schema %>%
    dplyr::filter(sheet_name == "PSNUxIM",
                  indicator_code %in% c("ID", "sheet_num", "DSD Dedupe",
                                        "TA Dedupe", "Crosswalk Dedupe")) %>%
    dplyr::pull(col)

  openxlsx::setColWidths(wb = r$wb,
                         sheet = "PSNUxIM",
                         cols = hiddenCols,
                         hidden = TRUE)

  # Tab generation date ####
  openxlsx::writeData(r$wb, "PSNUxIM",
                      paste("Last Updated on:", Sys.time()),
                      xy = c(1, 2),
                      colNames = F)

  # Package Version ####
  openxlsx::writeData(r$wb, "PSNUxIM",
                      paste("Package version:",
                            as.character(utils::packageVersion("datapackr"))),
                      xy = c(2, 2),
                      colNames = F)

  # Warning Messages ####
  interactive_print("Compiling alert messages...")
  warning_msg <-
    paste0(
      "INFO: Based on your submission, we have ",
      # ifelse(d$info$has_psnuxim,
      #        paste0("added ", NROW(data_structure), " rows to your PSNUxIM tab.",
      #               " These have been highlighted green for your reference."),
             "populated your PSNUxIM tab for the first time." #),
      ,
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

  r$info$messages <- appendMessage(r$info$messages, warning_msg, "INFO")

  return(r)

}
