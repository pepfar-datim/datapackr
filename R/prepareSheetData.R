#' @export
#' @title Prepare sheet-specific dataset for writing into a Data Pack sheet.
#'
#' @description
#' Prepares provided dataset for writing into specified Data Pack sheet.
#'
#' @param sheet Specified sheet within wb.
#' @param org_units Dataset of org units to include.
#' @param schema Defaults to standard Data Pack schema, but allows for provision
#' of custom schema if needed.
#' @param sheet_data Data to prepare.
#' @param cop_year COP year for dating as well as selection of
#' templates.
#'
#' @return dataframe of data prepared for Data Pack
#'
prepareSheetData <- function(sheet,
                             org_units,
                             schema = pick_schema(),
                             sheet_data,
                             cop_year = getCurrentCOPYear()) {

  # Get valid disaggs ####
  valid_disaggs <- schema %>%
    dplyr::filter(data_structure == "normal"
                  & sheet_name == sheet
                  & col_type == "target")

  # If there are valid_disaggs then format into their own individual cols ####
  if (NROW(valid_disaggs) > 0) {
    valid_disaggs %<>%
      dplyr::select(valid_ages, valid_sexes, valid_kps) %>%
      unique() %>%
      tidyr::unnest(valid_ages, names_sep = ".") %>%
      tidyr::unnest(valid_sexes, names_sep = ".") %>%
      tidyr::unnest(valid_kps, names_sep = ".") %>%
      unique() %>%
      dplyr::rename(Age = valid_ages.name,
                    Sex = valid_sexes.name,
                    KeyPop = valid_kps.name) %>%
      dplyr::arrange(Age, Sex, KeyPop)
  } else {
    # If there are not valid_disaggs then impute blank char columns ####
    valid_disaggs <- tibble::tribble(
      ~ Age,
      ~ Sex,
      ~ KeyPop,
      ~ valid_ages.id,
      ~ valid_sexes.id,
      ~ valid_kps.id,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    )
  }

  # Cross PSNUs and disaggs ####
  row_headers <- org_units %>%
    tidyr::crossing(valid_disaggs) %>%
    dplyr::mutate(
      AgeCoarse = dplyr::case_when(
        sheet == "OVC" ~ dplyr::case_when(
          Age %in% c("<01", "01-04", "05-09", "10-14", "15-17", "<18") ~ "<18",
          Age %in% c("18-24", "25+", "18+", "18-20") ~ "18+"),
        TRUE ~ dplyr::case_when(
          Age %in% c("<01", "01-04", "05-09", "10-14", "<15") ~ "<15",
          Age %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+", "15+") ~ "15+")
      )
    ) %>%
    dplyr::select(
      SNU1 = snu1, PSNU, Age, Sex, KeyPop, AgeCoarse,
      psnu_uid, valid_ages.id, valid_sexes.id, valid_kps.id) %>%
    dplyr::arrange_at(dplyr::vars(dplyr::everything()))

  # Setup data structure ####
  dataStructure <- schema %>% # Start with base schema
    dplyr::filter(sheet_name == sheet) %>% # Filter by sheet name
    dplyr::arrange(col) %>% # Arrange the data based upon columns
    `row.names<-`(.$indicator_code) %>%
    dplyr::select(formula) %>%
    t() %>%
    tibble::as_tibble() %>%
    ## Setup formulas by modifying char columns
    dplyr::slice(rep(seq_len(dplyr::n()), times = NROW(row_headers))) %>%
    dplyr::mutate_if(
      is.character,
      stringr::str_replace_all,
      pattern = paste0("(?<=[:upper:])", headerRow(tool = "Data Pack Template", cop_year = cop_year) + 1),
      replacement = as.character(seq_len(NROW(row_headers))
        + headerRow(tool = "Data Pack Template", cop_year = cop_year)))

  # Classify formula columns as formulas
  for (i in seq_along(dataStructure)) { # Iterates over each column # seq_along(dataStructure)
    if (sum(is.na(dataStructure[[i]])) < 1) {
      # !all(any(is.na(dataStructure[[i]])))) # For each column, Check the col values for NAs;
                                              # Returns list of T F, Check if any Trues exist,
                                              # Check if all of the values are NOT True
      # IF so set the class of the column to (col value, formula)
      class(dataStructure[[i]]) <- c(class(dataStructure[[i]]), "formula")
    }
  }

  # Adjust KP_MAT data to fit inside KP tab ####
  if (sheet == "KP") {
   sheet_data %<>%
     dplyr::mutate(
       kp_option_uid =
         dplyr::case_when(
           sex_option_uid == "Qn0I5FbKQOA" ~ "wyeCT63FkXB", # Male -> Male PWID
           sex_option_uid == "Z1EnpTPaUfq" ~ "G6OYSzplF5a", # Female -> Female PWID
           TRUE ~ kp_option_uid # If neither are true impute kp_option_uid
         ),
       sex_option_uid = NA_character_ # Transforms col 'sex_option_uid' into NA's
     )
  }

  if (sheet == "OVC") {
    DREAMS_FLAG <- sheet_data %>%
      dplyr::filter(indicator_code == "DREAMS_SNU.Flag") %>%
      tidyr::spread(key = indicator_code,
                    value = value) %>%
      dplyr::select(-age_option_uid, -sex_option_uid, -kp_option_uid)

    if (NROW(DREAMS_FLAG) > 0) {
      DREAMS_FLAG <-
        dplyr::mutate(DREAMS_FLAG,
                      DREAMS_SNU.Flag = as.character(DREAMS_SNU.Flag),
                      DREAMS_SNU.Flag = stringr::str_replace(DREAMS_SNU.Flag, "1", "Y"))
    }

    sheet_data %<>%
      dplyr::filter(indicator_code != "DREAMS_SNU.Flag")
  }

  # Swap in model data ####
  if (!is.null(sheet_data)) {
    sheet_data_spread <- sheet_data %>%
      tidyr::spread(key = indicator_code,
                    value = value)

    combined <- row_headers %>%
      dplyr::left_join(
        sheet_data_spread,
        by = c("psnu_uid" = "psnu_uid",
               "valid_ages.id" = "age_option_uid",
               "valid_sexes.id" = "sex_option_uid",
               "valid_kps.id" = "kp_option_uid"))

    if (sheet == "OVC") {
      combined %<>%
        dplyr::left_join(
          DREAMS_FLAG, by = c("psnu_uid" = "psnu_uid"))
    }

  } else {
    combined <- row_headers
  }

  dataStructure %<>%
    swapColumns(., combined) %>%
    as.data.frame(.)

  return(dataStructure)

}
