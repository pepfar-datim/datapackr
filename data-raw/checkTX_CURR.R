# Compare TX_CURR FY20 Targets in submitted Data Pack against DATIM
library(magrittr)
library(dplyr)
library(tidyr)
library(devtools)
library(utils)
library(httr)
library(readxl)
library(tibble)
library(stringr)

# devtools::install_github(repo = "pepfar-datim/datapackr",
#                          ref = "master",
#                          upgrade = TRUE)

library(datapackr)

# Point to DATIM login secrets ####
secrets <- "/Users/scott/.secrets/datim.json"

datimutils::loginToDATIM(secrets)

# Choose file ####
data_pack_filepath <- file.choose()

# Read in data ####
header_row <- datapackr::headerRow("Data Pack")

tx_data_pack <-
  readxl::read_excel(
    path = data_pack_filepath,
    sheet = "TX",
    range = readxl::cell_limits(c(header_row, 1), c(NA, NA)),
    col_types = "text"
  )

# Check for and remove duplicate cols ####
duplicate_cols <- duplicated(names(tx_data_pack))

if (any(duplicate_cols)) {
  tx_data_pack <- tx_data_pack[,-which(duplicate_cols)]
}

# Make sure no blank column names ####
tx_data_pack %<>%
  tibble::as_tibble(.name_repair = "unique") %>%
  
# Grab just TX_CURR FY 20 targets ####
  dplyr::select(PSNU, Age, Sex, TX_CURR.N.Age_Sex_HIVStatus.T_1) %>%

# Drop NAs or 0s ####
  tidyr::drop_na(TX_CURR.N.Age_Sex_HIVStatus.T_1) %>%
  dplyr::filter(TX_CURR.N.Age_Sex_HIVStatus.T_1 != 0) %>%

# Pull out PSNU uid ####
  dplyr::mutate(
    psnuid = stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)")
  ) %>%
  tidyr::pivot_longer(
    cols = TX_CURR.N.Age_Sex_HIVStatus.T_1,
    names_to = "indicator_code",
    values_to = "value_datapack"
  )

# Pull data from DATIM ####
country_uids <- 
  datapackr::unPackCountryUIDs(
    submission_path = data_pack_filepath,
    tool = "Data Pack")

URL <- paste0(
  d2_default_session$base_url,
  "api/29/analytics.csv?dimension=dx:XC0nrb9ZbQR&dimension=ou:OU_GROUP-AVy8gJXym2D;",
  country_uids,
  "&dimension=e485zBiR7vG:Z8MTaDxRBP6;BURHq262iEL;RV1ZeOr98rI;tIZRQs0FK5P;QOawCj9oLNS;BDPEHrovntA;K9Cw4402aAh;JqZFtdn1sG3;ftAnvKhxRxl;kyKHoWxTzHR;MsbFixtB8mu;bePcXLCq9Ov&dimension=jyUTj5YC3OK:hDBPKTjUPDm;ZOYVg7Hosni&filter=pe:2019Oct&outputIdScheme=NAME&columns=dx&rows=ou;e485zBiR7vG;jyUTj5YC3OK"
)

pad <- function(digit) {padded <- paste0("0", digit)}

tx_datim <- URL %>%
  utils::URLencode() %>%
  httr::GET(httr::timeout(180),
            handle = d2_default_session$handle) %>%
  httr::content(., "text") %>%
  readr::read_csv() %>%
  tidyr::pivot_longer(
    cols  = "TX_CURR (DSD+TA, Age/Sex/HIVStatus) TARGET",
    names_to = "indicator_name",
    values_to = "value_datim"
  ) %>%
  dplyr::select(
    psnuid = organisationunitid,
    Age = e485zBiR7vGname, Sex = jyUTj5YC3OKname, indicator_name,
    value_datim) %>%
  dplyr::mutate(
    Age = stringr::str_replace(Age, " \\((Specific|Inclusive)\\)", ""),
    Age = stringr::str_replace_all(Age, "(?<!\\d)\\d(?!\\d)", pad),
    Sex = stringr::str_replace(Sex, "ales", "ale"),
    indicator_code = 
      dplyr::case_when(
        indicator_name == "TX_CURR (DSD+TA, Age/Sex/HIVStatus) TARGET" ~
          "TX_CURR.N.Age_Sex_HIVStatus.T_1",
        TRUE ~ indicator_name
      )
  ) %>%
  tidyr::drop_na(value_datim) %>%
  dplyr::select(-indicator_name)


# Compare ####
tx_comparison <- tx_data_pack %>%
  dplyr::full_join(tx_datim)

tx_diffs <- tx_comparison %>%
  tidyr::replace_na(list(value_datim = 0, value_datapack = 0))%>%
  dplyr::filter(
    value_datapack != value_datim
  )
  
