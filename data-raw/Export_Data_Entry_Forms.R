# Pull dataEntryForms for a given dataSet and export all dataElement,
# categoryOptionCombo pairs

# Method using datimutils ####
# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "cop-test.json")
datimutils::loginToDATIM(secrets)
entryform_id <- "eeZpSGQu8hx"
data <- datimutils::getMetadata(end_point = "dataEntryForms",
                                paste0("id:eq:", entryform_id),
                                fields = "htmlCode") %>%
  stringr::str_extract_all("[[:alpha:]][[:alnum:]]{10}-[[:alpha:]][[:alnum:]]{10}") %>%
  unlist() %>%
  tibble::tibble(pairs = .) %>%
  tidyr::separate(pairs, c("dataElement", "categoryOptionCombo")) %>%
  dplyr::mutate(
    dataElementName = datimutils::getDataElements(values = dataElement),
    categoryOptionComboName = datimutils::getCatOptionCombos(values = categoryOptionCombo)
  )


# Method without datimutils ####
# username <- readline("Username: ")
# password <- getPass::getPass()
#
# base_url <- "https://cop-test.datim.org/"
# url <- utils::URLencode(URL = paste0(base_url, "api", "/me"))
# handle <- httr::handle(base_url)
#
# # Logging in here will give us a cookie to reuse
# r <- httr::GET(
#   url,
#   httr::authenticate(username,
#                      password),
#   httr::timeout(60),
#   handle = handle
# )
#
# URL <- paste0(
#   base_url, "api/dataEntryForms/eeZpSGQu8hx.json?paging=false") %>%
#   utils::URLencode()
#
# data <- URL %>%
#   httr::GET(httr::timeout(180),
#             handle = handle) %>%
#   httr::content(., "text") %>%
  # stringr::str_extract_all("[[:alpha:]][[:alnum:]]{10}-[[:alpha:]][[:alnum:]]{10}") %>%
  # unlist() %>%
  # tibble::tibble(pairs = .) %>%
  # tidyr::separate(pairs, c("dataElement", "categoryOptionCombo"))
