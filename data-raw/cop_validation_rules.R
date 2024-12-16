## code to prepare `cop_validation_rules` dataset goes here

require(jsonlite)
require(purrr)
require(dplyr)
require(magrittr)

# login
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

# unhash the following api link and run
# validation rules release is handled along side MER meta data
base_url <- Sys.getenv("BASE_URL")
url <- paste0(base_url, "api/validationRules.json?filter=name:like:TARGET&fields=id,name,periodType,description,operator,leftSide[expression,missingValueStrategy],rightSide[expression,missingValueStrategy]&paging=false")

# hit api and write out file
httr::GET(url, httr::timeout(30),
          handle = d2_default_session$handle,
          httr::write_disk(
            path = "./data-raw/COP25/cop25_validation_rules.json",
            overwrite = TRUE
            )
          )

processValidationRules <- function(r) {
  expression.pattern <- "[a-zA-Z][a-zA-Z0-9]{10}(\\.[a-zA-Z][a-zA-Z0-9]{10})?"

  vr <- jsonlite::fromJSON(r, flatten = TRUE)$validationRules

  # Static predefined map of operators
  op.map <- data.frame(x = c("greater_than_or_equal_to",
                             "greater_than",
                             "equal_to",
                             "not_equal_to",
                             "less_than_or_equal_to",
                             "less_than",
                             "exclusive_pair",
                             "compulsory_pair"),
                       y = c(">=", ">", "==", "!=", "<=", "<", "|", "&"),
                       stringsAsFactors = FALSE)
  # Strategies
  strat.map <- data.frame(x = c("SKIP_IF_ANY_VALUE_MISSING",
                                "SKIP_IF_ALL_VALUES_MISSING",
                                "NEVER_SKIP"))
  # Remap the operators
  vr$operator <- plyr::mapvalues(vr$operator,
                                 op.map$x,
                                 op.map$y,
                                 warn_missing = FALSE)

  # Count the left and right side operators
  vr$rightSide.ops <- stringr::str_count(vr$rightSide.expression,
                                           expression.pattern)

  vr$leftSide.ops <- stringr::str_count(vr$leftSide.expression,
                                          expression.pattern)

  # Remove any line breaks
  vr$leftSide.expression <- stringr::str_replace(vr$leftSide.expression,
                                                   pattern = "\n", "")

  vr$rightSide.expression <- stringr::str_replace(vr$rightSide.expression,
                                                    pattern = "\n", "")

  return(vr)
}

cop23 <- processValidationRules("./data-raw/COP23/cop23_validation_rules.json")
cop24 <- processValidationRules("./data-raw/COP24/cop24_validation_rules.json")
cop25 <- processValidationRules("./data-raw/COP25/cop25_validation_rules.json")

cop_validation_rules <- list(
  "2023" = cop23,
  "2024" = cop24,
  "2025" = cop25
  )

# use waldo to look at the differences in case
waldo::compare(
  cop_validation_rules$`2023`,
  cop_validation_rules$`2024`,
  cop_validation_rules$`2025`
)

usethis::use_data(cop_validation_rules, overwrite = TRUE)
