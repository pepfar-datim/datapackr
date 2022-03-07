## code to prepare `cop_validation_rules` dataset goes here

require(jsonlite)
require(purrr)
require(dplyr)
require(magrittr)

# paste0("https://www.datim.org/api/validationRules.json?
#        filter=name:like:TARGET&fields=id,name,periodType,description,operator,
#        leftSide[expression,missingValueStrategy],
#        rightSide[expression,missingValueStrategy]&paging=false")

processValidationRules <- function(r) {
  expression.pattern <- "[a-zA-Z][a-zA-Z0-9]{10}(\\.[a-zA-Z][a-zA-Z0-9]{10})?"
  vr <- jsonlite::fromJSON(r, flatten = TRUE)$validationRules
  #Static predefined map of operators
  op.map <- data.frame(x = c("greater_than_or_equal_to",
                             "greater_than",
                             "equal_to",
                             "not_equal_to",
                             "less_than_or_equal_to",
                             "less_than",
                             "exclusive_pair",
                             "compulsory_pair"),
                       y = c(">=", ">", "==", "!=", "<=", "<", "|", "&"),
                       stringsAsFactors = F)
  #Strategies
  strat.map <- data.frame(x = c("SKIP_IF_ANY_VALUE_MISSING",
                                "SKIP_IF_ALL_VALUES_MISSING",
                                "NEVER_SKIP"))
  #Remap the operators
  vr$operator <- plyr::mapvalues(vr$operator,
                                 op.map$x,
                                 op.map$y,
                                 warn_missing = FALSE)
  #Count the left and right side operators
  vr$rightSide.ops <- stringr::str_count(vr$rightSide.expression,
                                         expression.pattern)
  vr$leftSide.ops <- stringr::str_count(vr$leftSide.expression,
                                        expression.pattern)
  #Remove any line breaks
  vr$leftSide.expression <- stringr::str_replace(vr$leftSide.expression,
                                                 pattern = "\n", "")
  vr$rightSide.expression <- stringr::str_replace(vr$rightSide.expression,
                                                  pattern = "\n", "")
  return(vr)
}

cop20 <- processValidationRules("./data/cop20_validation_rules.json")
cop21 <- processValidationRules("./data/cop21_validation_rules.json")
cop22 <- processValidationRules("./data/cop22_validation_rules.json")


cop_validation_rules <- list("2020" = cop20, "2021" = cop21, "2022" = cop22)

usethis::use_data(cop_validation_rules, overwrite = TRUE)
