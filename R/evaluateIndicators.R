#' @export
#' @title Evaluate Indicators
#'
#' @param combis A vector of data element and category option combo UIDs
#' of the form #{dataelement_id.categoryoptioncombo_id}
#' @param values A vector of values
#' @param inds A dataframe consisting of indicator UIDs, name,
#' numerator, and denominator expression.
#'
#' @return Data frame of ids, names, numerators, denominators, and values
#'
evaluateIndicators <- function(combis, values, inds) {

  if (is.null(inds)) {
    stop("No indicator metadata found")
  }

  indicators_empty <- data.frame(id = character(),
                                 name = character(),
                                 numerator = numeric(),
                                 denominator = numeric(),
                                 value = numeric())

  #Get a vector of all data elements which exist in the supplied values
  this.des <-
    vapply(combis, function(x) {
      unlist(strsplit(x, "\\."))[[1]]
    }, FUN.VALUE = character(1))

  #Calculate data element totals
  totals_df <-
    data.frame(exp = this.des, values = values, stringsAsFactors = FALSE) %>%
    dplyr::group_by(exp) %>%
    dplyr::summarise(values = as.character(sum(values))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(exp = paste0(exp, "}"))

  #Define a function for any expressions which match
  #either the numerator or denominator
  matchesIndicator <- function(x) {
    agrepl(x, inds$numerator) |
      agrepl(x, inds$denominator)
  }

  #Determine which indicator formulas match our supplied data
  matches <- this.des %>%
    unique(.) %>%
    purrr::map(., matchesIndicator) %>%
    Reduce("|", .) %>%
    dplyr::filter(inds, .)

  #Return something empty here if we have no indicator matches
  if (nrow(matches) == 0) {
    return(indicators_empty)
  }

  escapeIndicatorOrnamentation <- function(x) {
    x  %>%
      stringr::str_replace_all("[.]", "\\\\.") %>%
      stringr::str_replace_all("[{]", "\\\\{") %>%
      stringr::str_replace_all("[}]", "\\\\}")
  }

  #Append totals
  combis <- c(combis, totals_df$exp)
  values <- c(values, totals_df$values)

  #Must escape the ornamentation, as these will be substituted
  combis <- escapeIndicatorOrnamentation(combis)

  #Function to substitute values based on the
  #dataelement_id.categoryoptioncombo_id
  replaceCombisWithValues <- function(x,
                                         expressions = combis,
                                         v = values) {
    for (i in seq_along(expressions)) {
       x <- stringr::str_replace_all(x, expressions[i], as.character(values[i]))
    }
    x
  }


  #Function to replace missing combis with zeros
  replaceExpressionsWithZeros <- function(x) {
    expression.pattern <- "#\\{[a-zA-Z][a-zA-Z0-9]{10}(\\.[a-zA-Z][a-zA-Z0-9]{10})?\\}"
    gsub(expression.pattern, "0", x)
  }

  #Function to parse and evaluate the expression to return a numeric value
  evaluateExpression <- function(exp) {
    vapply(exp, function(x) eval(parse(text = x)), FUN.VALUE = double(1))
  }

  matches %>%
    purrr::modify_at(., c("numerator", "denominator"), replaceCombisWithValues) %>%
    purrr::modify_at(., c("numerator", "denominator"), replaceExpressionsWithZeros) %>%
    purrr::modify_at(., c("numerator", "denominator"), evaluateExpression) %>%
    dplyr::mutate(value = numerator / denominator)

}
