#' @export
#' @title Evaluate Indicators
#'
#' @param combis A vector of data element and category option combo UIDs
#' of the form #{dataelement_id.categoryoptioncombo_id}
#' @param values A vector of values
#' @param inds A dataframe consisting of indicator UIDs, name,
#' numerator, and denominator expression.
#'
#' @return
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
    dplyr::summarise(values = sum(values)) %>%
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

  #Function to substitute values based on the
  #dataelement_id.categoryoptioncombo_id
  replaceCombisWithValues <- function(x,
                                         expressions = combis,
                                         v = values) {
    # TODO: replace with `stringr` function to remove `stringi` dependency
    stringi::stri_replace_all_fixed(x,
                                    expressions,
                                    v,
                                    vectorize_all = FALSE)
  }

  # Function to replace missing totals with zeros
  replaceTotalsWithValues <- function(x) {
    replaceCombisWithValues(x,
                               expressions = totals_df$exp,
                               v = totals_df$values)
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
    purrr::modify_at(., c("numerator", "denominator"), replaceTotalsWithValues) %>%
    purrr::modify_at(., c("numerator", "denominator"), replaceExpressionsWithZeros) %>%
    purrr::modify_at(., c("numerator", "denominator"), evaluateExpression) %>%
    dplyr::mutate(value = numerator / denominator)

}
