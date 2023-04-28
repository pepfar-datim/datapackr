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
  #combis <- escapeIndicatorOrnamentation(combis)

  #Function to substitute values based on the
  #dataelement_id.categoryoptioncombo_id
  replaceCombisWithValues <- function(x,
                                      expressions = combis,
                                      v = values) {
    stringi::stri_replace_all(x,
                              fixed =  expressions,
                              replacement = values,
                              vectorize_all = FALSE)
  }


  #Function to replace missing combis with zeros
  replaceExpressionsWithZeros <- function(x) {
    expression.pattern <- "#\\{[a-zA-Z][a-zA-Z0-9]{10}(\\.[a-zA-Z][a-zA-Z0-9]{10})?\\}"
    gsub(expression.pattern, "0", x)
  }


  #Pare down to only valid numeric expressions at this point
  expr_regexes <- list(plus  = "\\+",
  minus = "\\-",
  times = "\\*",
  division = "\\/",
  whitespace  = "\\s+",
  number = "[+\\-]?(?:0|[1-9]\\d*)(?:\\.\\d*)?(?:[eE][+\\-]?\\d+)?",
  lparen = "\\(",
  rparent = "\\)")


  sanitizeExpression <- function(expr) {
    parsed_exp <- lapply(expr, function(x) datimvalidation::lex(x, expr_regexes))
    exp_is_valid <-   !unlist(lapply(lapply(parsed_exp, function(x) names(x) == ".missing"), any))
    expr[!exp_is_valid] <- "NA"
    expr
  }


  #Function to parse and evaluate the expression to return a numeric value
  evaluateExpression <- function(exp) {
    vapply(exp, function(x) eval(str2lang(x)), FUN.VALUE = double(1))
  }

  matches %<>%
    purrr::modify_at(., c("numerator", "denominator"), replaceCombisWithValues) %>%
    purrr::modify_at(., c("numerator", "denominator"), replaceExpressionsWithZeros) %>%
    purrr::modify_at(., c("numerator", "denominator"), sanitizeExpression) %>%
    purrr::modify_at(., c("numerator", "denominator"), evaluateExpression) %>%
    dplyr::mutate(value = numerator / denominator)

  #TODO: Figure out these names attributes

  data.frame(id = matches$id,
             name = matches$name,
             numerator = unname(matches$numerator),
             denominator = unname(matches$denominator),
             value = unname(matches$value))

}
