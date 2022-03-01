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
evaluate_indicators <- function(combis, values, inds) {

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
  matches_indicator <- function(x) {
    agrepl(x, inds$numerator) |
      agrepl(x, inds$denominator)
  }

  #Determine which indicator formulas match our supplied data
  matches <- this.des %>%
    unique(.) %>%
    purrr::map(., matches_indicator) %>%
    Reduce("|", .) %>%
    dplyr::filter(inds, .)

  #Return something empty here if we have no indicator matches
  if (nrow(matches) == 0) {
    return(indicators_empty)
  }

  #Function to substitute values based on the
  #dataelement_id.categoryoptioncombo_id
  replace_combis_with_values <- function(x,
                                         combis.this = combis,
                                         values.this = values) {
    stringi::stri_replace_all_fixed(x,
                                    combis.this,
                                    values.this,
                                    vectorize_all = FALSE)
  }

  # Function to replace missing totals with zeros
  replace_totals_with_values <- function(x) {
    replace_combis_with_values(x,
                               combis = totals_df$exp,
                               values = totals_df$values)
  }

  #Function to replace missing combis with zeros
  replace_expressions_with_zeros <- function(x) {
    expression.pattern <- "#\\{[a-zA-Z][a-zA-Z0-9]{10}(\\.[a-zA-Z][a-zA-Z0-9]{10})?\\}"
    gsub(expression.pattern, "0", x)
  }

  #Function to parse and evalulate the expression to return a numeric value
  evaluate_expression <- function(exp) {
    vapply(exp, function(x) eval(parse(text = x)), FUN.VALUE = double(1))
  }

  matches %<>%
    purrr::modify_at(., c("numerator", "denominator"), replace_combis_with_values) %>%
    purrr::modify_at(., c("numerator", "denominator"), replace_totals_with_values) %>%
    purrr::modify_at(., c("numerator", "denominator"), replace_expressions_with_zeros) %>%
    purrr::modify_at(., c("numerator", "denominator"), evaluate_expression) %>%
    dplyr::mutate(value = numerator / denominator)

  matches
}
