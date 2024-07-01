indicator_regexes <- c(
  indicator = "N\\{[a-zA-Z][a-zA-Z0-9]{10}\\}",
  constant = "C\\{[a-zA-Z][a-zA-Z0-9]{10}\\}",
  de_coc_operand  = "#\\{[a-zA-Z][a-zA-Z0-9]{10}\\.[a-zA-Z][a-zA-Z0-9]{10}\\}",
  de_operand = "#\\{[a-zA-Z][a-zA-Z0-9]{10}\\}",
  plus  = "\\+",
  minus = "\\-",
  times = "\\*",
  division = "\\/",
  whitespace  = "\\s+",
  number = "[+\\-]?(?:0|[1-9]\\d*)(?:\\.\\d*)?(?:[eE][+\\-]?\\d+)?",
  lparen = "\\(",
  rparent = "\\)"
)


isValidIndicator <- function(parsed_inds) {

  !unlist(lapply(lapply(parsed_inds, function(x) names(x) == ".missing"), any))
}

getValidIndicators <- function(inds) {
  num_is_valid <- lapply(inds$numerator, datimvalidation::lex, indicator_regexes)
  denom_is_valid <- lapply(inds$denominator, datimvalidation::lex, indicator_regexes)
  is_valid <- isValidIndicator(num_is_valid) & isValidIndicator(denom_is_valid)
  inds[is_valid, ]
}
#' @export
#' @title Get COP Memo Indicators
#'
#' @inheritParams datapackr_params
#'
#' @return A dataframe consisting of indicator UIDs, name, numerator, and
#' denominator expression. If a given COP year does not exist, NULL is returned.
#'
getMemoIndicators <- function(cop_year,
                                d2_session = dynGet("d2_default_session",
                                                    inherits = TRUE)) {
  #Fetch indicators from the COP21 memo group
  ind_group <- switch(as.character(cop_year),
                      "2023" = "ZTGhB3qIPsi",
                      "2024" = "VOYwOgsftbT",
                      NULL)
  #Bail out early if don't have a group
  if (is.null(ind_group)) {
    warning("Could not find an indicator group for the given COP year.")
    return(NULL)
  }

  inds <-
    datimutils::getIndicatorGroups(
      ind_group,
      fields = "indicators[id, name, numerator, denominator]",
      d2_session = d2_session
    )

  #If we do not get any indicators at this point, something is wrong.
  if (class(inds) != "data.frame") {
    warning("Could not find fetch indicators from DATIM!")
    return(NULL)
  } else {
    #Only return indicators which we can actually process
    getValidIndicators(inds)
  }

}
