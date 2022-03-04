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
  #TODO: Make this work for both COP years.!
  ind_group <- switch(as.character(cop_year),
                      "2020" = "wWi08ToZ2gR",
                      "2021" = "TslxbFe3VUZ",
                      "2022" = "zRApVEi7qjo",
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
  }

  inds
}
