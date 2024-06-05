#' @export
#' @title Create a new Data Pack
#' @author Scott Jackson
#' @description Creates a brand new Data Pack with the supplied characteristics.
#'
#' @inheritParams datapackr_params
#'
#' @return Data Pack object
#'
createDataPack <- function(datapack_name = NULL,
                           country_uids,
                           template_path = NULL,
                           cop_year = NULL,
                           tool = NULL,
                           d2_session = dynGet("d2_default_session",
                                               inherits = TRUE)) {

  if (tool %in% c("Data Pack Template", "OPU Data Pack Template")) {
    tool <- stringr::str_remove(tool, " Template$")
  }

  # Check & assign params
  params <- check_params(
    country_uids = country_uids,
    cop_year = cop_year,
    tool = tool,
    template_path = template_path,
    schema = NULL,
    datapack_name = datapack_name)

  for (p in names(params)) {
    assign(p, purrr::pluck(params, p))
  }

  rm(params, p)

  wb <- openxlsx::loadWorkbook(template_path)

  options("openxlsx.numFmt" = "#,##0")

  # Write Home Sheet info
  wb <- writeHomeTab(wb = wb,
                     datapack_name = datapack_name,
                     country_uids = country_uids,
                     cop_year = cop_year,
                     tool = tool)

  wb_copy <- paste0(tempfile(), ".xlsx")
  openxlsx::saveWorkbook(wb = wb, file = wb_copy, overwrite = TRUE)

  # Create DP object
  d <- createKeychainInfo(submission_path = wb_copy,
                          tool = tool,
                          country_uids = country_uids,
                          cop_year = cop_year,
                          d2_session = d2_session)

  d$tool$wb <- wb

  unlink(wb_copy)

  if (d$info$tool %in% c("Data Pack Template", "OPU Data Pack Template")) {
    d$info$tool <- stringr::str_remove(d$info$tool, " Template$")
  }

  return(d)
}
