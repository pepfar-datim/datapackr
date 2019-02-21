#' @docType package
#' @name datapackr
#' @title Data Pack on R
#'
#' datapackr: a grammar of Data Pack manipulation
#'
#'
#' datapackr is the engine behind PEPFAR Data Packs, a tool used annually to
#' aid PEPFAR Country Teams in setting targets for their annual Country
#' Operating Plan (COP).
#'
#' It has three main goals:
#'
#' \itemize{
#' \item Generate Data Packs, either at District or Site Level as directed.
#' \item Process and validate Data Packs submitted for review
#' \item Prepare Data Pack data for import into DATIM, the PEPFAR data collection database.
#' }
#'
#'
if (getRversion() >= "2.15.1")
  utils::globalVariables(
    c(
      "indicatorCode",
      ".",
      "mechanismCode",
      "indicator",
      "disag",
      "value",
      "mechanismid",
      "n",
      "sheet_name",
      "indicatorCode",
      "template_order",
      "submission_order",
      "order_check",
      "model_uid",
      "sheet_num",
      "dataset",
      "dataelementuid",
      "categoryoptioncombouid",
      "period",
      "dataelement",
      "attributeoptioncombo",
      "orgunit",
      "psnuid",
      "categoryoptioncombo",
      "distribution",
      "row_id",
      "msg",
      "id",
      "name",
      "psnu",
      "level",
      "facility",
      "community",
      "planning",
      "prioritization",
      "country",
      "site_type",
      "organisationUnitGroups",
      "ancestors",
      "mil_psnu_uid",
      "mil_psnu",
      "mil_level",
      "is_region",
      "country_name",
      "country_uid",
      "name4",
      "name3",
      "",
      ""
    )
  )