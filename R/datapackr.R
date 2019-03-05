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
      ".",
      "Age",
      "ancestors",
      "attributeoptioncombo",
      "categoryoptioncombo",
      "categoryoptioncombouid",
      "categoryOptionGroups",
      "categoryOptions",
      "CoarseAge",
      "code",
      "col_type",
      "colType",
      "community",
      "country",
      "country_name",
      "country_uid",
      "country_uids",
      "dataelement",
      "dataelementuid",
      "DataPack_name",
      "data_pack_name",
      "DataPackTarget",
      "dataset",
      "disag",
      "distribution",
      "end_date",
      "endDate",
      "facility",
      "groupSets",
      "id",
      "IMPATT.PRIORITY_SNU.20T",
      "indicator",
      "indicator_code",
      "indicatorCode",
      "is_region",
      "KeyPop",
      "level",
      "mechanism",
      "mechanismCode",
      "mechanismid",
      "mil_level",
      "mil_psnu",
      "mil_psnu_uid",
      "model_uid",
      "msg",
      "n",
      "name",
      "name3",
      "name4",
      "newValue",
      "order_check",
      "organisationUnitGroups",
      "organisationUnits",
      "orgunit",
      "packageVersion",
      "period",
      "planning",
      "prioritization",
      "psnu",
      "PSNU",
      "psnuid",
      "row_id",
      "Sex",
      "sheet_name",
      "sheet_num",
      "sheets",
      "site_schema",
      "siteID",
      "siteValue",
      "site_tool_label",
      "site_type",
      "start_date",
      "startDate",
      "Status",
      "submission_order",
      "template_order",
      "type",
      "validAges",
      "validKPs",
      "validSexes",
      "value",
      "wb"
    )
    
  )