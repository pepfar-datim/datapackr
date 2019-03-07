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
      "attributeOptionCombo",
      "categoryoptioncombo",
      "categoryOptionCombo",
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
      "countryName",
      "country_uid",
      "country_uids",
      "countryUID",
      "Currently_in_DATIM",
      "dataelement",
      "dataElement",
      "dataelementuid",
      "DataPack_name",
      "data_pack_name",
      "DataPackSiteID",
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
      "isMil",
      "KeyPop",
      "level",
      "level4name",
      "mechanism",
      "mechanismCode",
      "mechanismid",
      "mil_level",
      "mil_psnu",
      "mil_psnu_uid",
      "milPSNU",
      "milPSNU_in_DATIM",
      "milPSNUuid",
      "model_uid",
      "msg",
      "n",
      "name",
      "name3",
      "name4",
      "newValue",
      "operating_unit",
      "order_check",
      "organisationunituid",
      "organisationUnitGroups",
      "organisationUnits",
      "orgunit",
      "orgUnit",
      "packageVersion",
      "period",
      "planning",
      "prioritization",
      "Prioritizing_at_Natl_or_SNU",
      "psnu",
      "PSNU",
      "psnuid",
      "row_id",
      "setNames",
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
      "typeOptions",
      "type",
      "uid",
      "validAges",
      "validKPs",
      "validSexes",
      "value",
      "wb"
    )
    
  )