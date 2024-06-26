% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{valid_OrgUnits_2024}
\alias{valid_OrgUnits_2024}
\title{List of valid valid organisation units used for generating Data Packs.}
\format{
\describe{
  \item{name}{Name of the organisation unit}
  \item{uid}{UID of the organisation unit}
  \item{org_type}{Type of organisation unit. One of PSNU, Military, Country
  or DSNU (Dreams sub-national organisation unit)}
  \item{ou}{Operating unit of the organisation unit}
  \item{ou_uid}{UID of the operating unit associated with the organisation unit}
  \item{country}{Country of the organisation unit}
  \item{country_uid}{UID of the country associated with the organisation unit}
  \item{snu1_uid}{Subnational Unit Level1 UID}
  \item{snu1}{Subnational Unit Level1 name}
  \item{lastUpdated}{The last time the Organisation Unit was updated}
  \item{ancestors}{A nested eleven column data frame that contains the
   list of parent organisation units that contain the PSNU,
   including the names, ids, and which organisationUnitGroups that those
   parent organisation units belong to}
  \item{organisationUnitGroups}{A nested two column data frame that
   contains the name and id of the groups the organisation unit is associated
   with. For example "Community" and "PvuaP6YALSA"}
  \item{DREAMS}{Determined, Resilient, Empowered, AIDS-free, Mentored, and
   Safe Partnernship. Character column "Y" or NA.}
}
}
\usage{
valid_OrgUnits_2024
}
\description{
List of valid organisation units used for generating Data Packs. Must be
synced and saved manually!
}
\keyword{datasets}
