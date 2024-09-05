
checkToolSetMetadata <- function(d, p) {
  to_compare <- c("country_uids", "cop_year", "datapack_name", "sane_name", "operating_uid")

  datapack_meta <- d$info[names(d$info) %in% to_compare]
  datapack_meta <- datapack_meta[order(to_compare)]
  psnuxim_meta <- p$info[names(p$info) %in% to_compare]
  psnuxim_meta <- psnuxim_meta[order(to_compare)]

  are_compatible <- identical(datapack_meta, psnuxim_meta)

  if (!are_compatible) {
    stop("This set of tools do not appear to be compatible with one another.")
  }

  TRUE

}

masqueradeTool <- function(tool)  {
  if (tool == "Target Setting Tool") {
    return("Data Pack")
  }

  if (tool == "OPU Data Pack") {
    return("PSNUxIM")
  }

  tool
}

#' @title unPackToolSet
#'
#' @description Packs the PSNUxIM tab in either a COP or OPU Data Pack.
#'
#' @inheritParams datapackr_params
#' @param d1_path Path to a DataPack
#' @param d2_path Path to a PSNUxIM tool
#' @param country_uids List of country UIDs
#' @param cop_year COP year
#'
#'
#' @return Datapackr d object merged from a Datapack and standalone PSNUxIM tab
#' @export

unPackToolSet <- function(d1_path = NULL,
                          d2_path = NULL,
                          country_uids = NULL,
                          cop_year = NULL,
                          d2_session = dynGet("d2_default_session", inherits = TRUE)) {

  d1 <- createKeychainInfo(d1_path)
  d2 <- createKeychainInfo(d2_path)


  #Are they reasonably comptaible with each other?
  are_compatible <- checkToolSetMetadata(d1, d2)


  d1_path <- d1$keychain$submission_path
  d2_path <- d2$keychain$submission_path

  #Deal with renaming of tools
  d1_tool <- masqueradeTool(d1$info$tool)
  d2_tool <- masqueradeTool(d2$info$tool)

  if (!setequal(c(d1_tool, d2_tool), c("Data Pack", "PSNUxIM"))) {
    stop("Cannot unpack that combination of tools.")
  }

  datapack_path <- ifelse(d1_tool == "Data Pack", d1_path, d2_path)
  psnuxim_path <- ifelse(d2_tool == "PSNUxIM", d2_path, d1_path)

  #Get the datapack
  d <- unPackTool(submission_path = datapack_path,
                  tool = "Data Pack",
                  country_uids = country_uids,
                  cop_year = cop_year,
                  d2_session =  d2_session)

  #Unpack the PSNUxIM
  #Use existing prioritizations if they are part of the datapack
  if (!is.null(d$datim$prioritizations)) {

    pzns <- d$datim$prioritizations %>%
    dplyr::select(orgUnit, value)
  }

  p <- unPackTool(submission_path = psnuxim_path,
                  tool = NULL,
                  country_uids = d$info$country_uids,
                  cop_year = d$info$cop_year,
                  pzns = pzns,
                  mer_data = d$data$MER,
                  d2_session = d2_session)


  d <- mergeDatapack(d, p)


  d$data$SNUxIM <- p$data$SNUxIM
  d$datim$OPU <- p$datim$OPU

  d <- checkNonEqualTargets(d, d$data$MER)
  d <- extractSNUxIMCombos(d, p)
  #We may need the path to the PSNUxIM tab, if they need to regenerated
  d$keychain$psnuxim_file_path <- p$keychain$submission_path
  #Keep PSNUxIM Schema for later
  d$info$psnuxim_schema <- p$info$schema

  dp_indicators <- unique(d$data$analytics$indicator_code)
  dp_indicators <- dp_indicators[!dp_indicators %in% unique(p$data$analytics$indicator_code)]


  dp_analytics <- d$data$analytics %>%
    dplyr::filter(indicator_code %in% dp_indicators)

  d$data$analytics <- dplyr::bind_rows(p$data$analytics, dp_analytics)

  d$info$has_psnuxim <- TRUE
  d$info$hybrid_tool <- TRUE
  d$info$needs_psnuxim <- NROW(d$tests$non_equal_targets) > 0 || NROW(d$data$missingCombos) > 0

  d$info$hybrid_tool <- TRUE

  d

}
