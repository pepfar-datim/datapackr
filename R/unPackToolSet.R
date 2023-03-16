
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


#' Title
#'
#' @param datapack_path
#' @param psnuxim_path
#' @param country_uids
#' @param cop_year
#' @param d2_session
#'
#' @return Datapackr d object merged from a Datapack and standalone PSNUxIM tab
#' @export

unPackToolSet <- function(datapack_path = NULL,
                          psnuxim_path = NULL,
                          country_uids = NULL,
                          cop_year = NULL,
                          d2_session = dynGet("d2_default_session", inherits = TRUE)) {

  #Get the datapack
  d <- unPackTool(submission_path = datapack_path,
                  tool = "Data Pack",
                  country_uids = country_uids,
                  cop_year = cop_year,
                  d2_session =  d2_session)

  #Unpack the PSNUxIM
  p <- unPackTool(submission_path = psnuxim_path,
                  tool = "PSNUxIM",
                  country_uids = country_uids,
                  cop_year = cop_year,
                  d2_session = d2_session)

  #Are they reasonably comptaible with each other?
  are_compatible <- checkToolSetMetadata(d, p)

  d$data$SNUxIM <- p$data$SNUxIM
  d$data$PSNUxIM_combos <- p$data$PSNUxIM_combos
  d <- datapackr:::checkNonEqualTargets(d, d$data$MER)
  #TODO: Not clear how to handle the two analytics objects?
  #
  #d$data$analytics <- p$data$analytics


  d

}
