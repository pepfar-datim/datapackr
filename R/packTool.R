#' @export
#' @title Pack a requested tool
#'
#' @description
#' Generates a requested Data Pack or OPU Data Pack tool by taking an Excel
#' template file and combining it with data pulled from DATIM API to produce
#' a file ready for distribution.
#'
#' @inheritParams datapackr_params
#'
#' @return Exports a Data Pack or OPU Data Pack tool to Excel within
#' \code{output_folder}.
#'
packTool <- function(model_data_path = NULL,
                     snuxim_model_data_path = NULL,
                     undistributed_mer_data = NULL,
                     tool,
                     datapack_name,
                     country_uids,
                     template_path,
                     cop_year,
                     output_folder,
                     results_archive = TRUE,
                     d2_session = dynGet("d2_default_session",
                                         inherits = TRUE)) {

  interactive_print(datapack_name)
  interactive_print(country_uids)

  # Checks parameters ####
  params <- check_params(cop_year = cop_year %missing% NULL,
                         country_uids = country_uids,
                         datapack_name = datapack_name %missing% NULL,
                         output_folder = output_folder %missing% NULL,
                         results_archive = results_archive,
                         template_path = template_path %missing% NULL,
                         tool = tool %missing% NULL)

  for (p in names(params)) {
    assign(p, purrr::pluck(params, p))
  }

  rm(params, p)

  # Create data sidecar ####
  d <- datapackr::createDataPack(datapack_name = datapack_name,
                                 country_uids = country_uids,
                                 template_path = template_path,
                                 cop_year = cop_year,
                                 tool = tool,
                                 d2_session = d2_session)

  # Adds additional folder and file paths to d object ####
  d$keychain$output_folder <- output_folder
  d$keychain$model_data_path <- model_data_path
  d$keychain$snuxim_model_data_path <- snuxim_model_data_path

  # Pack file based on type ####
  if (d$info$tool == "Data Pack") {
    d <- packDataPack(d, d2_session = d2_session)
  } else if (d$info$tool == "OPU Data Pack") {
    d <- packOPUDataPack(d,
                         undistributed_mer_data = undistributed_mer_data,
                         d2_session = d2_session)
  } else {
    stop("Selected tool not currently supported.")
  }

  # Save & Export Workbook ####
  interactive_print("Saving...")
  exportPackr(data = d$tool$wb,
              output_folder = d$keychain$output_folder,
              tool = d$info$tool,
              datapack_name = d$info$datapack_name)

  # Save & Export Archive ####
  if (results_archive) {
    interactive_print("Archiving...")
    exportPackr(data = d,
                output_folder = d$keychain$output_folder,
                tool = "Results Archive",
                datapack_name = d$info$datapack_name)
  }

  # Print messages ####
  printMessages(d$info$messages)

}
