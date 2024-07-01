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
packTool <- function(model_data = NULL,
                     model_data_path = NULL,
                     snuxim_model_data_path = NULL,
                     undistributed_mer_data = NULL,
                     tool,
                     datapack_name,
                     country_uids,
                     template_path,
                     cop_year,
                     output_folder,
                     results_archive = TRUE,
                     expand_formulas = FALSE,
                     spectrum_data = NULL,
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

  # Set global numeric format ####
  options("openxlsx.numFmt" = "#,##0")

  # Create data sidecar ####
  d <- createDataPack(datapack_name = datapack_name,
                      country_uids = country_uids,
                      template_path = template_path,
                      cop_year = cop_year,
                      tool = tool,
                      d2_session = d2_session)

  # Adds additional folder and file paths to d object ####
  d$keychain$output_folder <- output_folder
  d$keychain$model_data_path <- model_data_path
  d$keychain$snuxim_model_data_path <- snuxim_model_data_path

  # Start running log of all warning and information messages ####
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE

  # Pack file based on type ####
  if (d$info$tool == "Data Pack") {
    d <- packDataPack(d,
                      model_data = model_data,
                      spectrum_data = spectrum_data,
                      d2_session = d2_session)
  } else if (d$info$tool %in% c("OPU Data Pack", "PSNUxIM")) {

    d <- packOPUDataPack(d,
                         undistributed_mer_data = undistributed_mer_data,
                         expand_formulas = expand_formulas,
                         d2_session = d2_session)

  } else if (d$info$tool %in% c("PSNUxIM", "PSNUxIM Template")) {

    d <- writePSNUxIM(d, snuxim_model_data_path = d$keychain$snuxim_model_data_path, d2_session = d2_session)

    } else {
    stop("Selected tool not currently supported.")
  }

  # Save & Export Workbook ####
  interactive_print("Saving...")
  if (d$info$cop_year %in% c(2023, 2024) && d$info$tool == "Data Pack") {
    tool_name <- "Target Setting Tool"
  } else {
    tool_name <- d$info$tool
  }

  d$info$output_file <- exportPackr(data = d$tool$wb,
                                    output_folder = d$keychain$output_folder,
                                    tool = tool_name,
                                    datapack_name = d$info$datapack_name)

  # Save & Export Archive ####
  if (results_archive) {
    interactive_print("Archiving...")
    d$info$output_file <- exportPackr(data = d,
                                      output_folder = d$keychain$output_folder,
                                      tool = "Results Archive",
                                      datapack_name = d$info$datapack_name)
  }

  # Print messages ####
  printMessages(d$info$messages)

  #Return the d object for testing purposes
  return(d)

}
