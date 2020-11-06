#' @export
#' @title Export output.
#'
#' @description
#' Exports a datapackr output to specified filepath.
#'
#' @param data Data object to export. Can be either a dataframe or an Openxlsx
#' Workbook object.
#' @param output_path A local path directing to the folder where you would like
#' outputs to be saved. If not supplied, will output to working directory.
#' @param type File prefix to be applied in output filename, as follows:
#'   \describe{
#'     \item{\code{Data Pack}}{Openxlsx Workbook object containing Data Pack.}
#'     \item{\code{FAST Export}}{Data frame containing FAST export data.}
#'     \item{\code{SUBNAT IMPATT}}{Data frame containing SUBNAT/IMPATT data.}
#'     \item{\code{Results Archive}}{List object containing results archive.}
#'     \item{\code{OPU Data Pack}}{Openxlsx Workbook object containing OPU Data Pack.}
#'     \item{\code{Spectrum Example}}{Data frame containing example Spectrum output.}
#' }
#' @param datapack_name Name you would like associated with this Data Pack.
#' (Example: "Western Hemisphere", or "Caribbean Region", or "Kenya".)
#'
exportPackr <- function(data, output_path, type, datapack_name) {
  packName <- function(output_path, type, datapack_name, extension) {
    paste0(
      output_path,
      if (is.na(stringr::str_extract(output_path,"/$"))) {"/"} else {},
      type,"_",
      datapack_name,"_",
      format(Sys.time(), "%Y%m%d%H%M%S"),
      extension
    )
  }

  if (type %in% c("Data Pack", "OPU Data Pack")) {
    if (class(data) != "Workbook") {
      stop("Output type and data do not match!")
    }

    output_file_name <- packName(output_path, type, datapack_name, extension = ".xlsx")

    openxlsx::saveWorkbook(wb = data, file = output_file_name, overwrite = TRUE)
  }

  else if (type %in% c("FAST Export","SUBNAT IMPATT", "Spectrum Example")) {
    if (!any(stringr::str_detect(class(data), "data\\.frame|tbl_df"))) {
      stop("Output type and data do not match!")
    }

    output_file_name <- packName(output_path, type, datapack_name, extension = ".csv")

    readr::write_csv(data, output_file_name)
  }

  else if (type %in% c("Results Archive")) {
    if (class(data) != "list") {
      stop("Output type and data do not match!")
    }

    output_file_name <- packName(output_path, type, datapack_name, extension = ".rds")

    saveRDS(data, output_file_name)
  }

  print(paste0("Successfully saved ",type," to ", output_file_name))
}
