library(magrittr)
library(datapackr)

output_folder <- Sys.getenv("OUTPUT_FOLDER") %>% paste0("Spectrum Examples/")
dp_folder <- "/Users/scott/Downloads/BetaPack COP21 DPs"

dp_filenames <- list.files(dp_folder)

dp_filepaths <- paste0(dp_folder, "/", dp_filenames)

spectrum_extracts <- list()

for (filepath in dp_filepaths) {

  extract <-
    readxl::read_excel(
      path = filepath,
      sheet = "Spectrum",
      range = readxl::cell_limits(c(1, 4), c(NA, 16)),
      col_types = "text",
      .name_repair = "minimal"
    ) %>%
    dplyr::mutate(
      calendar_quarter =
        dplyr::case_when(
          calendar_quarter == "CY2021Q3" ~ "CY2022Q3",
          TRUE ~ calendar_quarter)
    )

  country_name <-
    datapackr::unPackDataPackName(
      submission_path = filepath,
      tool = "Data Pack")

  country_uid <-
    datapackr::unPackCountryUIDs(
      submission_path = filepath,
      tool = "Data Pack",
      cop_year = 2021)

  bundle <- list(
    country_name = country_name,
    data = extract
  )

  spectrum_extracts[[country_uid]] <- bundle

}

saveRDS(spectrum_extracts, paste0(output_folder, "SpectrumExtracts.rds"))


# Export ####

for (i in seq_len(spectrum_extracts)) {
  country <- spectrum_extracts[[i]]$country_name
  data <- spectrum_extracts[[i]]$data

  output_file_name <-
    paste0(output_folder, "COP21SpectrumExtract_", country, ".csv")

  readr::write_csv(data, output_file_name)
}
