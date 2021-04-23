# This script produces a mapping object between periods and datasets in DATIM and
# saves this to the package as a referencable data object.

datapackr::loginToDATIM("~/.secrets/datim.json")

DATIM_ds_pd_map <- datapackr::map_DATIM_dataset_period()

new <- DATIM_ds_pd_map
  
  # Compare old and new maps for accuracy ####
compare_diffs <- datapackr::DATIM_ds_pd_map %>%
  dplyr::mutate(in_pkg = "Y") %>%
  dplyr::full_join(
    new %>%
      dplyr::mutate(new = "Y"),
    by = c("dataset.id", "FY", "datastream", "targets_results", "ou_level",
           "DOD", "dataset.name")) %>%
  dplyr::filter(is.na(in_pkg) | is.na(new))


save(DATIM_ds_pd_map, file = "./data/DATIM_ds_pd_map.rda", compress = "xz")

# Remember to rebuild package and commit
