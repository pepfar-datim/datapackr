# originally used to compare the non-PSNUxIM versions of the data pack with the 
# PSNU version excluding the PSNUxIM tab in the comparison

# used to make sure we test we wern't accidentally
# altering the contents of data we aren't supposed to alter


library(tidyverse)
original <- file.choose()
original_sheets <- readxl::excel_sheets(original)
modified <- file.choose()
modified_sheets <- readxl::excel_sheets(modified)
assertthat::assert_that(dplyr::all_equal(original_sheets,modified_sheets))

original_data <- purrr::map(original_sheets,~readxl::read_xlsx(original,.x, col_names = FALSE) %>% 
                              dplyr::mutate(original=TRUE)) %>% 
  setNames(original_sheets)


modified_data <- purrr::map(original_sheets,~readxl::read_xlsx(modified,.x, col_names = FALSE)%>% 
                              dplyr::mutate(modified=TRUE))%>% 
  setNames(original_sheets)

# comparison_join <- purrr::map(original_sheets[1:22],~(dplyr::full_join(original_data[[.x]],
#                                                                 modified_data[[.x]]) %>% 
#                                                         dplyr::filter(is.na(original) |
#                                                                         is.na(modified))) %>% 
#                                  dplyr::arrange_all()) %>% 
#                            setNames(original_sheets[1:22])

comparison_join <- purrr::map(original_sheets,~(dplyr::full_join(original_data[[.x]],
                                                                       modified_data[[.x]]) %>%
                                                        dplyr::filter(is.na(original) |
                                                                        is.na(modified))) %>%
                                dplyr::arrange_all()) %>%
  setNames(original_sheets)
