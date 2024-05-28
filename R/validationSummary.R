#' @export
#' @title validationSummary(d)
#'
#' @description Produce an output of tests.
#'
#' @param d Datapackr object
#'
#' @return Data frame consisting of ou, ou_id, country_name, country_uid, validation_issue_category, count
#'
#'

validationSummary <- function(d) {

  tests_rows <- t(purrr::map_dfr(d$tests, NROW)) %>%
    as.data.frame() %>%
    dplyr::mutate(test_name = rownames(.),
                   count = as.numeric(V1)) %>%
    dplyr::select(test_name, count) %>%
    tibble::as_tibble()

  tests_names <- t(purrr::map_dfr(d$tests, attr, "test_name")) %>%
    as.data.frame() %>%
    dplyr::mutate(test_name = rownames(.),
                  validation_issue_category = V1) %>%
    dplyr::select(test_name, validation_issue_category) %>%
    tibble::as_tibble()


  dplyr::left_join(tests_names, tests_rows, by = "test_name") %>%
    dplyr::mutate(ou = d$info$datapack_name,
                  ou_id = d$info$operating_unit$ou_uid,
                  country_name = d$info$datapack_name,
                  country_uid = paste(d$info$country_uids, sep = "", collapse = ";")) %>%
    dplyr::filter(count > 0)

}
