#' @export
#' @importFrom plyr ldply
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

  tests_rows <- purrr::map(d$tests, NROW) %>%
    plyr::ldply(., data.frame) %>%
    `colnames<-`(c("test_name", "count"))

  tests_names <- purrr::map(d$tests, function(x) attr(x, "test_name")) %>%
    plyr::ldply(., data.frame) %>%
    `colnames<-`(c("test_name", "validation_issue_category"))


  dplyr::left_join(tests_names, tests_rows, by = "test_name") %>%
    dplyr::mutate(ou = d$info$datapack_name,
                  ou_id = d$info$country_uids,
                  country_name = d$info$datapack_name,
                  country_uid = d$info$country_uids) %>%
    dplyr::filter(count > 0)

}
