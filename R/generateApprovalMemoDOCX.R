#' Title
#'
#' @param d Dataoackr d object
#' @param draft Should a draft watermark be added to the document?
#' @param remove_empty_columns Should empty columns be removed?
#'
#' @return Returns a COP Memo DOCX object suitable for download
#' @export
#'
generateApprovalMemo <- function(d,draft=TRUE, remove_empty_columns = TRUE) {

  prio_table <- d %>%
    purrr::pluck("data") %>%
    purrr::pluck("prio_table")

  #Remove any columns which are all zeros to save space.
  column_filter <-
    d$data$prio_table %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = FALSE) != 0)) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::filter(!V1) %>%
    row.names()

  prio_table %<>% dplyr::select(-column_filter)

  #Transform all zeros to dashes
  prio_table %<>%
    dplyr::mutate_if(is.numeric,
                     function(x) ifelse(x == 0, "-", formatC(x, format = "f", big.mark = ",", digits = 0))) #nolint

  style_para_prio <- fp_par(text.align = "right",
                            padding.right = 0.04,
                            padding.bottom = 0,
                            padding.top = 0,
                            line_spacing = 1)

  style_header_prio <- fp_par(text.align = "center",
                              padding.right = 0,
                              padding.bottom = 0,
                              padding.top = 0,
                              line_spacing = 1)


  header_old <- names(prio_table)
  ou_name <- d$info$datapack_name
  header_new <- c(ou_name, ou_name, header_old[3:dim(prio_table)[2]])

  prio_table <- flextable(prio_table) %>%
    merge_v(., j = "Indicator") %>%
    delete_part(., part = "header") %>%
    add_header_row(., values = header_new) %>%
    add_header_row(., values = c(ou_name, ou_name, rep("SNU Prioritizations", (dim(prio_table)[2] - 2)))) %>%
    merge_h(., part = "header") %>%
    merge_v(., part = "header") %>%
    bg(., bg = "#CCC0D9", part = "header") %>%
    bg(., i = ~ Age == "Total", bg = "#E4DFEC", part = "body") %>% #Highlight total rows
    bold(., i = ~ Age == "Total", bold = TRUE, part = "body") %>%
    bg(., j = "Indicator", bg = "#FFFFFF", part = "body") %>%
    bold(., j = "Indicator", bold = FALSE) %>%
    bold(., bold = TRUE, part = "header") %>%
    fontsize(., size = 7, part = "all") %>%
    style(., pr_p = style_header_prio, part = "header") %>%
    style(., pr_p = style_para_prio, part = "body") %>%
    align(., j = 1:2, align = "center") %>% #Align first two columns center
    flextable::add_footer_lines(values = paste("* Totals may be greater than the sum of categories",
                                               "due to activities outside of the SNU",
                                               "prioritization areas outlined above"))

  fontname <- "Arial"
  if (gdtools::font_family_exists(fontname)) {
    prio_table <- font(prio_table, fontname = fontname, part = "all")
  }

  if (draft) {
    doc <- read_docx(path = "inst/extdata/draft_memo_template.docx")
  } else {
    doc <- read_docx()
  }

  doc <- body_add_flextable(doc, value = prio_table)
  doc <- body_add_break(doc, pos = "after")

  #Partners tables
  partners_table <- d$data$partners_table %>%
    dplyr::mutate_if(is.numeric,
                     function(x) ifelse(x == 0, "-", formatC(x, format = "f", big.mark = ", ", digits = 0)))

  sub_heading <- names(partners_table)[4:length(partners_table)] %>%
    stringr::str_split(., " ") %>%
    purrr::map(purrr::pluck(2)) %>%
    unlist() %>%
    c("Funding Agency", "Partner", "Mechanism", .)

  group_heading <- names(partners_table)[4:length(partners_table)] %>%
    stringr::str_split(., " ") %>%
    purrr::map(purrr::pluck(1)) %>%
    unlist() %>%
    c("Funding Agency", "Partner", "Mechanism", .)

  #TODO: This needs to be redone based on the COP year
  chunks <- list(c(1:15), c(1:3, 16:26), c(1:3, 27:35), c(1:3, 36:44))

  renderPartnerTable <- function(chunk) {

    partner_table <- flextable(partners_table[, chunk]) %>%
      bg(., i = ~ Partner == "", bg = "#D3D3D3", part = "body") %>%
      bold(., i = ~ Partner == "", bold = TRUE) %>%
      delete_part(., part = "header") %>%
      add_header_row(., values = sub_heading[chunk]) %>%
      add_header_row(., top = TRUE, values = group_heading[chunk]) %>%
      merge_h(., part = "header") %>%
      merge_v(., part = "header") %>%
      fontsize(., size = 7, part = "all") %>%
      style(., pr_p = style_para_prio, part = "body") %>%
      width(., j = 1:3, 0.75) %>%
      width(., j = 4:(length(chunk)), 0.4)

    fontname <- "Arial"
    if (gdtools::font_family_exists(fontname)) {
      partner_table <- font(partner_table, fontname = fontname, part = "all")
    }

    return(partner_table)
  }

  for (i in seq_along(chunks)) {
    chunk <- chunks[[i]]
    partner_table_ft <- renderPartnerTable(chunk = chunk)
    doc <- body_add_flextable(doc, partner_table_ft)
    doc <- body_add_break(doc, pos = "after")
  }

  return(doc)
}
