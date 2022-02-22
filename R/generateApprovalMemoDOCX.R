#' Title
#'
#' @param draft
#'
#' @return
#' @export
#'
generateMemoTemplate <- function(draft=TRUE) {


    if (draft) {
      doc <- read_docx(path = "inst/extdata/draft_memo_template.docx")
    } else {
      doc <- read_docx()
    }


  doc
}

#' Title
#'
#' @param doc
#' @param prio_table
#' @param ou_name
#'
#' @return
#' @export
#'
renderPrioTable <- function(doc,prio_table,ou_name, draft = TRUE) {

  if (is.null(prio_table)) {
    return(doc)
  }

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

   doc <- body_add_flextable(doc, value = prio_table)
   doc <- body_add_break(doc, pos = "after")

   doc
}


#' Title
#'
#' @param doc
#' @param partners_table
#' @param memo_structure
#'
#' @return
#' @export
#'
renderPartnerTable <- function(doc, partners_table, memo_structure) {


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


  #Partners tables
  partners_table <- partners_table %>%
    dplyr::mutate_if(is.numeric,
                     function(x) ifelse(x == 0, "-", formatC(x, format = "f", big.mark = ",", digits = 0)))

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


  chunks <- memo_structure %>%
    purrr::pluck("row_order") %>%
    dplyr::filter(!is.na(partner_chunk))

  renderPartnerTable <- function(chunk) {

    partner_table <- flextable(partners_table[, chunk]) %>%
      bg(., i = ~ Partner == "", bg = "#D3D3D3", part = "body") %>%
      bold(., i = ~ Partner == "", bold = TRUE) %>%
      delete_part(., part = "header") %>%
      add_header_row(., values = sub_heading[chunk]) %>%
      add_header_row(., top = TRUE, values = group_heading[chunk]) %>%
      merge_v(., part = "header") %>%
      merge_h(., i = 1, part = "header") %>%
      fontsize(., size = 7, part = "all") %>%
      style(., pr_p = style_para_prio, part = "body") %>%
      width(., j = 1:3, 0.75) %>%
      width(., j = 4:(length(chunk)), 0.4)

    fontname <- "Arial"
    if (gdtools::font_family_exists(fontname)) {
      partner_table <- font(partner_table, fontname = fontname, part = "all")
    }

    partner_table
  }

  for (chunk in unique(chunks$partner_chunk)) {
    this_chunk <- chunks %>%
      dplyr::filter(partner_chunk == chunk) %>%
      dplyr::mutate(cols = paste(ind,options)) %>%
      dplyr::pull(cols)

    this_chunk <-   which(names(partners_table) %in%  c("Agency","Partner","Mechanism",this_chunk))
    partner_table_ft <- renderPartnerTable(chunk = this_chunk)
    doc <- body_add_flextable(doc, partner_table_ft)
    doc <- body_add_break(doc, pos = "after")
  }

  doc
}

#' @title Generate COP Approval Memo Target Tables (DOCX Format)
#'
#' @param d
#' @param draft Should a draft watermark be added to the document?
#' @param remove_empty_columns Should empty columns be removed?
#'
#' @return Returns a COP Memo DOCX object suitable for download
#' @export
#'
generateApprovalMemo <-
  function(d,
           d2_session,
           memo_type,
           draft = TRUE,
           remove_empty_columns = TRUE,
           include_no_prio = TRUE) {

    d <- prepareMemoData(d, d2_session, memo_type, include_no_prio)

    doc <- generateMemoTemplate(draft)

    if (memo_type %in% c("datapack", "comparison")) {
      doc <-
        renderPrioTable(doc,d$memo$datapack$by_prio, d$info$datapack_name)
    }

    if (memo_type %in% c("datim", "comparison")) {
      doc <-
        renderPrioTable(doc,d$memo$datapack$by_prio, d$info$datapack_name)
    }

    if (memo_type %in% c("datapack", "comparison")) {
      doc <-
        renderPartnerTable(doc, d$memo$datapack$by_partner, d$memo$structure)
    }

    if (memo_type %in% c("datim", "comparison")) {
      doc <-
        renderPartnerTable(doc, d$memo$datim$by_partner, d$memo$structure)
    }


    doc
  }
