#' @export
#' @title Default Memo Font
#' @return String with default font for memo target tables
defaultMemoFont <- function() {
  "Arial"
}


#' @export
#' @title Default Memo Paragraph Style
#' @return Officer list object defining default style for memo table bodies
defaultMemoStylePara <- function() {
  officer::fp_par(text.align = "right",
                  padding.right = 0.04,
                  padding.bottom = 0,
                  padding.top = 0,
                  line_spacing = 1)
}


#' @export
#' @title Default Memo Header Style
#' @return Officer list object defining default style for memo table headers
defaultMemoStyleHeader <- function() {
  officer::fp_par(text.align = "center",
                  padding.right = 0,
                  padding.bottom = 0,
                  padding.top = 0,
                  line_spacing = 1)
}


#' @export
#' @title Zeroes to Dashes
#' @param x Target value to be formatted.
#' @return Formatted target value
zerosToDashes <- function(x) {
  ifelse(x == 0, "-", formatC(x, format = "f", big.mark = ",", digits = 0))
}



#' @export
#' @title Generate Memo Template File
#' @inheritParams datapackr_params
#' @return Officer docx object
generateMemoTemplate <- function(draft_memo = TRUE) {

  if (draft_memo) {
    draft_memo_template <- system.file("extdata", "draft_memo_template.docx", package = "datapackr")
    memo_doc <- officer::read_docx(path = draft_memo_template)
  } else {
    memo_doc <- officer::read_docx()
  }

  memo_doc
}


#' @export
#' @title Render Prioritization-Level Memo Target Table
#'
#' @param prio_table A table of target data aggregated to the Prioritization
#' level, either d$memo$datim$by_prio or d$memo$datapack$by_prio.
#' @param ou_name Name of organisation unit.
#' @inheritParams datapackr_params
#'
#' @return Formatted prioritization-level memo target table
#'
renderPrioTable <- function(memo_doc, prio_table, ou_name, source_type) {

  # Set the caption based on the source_type
  fig_caption <- switch(source_type,
                        "datapack" = "Table 1a: Prioritization Table (Proposed)",
                        "datim" = "Table 1b: Prioritization Table (Current)")

  memo_doc  %<>% officer::body_add(fig_caption, style = "Normal")


  if (is.null(prio_table)) {
    memo_doc  %<>%
      officer::body_add(value = "This page left intentionally blank. No data was found.") %>%
      officer::body_add_break(pos = "after")
    return(memo_doc)
  }


  has_no_prio <- "No Prioritization - USG Only" %in% names(prio_table)

  #Transform all zeros to dashes
  prio_table %<>%
    dplyr::mutate_if(is.numeric, zerosToDashes)

  style_para <- defaultMemoStylePara()

  style_header <- defaultMemoStyleHeader()

  header_old <- names(prio_table)
  header_new <- c(ou_name, ou_name, header_old[3:dim(prio_table)[2]])
  footer_message <- paste("* Totals may be greater than the sum of categories",
                          "due to activities outside of the SNU",
                          "prioritization areas outlined above")

  #Format totals bottom horizontal line
  totals_bottom_border <- officer::fp_border(color = "black", width = 1.5)

  prio_table <- flextable::flextable(prio_table) %>%
    flextable::merge_v(j = "Indicator") %>%
    flextable::delete_part(part = "header") %>%
    flextable::add_header_row(values = header_new) %>%
    flextable::add_header_row(values = c(ou_name, ou_name,
                                            rep("SNU Prioritizations",
                                                (dim(prio_table)[2] - 2)))) %>%
    flextable::merge_h(part = "header") %>%
    flextable::merge_v(part = "header") %>%
    flextable::bg(bg = "#CCC0D9", part = "header") %>%
    flextable::bg(i = ~ Age == "Total",
                  bg = "#E4DFEC",
                  part = "body") %>% #Highlight total rows
    flextable::hline(i = ~ Age == "Total", border = totals_bottom_border) %>%
    flextable::bold(i = ~ Age == "Total", bold = TRUE, part = "body") %>%
    flextable::bg(j = "Indicator", bg = "#FFFFFF", part = "body") %>%
    flextable::bold(j = "Indicator", bold = FALSE) %>%
    flextable::bold(bold = TRUE, part = "header") %>%
    flextable::fontsize(size = 7, part = "all") %>%
    flextable::style(pr_p = style_header, part = "header") %>%
    flextable::style(pr_p = style_para, part = "body") %>%
    flextable::align(j = 1:2, align = "center") %>% #Center first two columns
    flextable::add_footer_lines(values = footer_message)

    if (has_no_prio) {
      prio_table <- flextable::bg(prio_table,
                                  j = "No Prioritization - USG Only",
                                  bg = "#D3D3D3", part = "body")
    }


  fontname <- defaultMemoFont()

  if (gdtools::font_family_exists(fontname)) {
    prio_table %<>% flextable::font(fontname = fontname, part = "all")
  }

  memo_doc %<>%
    flextable::body_add_flextable(value = prio_table) %>%
    officer::body_add_break(pos = "after")

  memo_doc
}



#' @export
#' @title Render Agency-Level Memo Target Table
#'
#' @param agency_table A table of target data aggregated to the agency
#' level, either d$memo$datim$by_agency or d$memo$datapack$by_agency.
#' @param ou_name Name of the organisation unit.
#' @inheritParams datapackr_params
#'
#' @return Formatted agency-level memo target table
#'
renderAgencyTable <- function(memo_doc, agency_table, ou_name, source_type) {

  # Set the caption based on the source_type
  fig_caption <- switch(source_type,
                        "datapack" = "Table 2a: Agency Table (Proposed)",
                        "datim" = "Table 2b: Agency Table (Current)")

  memo_doc  %<>% officer::body_add(fig_caption, style = "Normal")

  if (is.null(agency_table)) {
    memo_doc  %<>%
      officer::body_add_break(pos = "before") %>%
      officer::body_add(value = "This page left intentionally blank. No data was found.") %>%
      officer::body_add_break(pos = "after")
    return(memo_doc)
  }

  #Transform all zeros to dashes
  agency_table %<>%
    dplyr::mutate_if(is.numeric, zerosToDashes)

  style_para <- defaultMemoStylePara()

  style_header <- defaultMemoStyleHeader()

  header_new <- c("Indicator", "Age",
                  rep("Duplicated Agency Totals", dim(agency_table)[2] - 3),
                  "Deduplicated Total")
  footer_message <- paste("* Agency totals cannot be presented as deduplicated",
                          "totals. The deduplicated total may not be equal to",
                          "the sum of the rows of the data presented due to",
                          "deduplication adjustments.")

  #Format totals bottom horizontal line
  totals_bottom_border <- officer::fp_border(color = "black", width = 1.5)

  agency_table_ft <- flextable::flextable(agency_table) %>%
    flextable::add_header_row(top = TRUE, values = header_new) %>%
    flextable::merge_v(part = "header") %>%
    flextable::merge_h(part = "header", i = 1) %>%
    flextable::merge_v(j = "Indicator") %>%
    flextable::bg(bg = "#CCC0D9", part = "header") %>%
    flextable::bg(i = ~ Age == "Total",
                  bg = "#E4DFEC",
                  part = "body") %>% #Highlight total rows
    flextable::hline(i = ~ Age == "Total", border = totals_bottom_border) %>%
    flextable::bold(i = ~ Age == "Total", bold = TRUE, part = "body") %>%
    flextable::bg(j = "Indicator", bg = "#FFFFFF", part = "body") %>%
    flextable::bold(j = "Indicator", bold = FALSE) %>%
    flextable::bold(bold = TRUE, part = "header") %>%
    flextable::fontsize(size = 7, part = "all") %>%
    flextable::style(pr_p = style_header, part = "header") %>%
    flextable::style(pr_p = style_para, part = "body") %>%
    flextable::align(j = 1:2, align = "center") %>% #Center first two columns
    flextable::add_footer_lines(values = footer_message)


  fontname <- defaultMemoFont()

  if (gdtools::font_family_exists(fontname)) {
    agency_table_ft %<>% flextable::font(fontname = fontname, part = "all")
  }

  memo_doc %>%
    flextable::body_add_flextable(value = agency_table_ft) %>%
    officer::body_add_break(pos = "after")

}



#' @export
#' @title Render Partner-Level Memo Target Tables
#'
#' @param partners_table A table of target data aggregated to the partner and
#' mechanism level, either d$memo$datim$by_partner or
#' d$memo$datapack$by_partner.
#' @inheritParams datapackr_params
#'
#' @return Formatted partner-level memo target table
#'
renderPartnerTable <- function(memo_doc, partners_table, memoStructure, source_type) {

  # Set the caption based on the source_type
  fig_caption <- switch(source_type,
                        "datapack" = "Table 3a: Partner Tables (Proposed)",
                        "datim" = "Table 3b: Partner Tables (Current)")

  memo_doc  %<>% officer::body_add(fig_caption, style = "Normal")

  if (is.null(partners_table)) {
    memo_doc  %<>%
      officer::body_add_break(pos = "before") %>%
      officer::body_add(value = "This page left intentionally blank. No data was found.") %>%
      officer::body_add_break(pos = "after")
    return(memo_doc)
  }


  style_para <- defaultMemoStylePara()

  style_header <- defaultMemoStyleHeader()

  #Format totals bottom horizontal line
  totals_bottom_border <- officer::fp_border(color = "black", width = 1.5)

  #Partners tables
  partners_table <- partners_table %>%
    dplyr::mutate_if(is.numeric, zerosToDashes)

  sub_heading <- names(partners_table)[4:length(partners_table)] %>%
    stringr::str_extract(., "<15|15\\+|<18|18\\+|Total") %>%
    c("Funding Agency", "Partner", "Mechanism", .)

  group_heading <- names(partners_table)[4:length(partners_table)] %>%
    stringr::str_split(., " ") %>%
    purrr::map(purrr::pluck(1)) %>%
    unlist() %>%
    c("Funding Agency", "Partner", "Mechanism", .)

  chunks <- memoStructure %>%
    purrr::pluck("row_order") %>%
    dplyr::filter(!is.na(partner_chunk))

  render_partner_chunk <- function(chunk) {

    partner_table <- flextable::flextable(partners_table[, chunk]) %>%
      flextable::bg(i = ~ Partner == "", bg = "#D3D3D3", part = "body") %>%
      flextable::bold(i = ~ Partner == "", bold = TRUE) %>%
      flextable::delete_part(part = "header") %>%
      flextable::add_header_row(values = sub_heading[chunk]) %>%
      flextable::add_header_row(top = TRUE, values = group_heading[chunk]) %>%
      flextable::merge_v(part = "header") %>%
      flextable::merge_h(i = 1, part = "header") %>%
      flextable::fontsize(size = 7, part = "all") %>%
      flextable::style(pr_p = style_header, part = "header") %>%
      flextable::style(pr_p = style_para, part = "body") %>%
      flextable::width(j = 1:3, 0.75) %>%
      flextable::width(j = 4:(length(chunk)), 0.4) %>%
      flextable::hline(border = totals_bottom_border, part = "body") %>%
      flextable::hline_bottom(border = totals_bottom_border, part = "header")

    fontname <- defaultMemoFont()
    if (gdtools::font_family_exists(fontname)) {
      partner_table %<>% flextable::font(fontname = fontname, part = "all")
    }

    partner_table
  }

  for (chunk in unique(chunks$partner_chunk)) {
    this_chunk <- chunks %>%
      dplyr::filter(partner_chunk == chunk) %>%
      dplyr::mutate(cols = paste(ind, options)) %>%
      dplyr::pull(cols)

    this_chunk <- which(names(partners_table) %in% c("Agency",
                                                     "Partner",
                                                     "Mechanism",
                                                     this_chunk))
    partner_table_ft <- render_partner_chunk(chunk = this_chunk)
    memo_doc %<>%
      flextable::body_add_flextable(partner_table_ft) %>%
      officer::body_add_break(pos = "after")
  }

  memo_doc
}

#' @export
#' @title Generate COP Approval Memo Target Tables (DOCX Format)
#'
#' @inheritParams datapackr_params
#'
#' @return Returns a COP Memo DOCX object suitable for download
#'
generateApprovalMemo <-
  function(d,
           memo_type,
           draft_memo = TRUE,
           d2_session = dynGet("d2_default_session",
                               inherits = TRUE)) {

    memo_doc <- generateMemoTemplate(draft_memo)

    if (memo_type %in% c("datapack", "comparison")) {
      memo_doc <- renderPrioTable(memo_doc,
                                    d$memo$datapack$by_prio,
                                    d$info$datapack_name,
                                    "datapack")
    }

    if (memo_type %in% c("datim", "comparison")) {
      memo_doc <- renderPrioTable(memo_doc,
                                  d$memo$datim$by_prio,
                                  d$info$datapack_name,
                                  "datim")
    }

    if (memo_type %in% c("datapack", "comparison")) {
      memo_doc <- renderAgencyTable(memo_doc,
                                    d$memo$datapack$by_agency,
                                    d$info$datapack_name,
                                    "datapack")
    }

    if (memo_type %in% c("datim", "comparison")) {
      memo_doc <- renderAgencyTable(memo_doc,
                                    d$memo$datim$by_agency,
                                    d$info$datapack_name,
                                    "datim")
    }

    if (memo_type %in% c("datapack", "comparison")) {
      memo_doc <- renderPartnerTable(memo_doc,
                                     d$memo$datapack$by_partner,
                                     d$memo$structure,
                                     "datapack")
    }

    if (memo_type %in% c("datim", "comparison")) {
      memo_doc <- renderPartnerTable(memo_doc,
                                     d$memo$datim$by_partner,
                                     d$memo$structure,
                                     "datim")
    }

    memo_doc
  }
