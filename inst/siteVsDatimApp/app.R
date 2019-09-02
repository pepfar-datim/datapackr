library(shiny)
library(shinyjs)
require(magrittr)
require(purrr)
require(dplyr)
require(datimvalidation)
require(ggplot2)
library(datapackimporter)

DHISLogin <- function(baseurl, username, password) {
  httr::set_config(httr::config(http_version = 0))
  url <- URLencode(URL = paste0(getOption("baseurl"), "api/me"))
  #Logging in here will give us a cookie to reuse
  r <- httr::GET(url,
                 httr::authenticate(username, password),
                 httr::timeout(60))
  if (r$status != 200L) {
    return(FALSE)
  } else {
    me <- jsonlite::fromJSON(httr::content(r, as = "text"))
    options("organisationUnit" = me$organisationUnits$id)
    return(TRUE)
  }
}

# datapackcommons::DHISLogin("/users/sam/.secrets/prod.json")
# base_url <- getOption("baseurl")

base_url <- "https://www.datim.org/"
# d <- datapackr::unPackSiteToolData("/Users/sam/Documents/cop_19_data/final_site_tool/17_JHHSiteTool_Rwanda_20190407_submission.xlsx")
# 
# temp <- datapackr::compareData_SiteVsDatim(d$datim$site_data, d$info$datapack_uid, "2019Oct")
# 
# 
# site_normal <- "/Users/sam/Downloads/SiteLevelReview_/SiteLevelReview_NORMAL_SITE_Eswatini_20190902151649.xlsx"
# site_hts <- "/Users/sam/Downloads/SiteLevelReview_/SiteLevelReview_HTS_SITE_Eswatini_20190902151655.xlsx"
# support_files <- "/Users/sam/Desktop/SupportingFiles_COP18_OPU/"
# 
# data <- dplyr::bind_rows(datapackimporter::DataPackR(site_normal,
#                                              "",
#                                              "/Users/sam/Desktop/SupportingFiles_COP18_OPU/")$data,
#                          datapackimporter::DataPackR(site_hts,
#                                                      "",
#                                                      "/Users/sam/Desktop/SupportingFiles_COP18_OPU/")$data)

# temp3 <- datapackr::compareData_SiteVsDatim(data, "V0qMZH29CtN", "2018Oct")


library(shiny)

ui <- fluidPage(
  shiny::textInput(inputId = "user", label = "DATIM User Name:"),
  shiny::passwordInput(inputId = "pw", label = "DATIM Password:"),
  shiny::actionButton(inputId = "log_me_in", label = "Log In"),
  shiny::dataTableOutput(outputId = "data_matched")
)

server <- function(input, output, session) {
  
  shiny::observeEvent(input$log_me_in,{
    x = DHISLogin(base_url, input$user, input$pw)
    d <- datapackr::unPackSiteToolData("/Users/sam/Documents/cop_19_data/final_site_tool/17_JHHSiteTool_Rwanda_20190407_submission.xlsx")
    temp <- datapackr::compareData_SiteVsDatim(d$datim$site_data, d$info$datapack_uid, "2019Oct")
    print("here")
    output$data_matched <- shiny::renderDataTable({temp$data_matched_value})
    })
    }
  
shinyApp(ui, server)
