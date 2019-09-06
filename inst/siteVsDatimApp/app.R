library(shiny)
library(shinyjs)
require(magrittr)
require(purrr)
require(dplyr)
require(datimvalidation)
require(datapackimporter)
library(shiny)

options("baseurl" = "https://www.datim.org/")
addResourcePath("www", system.file("siteVsDatimApp/www", package = "datapackr"))
DHISLogin <- function(username, password, 
                      base_url = getOption("baseurl")) {
  httr::set_config(httr::config(http_version = 0))
  url <- URLencode(URL = paste0(base_url, "api/me"))
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

ping <- function(){
  if (httr::GET(paste0(getOption("baseurl"),"api/system/ping")) %>% 
      httr::content() == "pong"){
    TRUE
  } else{
    FALSE
  }
}

ui <- fluidPage(
  

  
  sidebarLayout(
  sidebarPanel(
    
    shiny::textInput(
      inputId = "support_files",
      label = "Support Files:",
      value = "/Users/sam/Desktop/SupportingFiles_COP18_OPU/"
    ),
    shiny::radioButtons(
      inputId = "cop_year",
      label = "COP Year",
      choiceNames =  c("COP19", "COP18"),
      choiceValues = c("2019Oct", "2018Oct")
    ),
    shiny::selectInput(
      inputId = "org_unit",
      label = "Countries/Region",
      choices = c("Eswatini" = "V0qMZH29CtN",
                  "Rwanda" = "XtxUYCsDWrR"),
      multiple = TRUE,
      selectize = TRUE
    ),
    shiny::conditionalPanel(condition = "input.cop_year != '2018Oct'",
                            shiny::fileInput("site_in", "Site Tool")),
    shiny::conditionalPanel(
      condition = "input.cop_year == '2018Oct'",
      shiny::fileInput("hts_in", "HTS"),
      shiny::fileInput("normal_in", "Normal")
    ),
    shiny::actionButton("compare", "Compare")
  ),
  
  mainPanel(shiny::conditionalPanel(condition = "!output.logged_in", 
                                      shiny::img(src='www/pepfar.png', align = "center"),
                                      "Please log in.",
                                      # shiny::radioButtons(
                                      #   inputId = "server",
                                      #   label = "Server",
                                      #   choiceNames =  c("Production",
                                      #                    "Triage",
                                      #                    "Jason"),
                                      #   choiceValues = c(
                                      #     "https://www.datim.org/",
                                      #     "https://triage.datim.org/",
                                      #     "https://jason.datim.org/"
                                      #   )
                                      # ),
                                        shiny::textInput(
                                        inputId = "user",
                                        label = "DATIM User Name:",
                                        value = "sgarman@baosystems.com"
                                      ),
                                      shiny::passwordInput(inputId = "pw",
                                                           label = "DATIM Password:"),
                                      shiny::actionButton(inputId = "log_me_in",
                                                          label = "Log In")
                                      
  ),
  shiny::dataTableOutput(outputId = "data_matched"))
))

server <- function(input, output, session) {

  output$logged_in <- shiny::reactive(FALSE)
  outputOptions(output, "logged_in", suspendWhenHidden = FALSE)
  shiny::observeEvent(input$log_me_in, {
    output$logged_in <-  shiny::reactive(DHISLogin(input$user,
                                                   input$pw))
    })
  shiny::observeEvent(input$compare, {
    #output$logged_in <- ping()
    if (input$cop_year == "2018Oct") {
      data <-
        dplyr::bind_rows(
          datapackimporter::DataPackR(input$hts_in$datapath,
                                      "",
                                      input$support_files)$data,
          datapackimporter::DataPackR(input$normal_in$datapath,
                                      "",
                                      input$support_files)$data
        )
    } else if (input$cop_year == "2019Oct") {
      data <-
        datapackr::unPackSiteToolData(input$site_in$datapath)$datim$site_data
    } else {
      stop("Unsupported COP year")
    }
    compare_out <- datapackr::compareData_SiteVsDatim(data,
                                                      input$org_unit,
                                                      input$cop_year)
    output$data_matched <-
      shiny::renderDataTable({
        compare_out$matched
      })
  })
}

shinyApp(ui, server)
