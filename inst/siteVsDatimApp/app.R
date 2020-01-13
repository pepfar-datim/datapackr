library(shiny)
library(shinyjs)
require(magrittr)
require(purrr)
require(dplyr)
require(datimvalidation)
require(datapackimporter)
library(shiny)

options("baseurl" = "https://www.datim.org/")
options(shiny.maxRequestSize=30*1024^2)
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
  useShinyjs(),
  sidebarLayout(
  sidebarPanel(
    shiny::tags$div(id = "controls",
    shinyjs::disabled(
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
    shiny::conditionalPanel(condition = "input.cop_year != '2018Oct'",
                            shiny::fileInput("site_in", "Site Tool")),
    shiny::conditionalPanel(
      condition = "input.cop_year == '2018Oct'",
      shiny::fileInput("hts_in", "HTS"),
      shiny::fileInput("normal_in", "Normal")
    ),
    shiny::actionButton("compare", "Compare")
  ))),
  
  mainPanel(
    shiny::tags$div(id = "log_in_ui",
                    shiny::img(src = 'www/pepfar.png', align = "center"),
                    shiny::tags$br(),shiny::tags$br(),
                    shiny::tags$b("Analyze differences between a COP site tool and DATIM target data"),
                    shiny::tags$br(),shiny::tags$br(),
                    "Please log in.",
                    shiny::tags$br(),shiny::tags$br(),
                    shiny::textInput(inputId = "user",
                                     label = "DATIM User Name:",
                                     value = "sgarman@baosystems.com"),
                    shiny::passwordInput(inputId = "pw",
                                         label = "DATIM Password:"),
                    shiny::actionButton(inputId = "log_me_in",
                                        label = "Log In")
                    ),
    shinyjs::hidden(shiny::downloadButton("downloadFlatPack", label = "Download XLSX"))
      # shiny::dataTableOutput(outputId = "data_matched")
  )
  ))


server <- function(input, output, session) {
  
  shiny::observeEvent(input$log_me_in, {
    success <- DHISLogin(input$user,
                         input$pw)
    if (success) {
      shinyjs::enable("controls")
      shinyjs::hide("log_in_ui")
    } else{
      shinyjs::alert("Log in unsuccesful.")
    }
    print(success)
  })
  
  shiny::observeEvent(input$compare, {
    
    if (input$cop_year == "2018Oct") {
   
          hts<-datapackimporter::DataPackR(input$hts_in$datapath,
                                      "",
                                      input$support_files)
          
          normal<-datapackimporter::DataPackR(input$normal_in$datapath,
                                      "",
                                      input$support_files)
          
          orgunit<-hts$wb_info$ou_uid
          data <- dplyr::bind_rows(hts$data,normal$data)
          

        
    } else if (input$cop_year == "2019Oct") {
      site_tool <- datapackr::unPackSiteToolData(input$site_in$datapath)
      data  <- site_tool$datim$site_data
      orgunit <- site_tool$info$datapack_uid
    } else {
      stop("Unsupported COP year")
    }
    
    compare_out <- datapackr::compareData_SiteVsDatim(data,
                                                      orgunit,
                                                      input$cop_year)
    
  output$downloadFlatPack <- downloadHandler(
    
    filename = function() {
      
      prefix <- "target_site_to_datim_compare_flatpack"
      
      date<-format(Sys.time(),"%Y%m%d_%H%M%S")
      
      paste0(paste(prefix,date,sep="_"),".xlsx")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(compare_out, file = file)
      
    })
  shinyjs::show("downloadFlatPack")
  })
}

shinyApp(ui, server)
