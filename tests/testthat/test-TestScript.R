
context("All")

test_that("All", {
  branch <- "master"
  
  # detach("package:datapackr")
  # rstudioapi::restartSession()
  
  library(datapackr)
  library(magrittr)
  
  secrets <- "~/secrets/datim.json"
  model_data_path <- "~/datapackr_test_files/Testing/support_files/model_data_pack_input_21_20210208_1_flat.rds"
  snuxim_model_data_path <- "~/datapackr_test_files/Testing/support_files/PSNUxIM_20210201_1.rds"
  output_folder <- "~/datapackr_test_files"
  
# app style login with explicit session
     datimutils::loginToDATIM(secrets,
                d2_session_name = "d2_session")
# console style login with default session
     # datimutils::loginToDATIM(secrets)
     
     model_data <- readRDS(model_data_path)

     batch <- tibble::tribble(
       ~datapack_name, ~country_uids,
       "Angola","XOivy2uDpMF",                                               #1
       "Botswana","l1KFEXKI4Dg",                                             #2
       "Burundi","Qh4XMQJhbk8",                                             #3
       "Cameroon","bQQJe0cC1eD",                                             #4
       "Cote d'Ivoire","ds0ADyc9UCU",                                        #5
       "Democratic Republic of the Congo","ANN4YCOufcP",                    #6
       "Dominican Republic","NzelIFhEv3C",                                   #7
       "Eswatini","V0qMZH29CtN",                                             #8
       "Ethiopia","IH1kchw86uA",                                            #9
       "Haiti","JTypsdEUNPw",                                                #10
       "Kenya","HfVjCurKxh2",                                                #11
       "Lesotho","qllxzIjjurr",                                              #12
       "Malawi","lZsCb6y0KDX",                                               #13
       "Mozambique", "h11OyvlPxpJ",                                          #14
       "Namibia","FFVkaV9Zk1S",                                              #15
       "Nigeria","PqlFzhuPcF1",                                             #16
       "Rwanda","XtxUYCsDWrR",                                               #17
       "South Africa", "cDGPF739ZZr",                                        #18
       "South Sudan","WLG0z5NxQs8",                                          #19
       "Tanzania","mdXu6iCbn2G",                                            #20
       "Uganda","FETQ6OmnsKB",                                               #21
       "Ukraine","ligZVIYs2rL",                                              #22
       "Vietnam","YM6xn5QxNpY",                                              #23
       "Zambia","f5RoebaDLMx",                                               #24
       "Zimbabwe","a71G4Gtcttv",                                             #25
       "Burma","wChmwjpXOw2",                                                #26
       "Cambodia","XWZK2nop7pM",                                             #27
       "India","skj3e4YSiJY",                                                #28
       "Indonesia","W73PRZcjFIU",                                            #29
       "Laos","PcXTNoVUrUc",                                                 #30
       "Papua New Guinea","cl7jVQOW3Ks",                                     #31
       "Thailand","Gv5ApcpDrIB",                                             #32
       "Kazakhstan","xVvOdyoS7wi",                                           #33
       "Kyrgyzstan","vm58KTm9wvy",                                           #34
       "Nepal","YlSE5fOVJMa",                                                #35
       "Tajikistan","ZtoVYbNCnsj",                                           #36
       "Philippines","p1E1K4MWGpa",                                         #37
       "Caribbean Region",c("RKoVudgb05Y","PeOHqAwdtez","WuxG6jzaypt","zhJINyURZ5Y","WSl5y9jxCpC"), #38
       "Latin America Region",c("joGQFpKiHl9","QKD4CzBG2GM","N7QAPGSaODP","EXVC4bNtv84","w5NMe34EjPN","aUTsSmqqu9O","oK0gC85xx2f"), #39
       "Burkina Faso","ZeB2eGmDfGw",                                        #40
       "Ghana","y3zhsvdXlhN",                                                #41
       "Liberia","kH29I939rDQ",                                             #42
       "Mali","N3xTKNKu5KM",                                                #43
       "Senegal","N5GhQWVpVFs",                                             #44
       "Sierra Leone","ODOymOOWyl0",                                        #45
       "Togo","EIUtrKbw8PQ"                                                 #46
     )
     
     pick <- batch[c(17),]
     

     
     # pack a cop 20 OPU datapack
     for (i in 1:NROW(pick)) {
       print(paste0(i," of ",NROW(pick), ": ", pick[[i,1]]))
       
       packOPUDataPack(datapack_name = pick[[i,1]],
                       country_uids = unlist(pick[[i,"country_uids"]]),
                       template_path = NULL,
                       cop_year = 2020,
                       output_folder = output_folder,
                       d2_session = d2_session,
                       results_archive = FALSE)
     }
     
     # unpack a cop 20 data pack
     d <- unPackTool("~/datapackr_test_files/Testing/OPU/OPU Data Pack_Eswatini_20201116165741_CDC_USAID_with dedup.xlsx"
                     ,d2_session = d2_session
     )
     
     assign(paste0("d_cop20_opu_", branch), d)
     
     foo <- datapackr::compareData_OpuDatapackVsDatim(d 
                                                      , d2_session = d2_session
     )
     assign(paste0("compare_cop20_opu", branch), foo)  
     
     # pack a cop 21 data pack
     for (i in 1:NROW(pick)) {
       print(paste0(i," of ",NROW(pick)))
       
       packDataPack(model_data = model_data,
                    datapack_name = pick[[i,1]],
                    country_uids = pick[[i,2]],
                    template_path = NULL,
                    cop_year = 2021,
                    output_folder = output_folder,
                    d2_session = d2_session,
                    results_archive = FALSE)
     }
     ## don't forget I need to open and save the file
     
     
     d <- unPackTool("~/datapackr_test_files/Testing/No PSNUxIM/Data Pack_Zambia_20210121180718.xlsx"
                     ,d2_session = d2_session
     )
     assign(paste0("d_cop21", branch), d)  
     
     
     foo <- datapackr::create_play_spectrum_output("XtxUYCsDWrR",
                                                   2020,
                                                   "~/datapackr_test_files"
                                                   , d2_session = d2_session
     )
     
     assign(paste0("d_cop21", branch), d)  
     
     d <- writePSNUxIM(d,
                       snuxim_model_data_path,
                       output_folder
                       , d2_session = d2_session
     )  
     
     
     d <- unPackTool( "~/datapackr_test_files/Testing/With PSNUxIM/Data Pack_Malawi_20210121230425.xlsx"
                      , d2_session = d2_session
     )
     d <- writePSNUxIM(d,
                       snuxim_model_data_path,
                       output_folder
                       , d2_session = d2_session
     )
     
     getMechanismView(d2_session)
     
     d <- checkAnalytics(d,
                         model_data_path
                         ,d2_session = d2_session
     )
     
     
     # code_list = pullFullCodeList(2021)
     
     #sites = getSiteList("XtxUYCsDWrR")
     
     
     # datapackr::compareData_DatapackVsDatim(d)
     

   source("data-raw/update_cached_PSNUs.R")
   source("data-raw/update_cached_de_coc_co_map.R")
  source("data-raw/update_cop21_datapack_schema.R")
 #  source("/Users/sam/Documents/GitHub/datapackr/data-raw/produceConfigFile.R")

testthat::expect_equal(1,1)
})