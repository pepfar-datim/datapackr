library(datapackr)
library(magrittr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

output_folder <- Sys.getenv("OUTPUT_FOLDER")

batch <- tibble::tribble(
  ~datapack_name, ~country_uids,
  "Angola", "XOivy2uDpMF",                                               #1
  "Botswana", "l1KFEXKI4Dg",                                             #2
  "Burundi", "Qh4XMQJhbk8",                                             #3
  "Cameroon", "bQQJe0cC1eD",                                             #4
  "Cote d'Ivoire", "ds0ADyc9UCU",                                        #5
  "Democratic Republic of the Congo", "ANN4YCOufcP",                    #6
  "Dominican Republic", "NzelIFhEv3C",                                   #7
  "Eswatini", "V0qMZH29CtN",                                             #8
  "Ethiopia", "IH1kchw86uA",                                            #9
  "Haiti", "JTypsdEUNPw",                                                #10
  "Kenya", "HfVjCurKxh2",                                                #11
  "Lesotho", "qllxzIjjurr",                                              #12
  "Malawi", "lZsCb6y0KDX",                                               #13
  "Mozambique", "h11OyvlPxpJ",                                          #14
  "Namibia", "FFVkaV9Zk1S",                                              #15
  "Nigeria", "PqlFzhuPcF1",                                             #16
  "Rwanda", "XtxUYCsDWrR",                                               #17
  "South Africa", "cDGPF739ZZr",                                        #18
  "South Sudan", "WLG0z5NxQs8",                                          #19
  "Tanzania", "mdXu6iCbn2G",                                            #20
  "Uganda", "FETQ6OmnsKB",                                               #21
  "Ukraine", "ligZVIYs2rL",                                              #22
  "Vietnam", "YM6xn5QxNpY",                                              #23
  "Zambia", "f5RoebaDLMx",                                               #24
  "Zimbabwe", "a71G4Gtcttv",                                             #25
  "Burma", "wChmwjpXOw2",                                                #26
  "Cambodia", "XWZK2nop7pM",                                             #27
  "India", "skj3e4YSiJY",                                                #28
  "Indonesia", "W73PRZcjFIU",                                            #29
  "Laos", "PcXTNoVUrUc",                                                 #30
  "Papua New Guinea", "cl7jVQOW3Ks",                                     #31
  "Thailand", "Gv5ApcpDrIB",                                             #32
  "Kazakhstan", "xVvOdyoS7wi",                                           #33
  "Kyrgyzstan", "vm58KTm9wvy",                                           #34
  "Nepal", "YlSE5fOVJMa",                                                #35
  "Tajikistan", "ZtoVYbNCnsj",                                           #36
  "Philippines", "p1E1K4MWGpa",                                          #37
  "Caribbean Region", c("RKoVudgb05Y",
                        "PeOHqAwdtez",
                        "WuxG6jzaypt",
                        "zhJINyURZ5Y",
                        "WSl5y9jxCpC"),                                 #38
  "Latin America Region", c("joGQFpKiHl9",
                            "QKD4CzBG2GM",
                            "N7QAPGSaODP",
                            "EXVC4bNtv84",
                            "w5NMe34EjPN",
                            "aUTsSmqqu9O",
                            "oK0gC85xx2f"),                             #39
  "Burkina Faso", "ZeB2eGmDfGw",                                        #40
  "Ghana", "y3zhsvdXlhN",                                                #41
  "Liberia", "kH29I939rDQ",                                             #42
  "Mali", "N3xTKNKu5KM",                                                #43
  "Senegal", "N5GhQWVpVFs",                                             #44
  "Sierra Leone", "ODOymOOWyl0",                                        #45
  "Togo", "EIUtrKbw8PQ"                                                 #46
)

pick <- batch[c(8), ]
# i = 1

for (i in seq_len(pick)) {
  print(paste0(i, " of ", NROW(pick), ": ", pick[[i, 1]]))

  spectrum_output <-
    create_play_spectrum_output(
      country_uids = unlist(pick[[i, "country_uids"]]),
      cop_year = 2021)
}
