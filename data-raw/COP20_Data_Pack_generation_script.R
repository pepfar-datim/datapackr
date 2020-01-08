library(datapackr)
library(magrittr)

secrets <- "/Users/scott/.secrets/datim.json"

loginToDATIM(secrets)

output_folder <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 20/3) Testing & Deployment/Beta Packs/Testing"

model_data_path <- "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 20/3) Testing & Deployment/model_data_pack_input_20_20200107_1_flat.rds"

model_data <- readRDS(model_data_path)

batch <- tibble::tribble(
  ~datapack_name, ~country_uids,
  "Angola","XOivy2uDpMF",
  "Botswana","l1KFEXKI4Dg",
  #"Burundi","Qh4XMQJhbk8",
  "Cameroon","bQQJe0cC1eD",
  "Cote d'Ivoire","ds0ADyc9UCU",
  #"Democratic Republic of the Congo","ANN4YCOufcP",
  "Dominican Republic","NzelIFhEv3C",
  "Eswatini","V0qMZH29CtN",
  #"Ethiopia","IH1kchw86uA",
  "Haiti","JTypsdEUNPw",
  "Kenya","HfVjCurKxh2",
  "Lesotho","qllxzIjjurr",
  "Malawi","lZsCb6y0KDX",
  "Mozambique", "h11OyvlPxpJ",
  "Namibia","FFVkaV9Zk1S",
  #"Nigeria","PqlFzhuPcF1",
  "Rwanda","XtxUYCsDWrR",
  "South Africa", "cDGPF739ZZr",
  "South Sudan","WLG0z5NxQs8",
  #"Tanzania","mdXu6iCbn2G",
  "Uganda","FETQ6OmnsKB",
  "Ukraine","ligZVIYs2rL",
  "Vietnam","YM6xn5QxNpY",
  "Zambia","f5RoebaDLMx",
  "Zimbabwe","a71G4Gtcttv",
  "Burma","wChmwjpXOw2",
  "Cambodia","XWZK2nop7pM",
  "India","skj3e4YSiJY",
  "Indonesia","W73PRZcjFIU",
  "Laos","PcXTNoVUrUc",
  "Papua New Guinea","cl7jVQOW3Ks",
  "Thailand","Gv5ApcpDrIB",
  "Kazakhstan","xVvOdyoS7wi",
  "Kyrgyzstan","vm58KTm9wvy",
  "Nepal","YlSE5fOVJMa",
  "Tajikistan","ZtoVYbNCnsj",
  #"Philippines","p1E1K4MWGpa",
  "Caribbean Region",c("RKoVudgb05Y","PeOHqAwdtez","WuxG6jzaypt","zhJINyURZ5Y","WSl5y9jxCpC"),
  "Latin America Region",c("joGQFpKiHl9","QKD4CzBG2GM","N7QAPGSaODP","EXVC4bNtv84","w5NMe34EjPN","aUTsSmqqu9O","oK0gC85xx2f"),
  #"Burkina Faso","ZeB2eGmDfGw",
  "Ghana","y3zhsvdXlhN",
  #"Liberia","kH29I939rDQ",
  #"Mali","N3xTKNKu5KM",
  #"Senegal","N5GhQWVpVFs",
  #"Sierra Leone","ODOymOOWyl0",
  #"Togo","EIUtrKbw8PQ"
)

for (i in 1:NROW(batch)) {
  packDataPack(model_data = model_data,
               datapack_name = batch[[i,1]],
               country_uids = batch[[i,2]],
               template_path = NULL,
               cop_year = 2020,
               output_folder = output_folder)
}
