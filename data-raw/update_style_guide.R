# Home Tab Styles ####
  home <- list(
  ## Home Tab Title
    title = openxlsx::createStyle(fontColour = "#000000",
                                  fontSize = 76,
                                  textDecoration = "bold",
                                  halign = "left",
                                  valign = "center"),
  ## Home Tab OU Name
    datapack_name = openxlsx::createStyle(fontColour = "#073763",
                                          fontSize = 64,
                                          textDecoration = "bold",
                                          halign = "left",
                                          valign = "center"),

  ## Home Tab PEPFAR banner
    pepfar = openxlsx::createStyle(fontColour = "#7F7F7F",
                                   fontSize = 36,
                                   halign = "left",
                                   valign = "center")
  )

# Site Lists ####
  siteList <- list(
    community = openxlsx::createStyle(fontColour = "#000000",
                                      bgFill = "#EBF1DE"),
    facility = openxlsx::createStyle(fontColour = "#000000",
                                     bgFill = "#DCE6F1"),
    inactive = openxlsx::createStyle(fontColour = "#000000",
                                     bgFill = "#808080"),
    national = openxlsx::createStyle(fontColour = "#000000",
                                     bgFill = "#CCC0DA"),
    military = openxlsx::createStyle(fontColour = "#000000",
                                     bgFill = "#C4BD97")
  )

# Data Tabs ####
  data <- list(
    title = openxlsx::createStyle(fontSize = 18,
                                  textDecoration = "bold",
                                  halign = "left",
                                  valign = "center"),
    header = openxlsx::createStyle(fontSize = 12,
                                   textDecoration = "bold",
                                   halign = "left",
                                   valign = "center",
                                   fgFill = "#E4E0A7"),
    label = openxlsx::createStyle(wrapText = TRUE,
                                  halign = "center",
                                  valign = "center",
                                  fgFill = "#9CBEBD"),
    uid = openxlsx::createStyle(textDecoration = "bold",
                                fgFill = "#C2D8D8",
                                fontColour = "#C2D8D8"),
    rowHeader = openxlsx::createStyle(textDecoration = "bold",
                                      fgFill = "#C2D8D8",
                                      fontColour = "#000000"),
    sumRows = openxlsx::createStyle(textDecoration = "bold"),
    invalidDisagg = openxlsx::createStyle(fontColour = "#C00000",
                                          bgFill = "#000000")
  )

# COP21 OPU Styles ####
  cop21_opu <- list(
    thin_border = openxlsx::createStyle(border = "left",
                                         borderStyle = "thin"),
    thick_border = openxlsx::createStyle(border = "left",
                                         borderStyle = "thick"),
    numeric_format = openxlsx::createStyle(numFmt = "#,##0")
  )

 # Compile ####
  styleGuide <- list(home = home,
                     siteList = siteList,
                     data = data,
                     cop21_opu = cop21_opu)

  save(styleGuide,
       file = "./data/styleGuide.rda",
       compress = "xz")
