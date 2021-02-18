library(datapackr)
library(datimutils)
library(magrittr)

datimutils::loginToDATIM("~/.secrets/datim.json")

d2_session <- d2_default_session

#Unpack tool
d <- datapackr::unPackTool()

#Create some addditional metadadta for S3 tagging
d$info$sane_name <- paste0(stringr::str_extract_all(d$info$datapack_name,"[A-Za-z0-9_]",
                                                    simplify = TRUE),sep="",collapse="")
d$info$source_user <- d2_session$me$userCredentials$username
#All self-service datapacks should be marked as unapproved for PAW
d$info$approval_status<-"UNAPPROVED"

d <- validatePSNUData(d, d2_session = d2_session)
d <- validateMechanisms(d, d2_session = d2_session)

datapack_name <-d$info$datapack_name

# Generate Validation Messages =================================================

messages_file <- file(paste("DataPack_Validation_Messages_",
                            Sys.Date(), ".txt", sep = ""))
writeLines(d$info$warning_msg, messages_filename)
close(messages_file)

# Generate Validation Results ==================================================

prefix <- "validation_results"

date<-format(Sys.time(),"%Y%m%d_%H%M%S")

validationresults_filename <- paste0(paste(prefix,date,sep="_"),".xlsx")

sheets_with_data<-d$tests[lapply(d$tests,NROW) > 0]

if (length(sheets_with_data)>0) {
  openxlsx::write.xlsx(sheets_with_data, file = validationresults_filename)
} else {
  print("No validation issues, so nothing to download!")
}

# Generate Comparison File =====================================================
prefix <- "comparison"

date<-format(Sys.time(),"%Y%m%d_%H%M%S")

comparison_filename <- paste0(paste(prefix,date,sep="_"),".xlsx")

comparison_wb <- openxlsx::createWorkbook()

d_compare <- datapackr::compareData_OpuDatapackVsDatim(d, d2_session = d2_session)

openxlsx::addWorksheet(comparison_wb, "Updated Targets")
openxlsx::writeDataTable(wb = comparison_wb,
                         sheet = "Updated Targets",
                         x = d_compare$updates)

openxlsx::addWorksheet(comparison_wb, "Deleted Targets")
openxlsx::writeDataTable(wb = comparison_wb,
                         sheet = "Deleted Targets",
                         x = d_compare$deletes)

openxlsx::addWorksheet(comparison_wb, "Deduplications")
openxlsx::writeDataTable(wb = comparison_wb,
                         sheet = "Deduplications",
                         x = d_compare$dedupes)

openxlsx::saveWorkbook(comparison_wb, file = comparison_filename, overwrite = TRUE)


# Generate FlatPack ===========================================================
prefix <- "flatpack"

date<-format(Sys.time(),"%Y%m%d_%H%M%S")

flatpack_filename <- paste0(paste(prefix,date,sep="_"),".xlsx")

flatpack_wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(flatpack_wb, "Distributed MER Data")
openxlsx::writeDataTable(wb = flatpack_wb,
                         sheet = "Distributed MER Data",
                         x = d$data$analytics)

openxlsx::addWorksheet(flatpack_wb, "Analytics Summary")
openxlsx::writeData(wb = flatpack_wb,
                    sheet = "Analytics Summary",
                    x = modalitySummaryTable(d$data$analytics))

openxlsx::saveWorkbook(flatpack_wb, file = flatpack_filename, overwrite = TRUE)


