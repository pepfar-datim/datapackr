#' @title emptySiteToolSheetFrame(d)
#' @return An empty intermediate data frame when all values in a given Site
#' Tool sheet are completely empty. 
emptySiteToolSheetFrame<-function(){
  
  tibble::tribble(
    ~Site, 
    ~site_uid,
    ~mech_code,
    ~Type, 
    ~sheet_name, 
    ~indicatorCode, 
    ~Age, 
    ~Sex, 
    ~KeyPop, 
    ~value
  )
  
}
