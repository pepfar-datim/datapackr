#' Title MessageQueue 
#' @description A simple S3 object to deal with messages created during 
#' DataPack processing
#' @param message One or more character strings
#' @param level One of ERROR, WARNING, INFO
#'
#' @return Object of class data.frame and Message queue.
#'

MessageQueue <- function(message=character(),level=character())
{
  
  me <- data.frame(
    message = message,
    level = level, stringsAsFactors = FALSE
  )
  
  ## Set the name for the class
  class(me)<-c("data.frame","MessageQueue")
  return(me)
}


appendMessage<-function(x,message,level) {
  UseMethod("appendMessage",x) }

appendMessage.MessageQueue<-function(x, message=NA,level=NA) {
  
  if (length(message) != length(level)) {
    stop("Messages and warnings must be of the same length")
  }
  
  if (any(is.na(message))) {
    warning("Empty message detected.")
  }
  
  if (any(is.na(level))) {
    level[is.na(level)]<-"UNKNOWN"
  }
  
  new_me<-rbind.data.frame(x,list(message=message,level=level),
                           stringsAsFactors = FALSE)
  class(new_me)<-c("data.frame","MessageQueue")
  return(new_me)
}

printMessages.MessageQueue<-function(x) {
  UseMethod("print",x)
}

printMessages <- function(d) {
  # If warnings, show all grouped by sheet and issue
  if ( NROW(d$info$messages) > 0 & interactive() ) {
    options(warning.length = 8170)
    
    levels<-c("ERROR","WARNING","INFO")
    messages <- d$info$messages
    class(messages)<-"data.frame"
    messages <- messages %>% 
      dplyr::mutate(level = factor((level),levels = levels)) %>% 
      dplyr::arrange(level,message) %>% 
      dplyr::select(message)
    
    messages <-
      paste(
        paste(
          seq_along(messages$message),
          ": " , messages$message
          #stringr::str_squish(gsub("\n", "", d$info$messages))
        ),
        sep = "",
        collapse = "\r\n")
    
    key = paste0(
      "*********************\r\n",
      "KEY:\r\n",
      "- WARNING!: Problematic, but doesn't stop us from processing your tool. May waive with approval from PPM and DUIT.\r\n",
      "- ERROR!: You MUST address these issues and resubmit your tool.\r\n",
      "*********************\r\n\r\n")
    
    cat(crayon::red(crayon::bold("VALIDATION ISSUES: \r\n\r\n")))
    cat(crayon::red(key))
    cat(crayon::red(messages))
  }
}
