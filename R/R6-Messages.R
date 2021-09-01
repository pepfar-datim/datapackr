
#' @title Message queue class for handling warnings and other messages
#'
#' @description
#' Processes a submitted Data Pack by identifying integrity issues, checking
#' data against DATIM validations, and extracting data.
#'
#'
Messages<-R6::R6Class("Messages",
                  #' @title Messages
                  public=list(
                  #' @field msg_frame Data frame consisting of message and level
  msg_frame=data.frame(message=character(),
                       level= character()),
  #' @method append Add a new message. Requires message and level as a parameter
  append=function(message,level) {
    levels<-c("ERROR","WARNING","INFO")
    new_row <- list(message=message,level=level)
    self$msg_frame<-dplyr::bind_rows(self$msg_frame,new_row) %>%
      dplyr::mutate(level = factor((level),levels = levels)) %>% 
      dplyr::arrange(level,message)
      
    invisible(self)
  },
  print=function() {
    key = paste0(
      "*********************\r\n",
      "KEY:\r\n",
      "- ERROR!: You MUST address these issues and resubmit your tool.\r\n",
      "- WARNING!: Problematic, but doesn't stop us from processing your tool. May waive with approval from PPM and DUIT.\r\n",
      "- INFO: Provided for your information and action only. Does not indicate an problem with your tool.\r\n",
      "*********************\r\n\r\n")
    messages<-paste(seq_along(self$msg_frame),self$msg_frame$message,sep="",collapse="\r\n")
    cat(crayon::red(crayon::bold("VALIDATION ISSUES: \r\n\r\n")))
    cat(crayon::red(key))
    cat(crayon::red(messages))
  }
))