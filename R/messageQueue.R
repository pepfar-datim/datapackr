#' Title MessageQueue
#' @description A simple S3 object to deal with messages created during
#' DataPack processing
#' @param message One or more character strings
#' @param level One of ERROR, WARNING, INFO
#'
#' @return Object of class data.frame and Message queue.
#' @export

MessageQueue <- function(message = character(), level = character(), tool = character()) {

  # if (missing(tool)) {
  #   tool <- NA_character_
  # }

  messages <- data.frame(
    message = message,
    level = level,
    tool = tool,
    stringsAsFactors = FALSE
  )

  ## Set the name for the class

  attr(messages, "class") <- "MessageQueue"

  messages
}


#' Title appendMessage
#' @description Generic function to handle appending messages to a MessageQueue
#' @param x A message queue
#' @param message A string or vector of strings of messages.
#' @param level A string or vector of strings of
#' @param tool A string vector of the tool (optional) description
#' message levels (ERROR, WARNING, INFO)
#' @return A MessageQueue class.
#' @export

appendMessage <- function(x, message, level, tool) {
  UseMethod("appendMessage", x)
}

#' Title appendMessage.MessageQueue
#' @description Internal S3 method to deal with appending messages
#' @param x A MessageQueue object
#' @param message  A message or vector of messages.
#' @param level  A string or vector of message levels (ERROR, WARNING, INFO)
#' @return A MessageQueue object
#' @export

appendMessage.MessageQueue <- function(x, message = NA_character_, level = NA_character_, tool = NA_character_) {

  if (!is.vector(message) || is.list(message)) {
    stop("Please supply a vector of messages")
  }

  if (!is.vector(level) || is.list(level)) {
    stop("Please supply a vector of levels")
  }

  if (length(message) != length(level)) {
    stop("Messages and warnings must be of the same length")
  }

  #Make the tool argument optional, but take the shortest length if missing
  if (missing(tool)) {
    tool <- rep("UNKNOWN", min(length(message), length(level)))
  } else {
    if (!is.vector(tool) || is.list(tool)) {
      stop("Please supply a vector of tools")
    }
  }


  if (!all.equal(length(message), length(level), length(tool))) {
      stop("Messages and tools must be of the same length")
    }

  empty_messages <- sapply(message, is_empty)
  empty_levels <- sapply(level, is_empty)
  empty_tools <- sapply(tool, is_empty)

  #Do nothing if everything is blank
  if (all(empty_messages) && all(empty_levels)) {
    return(x)
  }

  if (any(empty_levels)) {
    warning("Empty level detected.")
    level[empty_levels] <- "UNKNOWN"
  }

  if (any(empty_tools)) {
    tool[empty_tools] <- "UNKNOWN"
  }


  if (any(empty_messages)) {
    warning("Empty message detected.")
    message <- message[!empty_messages]
    level <- level[!empty_messages]
    tool <- tool[!empty_messages]

    if (length(message) == 0) {
      return(x)
    }
  }

  #Check to see if the message and level match.
  #If they don't issue a warning

  inconsistent_messages <- mapply(function(m, l)  !grepl(l, substring(m, first = 0, last = 20)), message, level)

  if (any(inconsistent_messages)) {

    warning(paste("At least one inconsistent message and level detected!"))

  }

  new_me <- rbind.data.frame(x, data.frame(message = message, level = level, tool = tool),
                           stringsAsFactors = FALSE)
  class(new_me) <- c("data.frame", "MessageQueue")
  return(new_me)
}

#' Title printMessages
#' @description Generic function to handle printing messages of a MessageQueue
#'
#' @param x A MessageQueue object
#'
#' @return Returns a formatted output to the console
#' @export
#'
printMessages <- function(x) {
  UseMethod("printMessages", x)
}


#' Title printMessage.MessageQueue
#' @description Internal S3 method to deal with printing messages
#'
#' @param x A MessageQueue object
#'
#' @return Returns a formatted output to the console
#' @export
#'
printMessages.MessageQueue <- function(x) {
  # If warnings, show all grouped by sheet and issue
  if (NROW(x) > 0 && interactive()) {
    options(warning.length = 8170)

    levels <- c("ERROR", "WARNING", "INFO")
    messages <- x
    class(messages) <- "data.frame"
    messages <- messages %>%
      dplyr::mutate(level = factor((level), levels = levels)) %>%
      dplyr::arrange(level, message) %>%
      dplyr::select(message)

    messages <-
      paste(
        paste(
          seq_along(messages$message),
          ": ", messages$message
          # stringr::str_squish(gsub("\n", "", d$info$messages))
        ),
        sep = "",
        collapse = "\r\n")

    key <- paste0(
      "*********************\r\n",
      "KEY:\r\n",
      "- WARNING!: Problematic, but doesn't stop us from processing your tool.",
      "May waive with approval from PPM and DUIT.\r\n",
      "- ERROR!: You MUST address these issues and resubmit your tool.\r\n",
      "*********************\r\n\r\n")

    cat(crayon::red(crayon::bold("VALIDATION ISSUES: \r\n\r\n")))
    cat(crayon::red(key))
    cat(crayon::red(messages))
  }
}
