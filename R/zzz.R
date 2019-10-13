#' @title On attachment of SMART
#' @description setup the quartz function depending on the user platform

.onAttach<-function(...){
  # Initiate quartz
  if(get_os() == "windows"){
    quartz<-function(width, height, ...){windows(width, height, ...)}
  } else if(get_os() == "linux"){
    quartz<-function(...){x11(...)}
  } else if(get_os() == "osx"){
    quartz <-  function(title, width, height, xpos = NULL, ...){
      if(!is.null(xpos)) {
        warning("MacOS quartz does not accept `xpos`. Not using parameter")
      }
      R.utils::doCall(quartz, args = list(title, width, height, xpos = NULL, ...))
    }

    savePlot <- function(filename = NULL, ...){quartz.save(file = filename, ...)
    }
    assign("savePlot", savePlot, envir = .GlobalEnv)
  }
  assign("quartz", quartz, envir = .GlobalEnv)
}
