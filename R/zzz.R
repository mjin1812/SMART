#' @title On attachment of SMART
#' @description setup the quartz function depending on the user platform

.onAttach<-function(...){
  get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
      os <- sysinf['sysname']
      if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
  }

  # Initiate quartz
  if(get_os() == "windows") {
    quartz<-function(width, height, ...){windows(width, height, ...)}
    assign("quartz", quartz, envir = .GlobalEnv)
  } else if(get_os() == "linux") {
    quartz<-function(width, height, ...){X11(width, height, ...)}
    assign("quartz", quartz, envir = .GlobalEnv)
  }
}
