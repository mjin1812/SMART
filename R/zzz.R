#' @title On attachment of SMART
#' @description setup the quartz function depending on the user platform

.onAttach<-function(...){
  # Initiate quartz
  if(get_os() == "windows") {
    quartz<-function(width, height, ...){windows(width, height, ...)}
    assign("quartz", quartz, envir = .GlobalEnv)
  } else if(get_os() == "linux") {
    quartz<-function(width, height, ...){X11(width, height, ...)}
    assign("quartz", quartz, envir = .GlobalEnv)
  }
}
