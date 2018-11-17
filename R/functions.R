#' @title Helper background functions
#' @description  necessary functions and data structures to load
#' @export
#
# # Load whole brain atlas index
# load(file.path(.libPaths()[1],"wholebrain","data","atlasIndex.RData"))

# Load whole brain atlasIndex values and extract only coronal slices
# at_ind  <- atlasIndex$mm.from.bregma[1:132]

# Initiate quartz
if (.Platform$OS.type == "windows" | .Platform$OS.type == "unix") {
  quartz <- function(width,height){
    windows(width, height)}
}

# return plate number based on entered AP
platereturn <- function(AP) {
  ind <-c()
  for (n in 1:length(AP)){
    ind <- c(ind, which.min(abs(AP[n]-atlasIndex$mm.from.bregma[1:132])))
  }
  return(ind)
}

# Rounds to nearest atlas coordinate in whole brain (coronal)
roundAP <- function(AP) {
  # AP can be a vector of values
  vec   <- c()
  for (n in 1:length(AP)){
    ind    <- which.min(abs(AP[n]-atlasIndex$mm.from.bregma[1:132]))
    vec[n] <- atlasIndex$mm.from.bregma[1:132][ind]
  }
  return(vec)
}

