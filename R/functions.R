#' @title Helper background functions
#' @description  necessary functions and data structures to load
#' @family aggregate functions
# # Load whole brain atlas index
# load(file.path(.libPaths()[1],"wholebrain","data","atlasIndex.RData"))

# Load whole brain atlasIndex values and extract only coronal slices
# at_ind  <- atlasIndex$mm.from.bregma[1:132]

#' @export
# return plate number based on entered AP
platereturn <- function(AP) {
  ind <-c()
  for (n in 1:length(AP)){
    ind <- c(ind, which.min(abs(AP[n]-atlasIndex$mm.from.bregma[1:132])))
  }
  return(ind)
}

#' @export
# Get Allen Mouse Brain Atlas Return
AMBA_return <- function(AP){
  ind <- platereturn(AP)
  return(atlasIndex$plate.id[ind])
}

#' @export
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

#' @export
# Get cell counts of data based on regions of interest given
get_count <- function(dataset, roi = c('MO', 'TH')){
  out <- unlist(lapply(roi, function(x)sum(dataset$acronym %in% wholebrain::get.sub.structure(x))))
  roi.data <- data.frame(acronym = roi, cell.count = out)
  return(roi.data)
}

#' @export
# Recursive function to get all child regions of entered regions of interest
get_all_children <- function (rois, children = c()) {
  for (l in 1:length(rois)){
    # Get child regions
    new_children <- wholebrain::get.acronym.child(rois[l])

    # If there are more child regions
    if (!anyNA(new_children)) {
      children <- c(children, new_children)
      children <- get_all_children(new_children, children = children)

    }
  }
  return(children)
}

# Get current OS
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
  return(tolower(os))
}

# convertpath for windows users
convertpath <- function(path){
  return(gsub("\\\\", "/", path, ignore.case = FALSE, perl = FALSE,
       fixed = FALSE, useBytes = FALSE))
}

# Get counts based on hemisphere
get_hemi_cnts <- function(dataset, roi = c('MO', 'TH'), hemi= "Right"){
  if (hemi == "Right") {
    out <- unlist(lapply(roi, function(x)sum(dataset$acronym %in% wholebrain::get.sub.structure(x) * dataset$right.hemisphere)))
  } else if (hemi == "Left") {
    out <- unlist(lapply(roi, function(x)sum(dataset$acronym %in% wholebrain::get.sub.structure(x) * !dataset$right.hemisphere)))
  }
  roi.data <- data.frame(acronym = roi, cell.count = out)
  return(roi.data)
}


