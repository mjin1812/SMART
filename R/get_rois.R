#' @title  Get ROI data from a mapped wholebrain dataset.
#' @description Allows the user to enter a character vector of the regions of
#'   interest (ROIs) and obtain a subset of the dataframe from the wholebrain
#'   dataset of just the ROIs.
#' @param dataset (required) Whole brain region mapped dataset generated from
#'   the [forward_warp()] function
#' @param rois (optional, default = c("ACB", "CEA")) A character vector
#'   containing the regions of interest.
#' @return Returns a dataset of just the regions of interest from the whole
#'   brain dataset.
#' @export
#' @md


get_rois <- function(dataset, rois = c("ACB", "CEA")) {

  rois_ind <-c()
  for (r in 1:length(rois)) {
    regions <- wholebrain::get.sub.structure(rois[r])
    for (r2 in 1:length(regions)) {
      cur_rois_ind <- which(dataset$acronym == regions[r2])
      rois_ind <- c(rois_ind, cur_rois_ind)
    }
  }

  # Calculate the cells to delete
  cells_delete <- 1:length(dataset$x)
  cells_delete <- cells_delete[-(rois_ind)]

  newdataset <- vector('list', length = 14)
  names(newdataset) <- names(dataset)

  for (c in 1:length(dataset)){
    newdataset[[c]] <- dataset[[c]][-cells_delete]
  }
  newdataset <- as.data.frame(newdataset)
  return(newdataset)
}

