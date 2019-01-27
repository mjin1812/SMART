#' @title Modified glassbrain function
#' @description A modified version of [wholebrain::glassbrain()] that
#' 1) Gives the user an option to plot the "jitter" in cell placement in original glassbrain function
#' (the jitter gives a more space filled look) .
#' 2) Does not show the glass brain display when high.res is set to "OFF" (otherwise set to TRUE or FALSE).
#' @param dataset (required) Cell count dataset
#' @param high.res (default = "OFF") TRUE or FALSE sets high or low resolution glassbrain. "OFF"
#' doesn't plot plot the glassbrain. Options: "OFF", TRUE, FALSE
#' @param dim (default = c(720, 1080))  Original argument from [wholebrain::glassbrain()].
#' @param device (default = TRUE) Original argument from [wholebrain::glassbrain()].
#' @param col (default = "region") Original argument from [wholebrain::glassbrain()].
#' @param cex (default = 0.5) Graphical parameter for text size.
#' @param hemisphere (default = "right") Original argument from [wholebrain::glassbrain()].
#' @param spheres (default = FALSE) Original argument from [wholebrain::glassbrain()].
#' @param alpha (default = 1) Original argument from [wholebrain::glassbrain()].
#' @param laterality (default = TRUE) Original argument from [wholebrain::glassbrain()].
#' @param plane (default = "coronal") Original argument from [wholebrain::glassbrain()].
#' @param jitter (default = FALSE) If TRUE, add jitter in z-axis to fill space between z-planes.
#' @seealso See also the function [wholebrain::glassbrain()] from the wholebrain package.
#' @export
#' @md

glassbrain2 <- function(dataset, high.res = "OFF", dim = c(720, 1080), device = TRUE,
                          col = "region", cex = 0.5, hemisphere = "right", spheres = FALSE,
                          alpha = 1, laterality = TRUE, plane = "coronal", jitter = FALSE){

  data(glasbrain, package = "wholebrain", envir = environment())
  if (sum(dataset$color == "#000000") > 0) {
    dataset <- dataset[-which(dataset$color == "#000000"),]
  }
  if (device) {
    rgl::open3d(windowRect = c(0, 0, 1280, 720))
  }

  if (high.res!="OFF") {
    rgl::par3d(persp)
    if (high.res) {
      misc3d::drawScene.rgl(list(VOLUME))
    }
    else {
      misc3d::drawScene.rgl(list(VOLUMESMALL))
    }
  }

  if (col == "region") {
    color <- as.character(dataset$color)
  } else {
    color <- col
  }
  if (hemisphere == "right") {
    hemisphere <- 2
  } else {
    hemisphere <- 1
  }
  if (plane == "coronal") {
    if (isTRUE(jitter)) {
      smp.AP <- rnorm(length(dataset$AP), 0, (320/9.75) * 0.2)
    } else {
      smp.AP <- 0
    }
    smp.ML <- 0
  } else {
    if (isTRUE(jitter)) {
      smp.ML <- rnorm(length(dataset$AP), 0, (320/9.75) * 0.2)

    } else {
      smp.ML <- 0
    }
    smp.AP <- 0
  }
  if (laterality) {
    if (length(unique(dataset$AP)) > 1) {
      laterality <- table(dataset$AP, dataset$right.hemisphere)
      for (i in 1:nrow(laterality)) {
        if (hemisphere == "right") {
          if (laterality[i, 1] > laterality[i, 2]) {
            dataset$ML[which(dataset$AP == as.numeric(row.names(laterality))[i])] <- -dataset$ML[which(dataset$AP ==
                                                                                                         as.numeric(row.names(laterality))[i])]
          }
        }
        else {
          if (laterality[i, 2] > laterality[i, 1]) {
            dataset$ML[which(dataset$AP == as.numeric(row.names(laterality))[i])] <- -dataset$ML[which(dataset$AP ==
                                                                                                         as.numeric(row.names(laterality))[i])]
          }
        }
      }
    }
  }
  if (spheres) {
    rgl::spheres3d(wholebrain::paxTOallen(dataset$AP) - 530/2 + smp.AP, -dataset$DV *
                1000/25 - 320/2, dataset$ML * 1000/25 + smp.ML, col = color,
              radius = cex, alpha = alpha)
  }
  else {
    rgl::points3d(wholebrain::paxTOallen(dataset$AP) - 530/2 + smp.AP, (-dataset$DV *
                                                                     1000/25 * 0.95) - 320/2, dataset$ML * 1000/25 + smp.ML,
             col = color, size = cex, alpha = alpha)
  }
}


