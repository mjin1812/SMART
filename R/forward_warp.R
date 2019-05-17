#' @title Forward warp back to atlas space
#' @description Loop through all segmented cells and perform forward warp loop
#'   back onto atlas space using registration and segmentation data. Options for
#'   plotting a schematic plot and saving forward warp images. For a whole
#'   brain, this function should be used after the segmentation list *segs* has
#'   been cleaned of duplicate cell counts. Note: if the imaging dataset is
#'   large this will be a time intensive process. Processing time will be
#'   printed once the function is finished.
#' @param setup (required) Setup list from [setup_pl()].
#' @param segs (required) Segmentation data.
#' @param regis (required) Registration data.
#' @param filetype (optional, default = "tif") Image type to save as. See the
#'   *type* argument in the function [savePlot()]. Options: "tif", "tiff",
#'   "wmf", "emf", "png", "jpg", "jpeg", "bmp","ps", "eps", "pdf"
#' @param savewarp (optional, default = TRUE) Save forward warp plot image in
#'   folder 'setup$savepaths$out_segmentation_warps'.
#' @param saveschematic (optional, default = TRUE) Save schematic plot images in
#'   folder 'setup$savepaths$out_segmentation_schem'.
#' @param plane (optional, default = "coronal") Atlas plane to register to.
#'   options: "coronal", "sagittal"
#' @param title (optional, default = FALSE) Title for the schematic plot.
#' @param mm.grid (optional, default = TRUE) Plot grid in schematic plot.
#' @param dev.size (optional, default = c(5.4, 4.465)). See same argument from
#'   [wholebrain::schematic.plot()].
#' @param pch (optional, default = 21) Graphical parameter for point shape.
#' @param cex (optional, default = 0.5) Graphical parameter for text size.
#' @param col (optional, default = "black") Graphical parameter for color.
#' @param scale.bar (optional, default = TRUE)  Display a measure bar in the
#'   schematic plot
#' @param region.colors (optional, default = TRUE)  Display Allen Brain Atlas
#'   colors in schemetic plot.
#' @return Returns *dataset* a variable storing all mapped segmentation data to
#'   registration data.
#' @seealso See also [wholebrain::schematic.plot()].
#' @export
#' @md

forward_warp <- function(setup, segs, regis,
                            filetype = c("tif", "tiff", "wmf", "emf", "png", "jpg", "jpeg", "bmp","ps", "eps", "pdf"),
                            savewarp = TRUE, saveschematic = TRUE,
                            plane = "coronal", title = FALSE,
                            mm.grid = TRUE, dev.size = c(5.4, 4.465),
                            pch = 21, cex = 0.5, col = "black", scale.bar = FALSE, region.colors = TRUE){
  tictoc::tic()
  # Match filetype argument
  filetype <- match.arg(filetype)

  # Match the indices in the registration vector with the appropriate index
  # of the registration vector
  if (length(setup) > 9 ){
    regi_ind <- vector(mode="numeric",length(segs$seg_z))
    for (n in 1:length(segs$seg_z) ) {
      regi_ind[n] <- which.min(abs(setup$regi_z-segs$seg_z[n]))
    }
  } else {
    regi_ind <- 1:length(segs$seg_z)
  }

  if (length(setup$regi_z) > 1) {
    # Interpolate all z images numbers to AP values
    my_function <- approxfun(setup$regi_z, setup$regi_AP, method = "linear")
    z_matched_AP <- my_function(segs$seg_z)
  } else {
    z_matched_AP <- setup$regi_AP
  }

  # Loop through every z-image and perform forward warp
  for (n in 1:length(segs$seg_z)) {
    regi <- regis[[regi_ind[n]]]
    seg  <- segs$segmentations[[n]]

    # perform forward warp
    data <- wholebrain::inspect.registration(regi, seg, soma = TRUE,
                                             forward.warps = TRUE, batch.mode=TRUE)
    if (savewarp) {
      path <- paste0(setup$savepaths$out_segmentation_warps, "/forwardwarp_z_", toString(segs$seg_z[n]),
                     "_AP_", toString(round(regi$coordinate, digits = 2)), "_plate_",
                     platereturn(round(regi$coordinate, digits = 2)), ".", filetype)

      savePlot(filename = path, type = filetype)
    }
    dev.off()

    # clean up null region ids and add animal number
    data <- data[!data$id==0,]
    data$animal <- setup$anim_ID

    # Save schematic plot
    if(saveschematic){
      wholebrain::schematic.plot(dataset = data, plane = plane, save.plots = FALSE, title = title, mm.grid = mm.grid,
                                 dev.size = dev.size, pch = pch, cex = cex, col = "black",
                                 scale.bar = scale.bar, region.colors = region.colors )
      path <- paste0(setup$savepaths$out_segmentation_schem, "/schematic_z_", toString(segs$seg_z[n]),
                     "_AP_", toString(round(regi$coordinate, digits = 2)), "_plate_",
                     platereturn(round(regi$coordinate, digits = 2)), ".", filetype)
      savePlot(filename = path, type = filetype)
      dev.off()
    }

    # Set AP to interpolated value
    data$AP<- z_matched_AP[n]

    # Add to master dataset
    if (n==1) {
      dataset <- data
    } else {
      dataset <- rbind(dataset, data)
    }
  }
  # Return processing
  time <- tictoc::toc(quiet = TRUE)
  cat("\n", toString(round((time$toc - time$tic)/60, digits = 2)), "min elapsed")
  return(dataset)
}
