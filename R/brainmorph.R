#' @title Generate a brain morph plot
#' @description Generate a brain morph plot (normalized to 1). For use with mapping whole brain only.
#' Creates a plot that can be easily modified using 'par()' to query or set graphical parameters
#' @param setup (required) Argument is used to get internal reference AP and z coordinates.
#' @param savepaths (required) Save directory path.
#' @param saveplot (optional, default=TRUE) Save the plot.
#' Note: saveplot will automatically save the device without leaving the device open
#' @param filetype (optional, default = "tif) Image type to save as. See the *type* argument in the function *saveplot.*
#' Options: "tif", "tiff", "wmf", "emf", "png", "jpg", "jpeg", "bmp","ps", "eps", "pdf"
#' @param lineplot (optional, default = TRUE) Plot horizontal line at y = 1.0.
#' @export
#' @md

brainmorph <- function(setup, savepaths, lineplot = TRUE, saveplot = TRUE,
                          filetype = c("tif", "tiff", "wmf", "emf", "png", "jpg", "jpeg", "bmp","ps", "eps", "pdf")) {

  setwd(setup$work_dir)
  # ##### Get necessary functions NOTE: these will be modified to be internal package functions later
  # source('wb_functions.R')
  # #####

  AP <- roundAP(c(setup$first_AP, setup$internal_ref_AP, setup$last_AP))
  z  <- c(setup$first_z, setup$internal_ref_z, setup$last_z)

  # Fit a monotonic spline over reference point data
  my_function <- splinefun(AP, z, method="monoH.FC")
  lin_slope   <- mean(my_function(AP, deriv=1))
  by          <- -0.05

  ## SCRATCH PLOT, FOR TESTING ONLY
  # windows()
  # plot(seq(from=3.0, to =-5.0 , by= by),  my_function(seq(from=3.0, to =-5.0 , by= by), deriv=1)/lin_slope,  "l" ,
  #      xlim=rev(range(AP)), ylim=c(.5, 1.5),
  #      main="1st deriv. plot")
  # points(AP, my_function(AP, deriv=1)/lin_slope, pch = 19, col="red", bg="red"  )
  ##

  # The line below does 2 things:
  # 1) Uses monotonic spline function to find first derivative (immediate expansion rate) over average expansion rates at reference points to normalize to 1
  # 2) Uses these normalized expansion rates at references points to create another spline function to interpolate expansion rates across the brain
  my_function2 <- splinefun(AP, my_function(AP, deriv=1)/lin_slope, method = "fmm" )

  # Plot the normalized brain morph plot
  windows()
  plot(seq(from = 3.0, to = -5.0 , by = by), my_function2(seq(from = 3.0, to = -5.0 , by = by)), "l",lwd = 3,
       xlab = "AP coordinate (mm)",
       xlim = rev(range(AP)),
       ylab = "Expansion ratio",
       ylim = c(0, 1.5),
       yaxp = c(0, 1.5, 15),
       main = "Normalized expansion",
       cex.axis = 1.2,
       cex.lab = 1.5,
       cex.main = 1.5)
  points(AP, my_function(AP, deriv=1)/lin_slope, pch = 19, col="red", bg="red",  lwd=8)

  #
  # ##########
  # #
  # xlab = "AP coordinate (mm)",
  # xlim = rev(range(AP)),
  # ylab = "Expansion ratio",
  # ylim = c(0, 1.5),
  # yaxp = c(0, 1.5, 15),
  # main = "Normalized expansion",
  # cex.axis = 1.2,
  # cex.lab = 1.5,
  # cex.main = 1.5
  #
  # ##########

  # Plotting line if lineplot is TRUE
  if (lineplot) {
    abline(a=1.0, b=0, h=1.0)
  }

  # Saving plot
  if (saveplot) {
    filetype <- match.arg(filetype)
    savepath <- file.path(savepaths$out_RC_brain_morph, paste0("Animal_", setup[[1]], "_brain_morph_", setup[[2]],".", filetype))
    savePlot( filename = savepath, type = filetype)
    dev.off()
  }
}
