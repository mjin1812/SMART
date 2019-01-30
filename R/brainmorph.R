

#' @title Generate a brain morph plot **(W)**
#' @description Generate a brain morph plot (normalized to 1). For use with mapping whole brain only.
#' Creates a plot that can be easily modified using 'par()' to query or set graphical parameters
#' @param setup (required) Setup list from [setup_pl()].
#' @param saveplot (optional, default=TRUE) Save the plot.
#' Note: saveplot will automatically save the device without leaving the device open
#' @param filetype (optional, default = "tif") Image type to save as. See the *type* argument in the function *saveplot.*
#' Options: "tif", "tiff", "wmf", "emf", "png", "jpg", "jpeg", "bmp","ps", "eps", "pdf"
#' @param aspect.ratio (optional, default = 0.9) Aspect ratio of the graph.
#' @param title_size (optional, default = 25)
#' @param x_title_size (optional, default = 20)
#' @param y_title_size (optional, default = 20)
#' @param pointsize (optional, default = 4)
#' @param linesize (optional, default = 1.5)
#' @param y_lim (optional, default = c(0, 1.6)) Lower and upper limits of y-axis.
#' @param y_by (optional, default = 0.2) Tick mark interval of y-axis.
#' @export
#' @md

brainmorph <- function(setup, saveplot = TRUE,
                          filetype = c("tif", "tiff", "wmf", "emf", "png", "jpg", "jpeg", "bmp","ps", "eps", "pdf"),
                      aspect.ratio = .9, title_size = 25, x_title_size = 20, y_title_size = 20,
                      pointsize = 4, linesize = 1.5, y_lim =  c(0, 1.6), y_by = 0.2) {


  AP_ref <- roundAP(c(setup$first_AP, setup$internal_ref_AP, setup$last_AP))
  z_ref  <- c(setup$first_z, setup$internal_ref_z, setup$last_z)

  # Fit a monotonic spline over reference point data
  my_function <- splinefun(AP_ref, z_ref, method="monoH.FC")
  lin_slope   <- mean(my_function(AP_ref, deriv=1))
  by          <- -0.05

  # The line below does 2 things: 1) Uses monotonic spline function to find
  # first derivative (immediate expansion rate) over average expansion rates at
  # reference points to normalize to 1 2) Uses these normalized expansion rates
  # at references points to create another spline function to interpolate
  # expansion rates across the brain
  my_function2 <- splinefun(AP_ref, my_function(AP_ref, deriv=1)/lin_slope, method = "fmm")


  # plot brainmorph with ggplot2
  # Total
  AP_coor <- seq(from = 3.0, to = -5.0 , by = by)
  exp_ratio <- my_function2(AP_coor)
  morph_data <- data.frame(AP_coor, exp_ratio)

  # Reference
  ref_ratio <- my_function2(AP_ref)
  ref_morph <- data.frame(AP_ref, ref_ratio)

  library(ggplot2)
  options(warn=-1)
  g <- ggplot(data = morph_data,  aes(x = AP_coor, y = exp_ratio)) +
    theme_bw() +
    scale_x_continuous(breaks =  seq(from = -5.0, to = 3.0 , by = 1.0),
                       labels =  unlist(strsplit(toString(seq(from = -5.0, to = 3.0 , by = 1.0)), ",")),
                       trans = "reverse") +
    scale_y_continuous(breaks = seq(from = y_lim[1], to = y_lim[2], by = y_by), limits = y_lim) +
    theme(axis.line = element_line(colour = "black")) +
    labs(title = "Normalized expansion", x = "AP coordinate (mm)", y = "Expansion ratio") +
    theme(plot.title = element_text(hjust = 0.5, size = title_size, margin = margin(b = 20)),
          axis.title.x = element_text(size = x_title_size, margin = margin(t = 15)),
          axis.title.y = element_text(size = y_title_size, margin = margin(r = 20)),
          axis.text.x =  element_text(size = 15, margin = margin(t = 5)),
          axis.text.y =  element_text(size = 15, margin = margin(r = 5)),
          aspect.ratio = aspect.ratio) +
    geom_line(size = linesize, show.legend = FALSE) +
    geom_point(data = ref_morph, mapping = aes(x = AP_ref, y = ref_ratio), colour = "red", size = pointsize,
               show.legend = FALSE)

  quartz()
  plot(g)

  # Saving plot
  if (saveplot) {
    filetype <- match.arg(filetype)
    savepath <- file.path(setup$savepaths$out_RC_brain_morph, paste0("Animal_", setup[[1]], "_brain_morph_", setup[[2]],".", filetype))
    savePlot( filename = savepath, type = filetype)
    dev.off()
  }
  detach(package:ggplot2)
  options(warn=0)
}
