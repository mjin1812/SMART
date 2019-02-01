#' @title Loop through and segment images in the segmentation channel
#' @description Loop through the z images in the segmentation folder path. For a
#'   whole brain, every N images is segmented, where N is equal to
#'   `setup$seg_step.` For a partial brain, all the images in `setup$regi_z` are
#'   segmented. The function will return a list called *segs* of length 2. Note:
#'   if the imaging dataset is large this will be a time intensive process.
#'   Processing time will be printed once the function is finished.
#' @param setup (required) Setup list from [setup_pl()].
#' @param filter (required) Filter optimized for segmentation of images in the segmentation channel.
#' @param numthresh (optional, default = 8) Original argument from [wholebrain::segment()].
#' @param downsample (optional, default = 0.25) Original argument from [wholebrain::segment()].
#' @param post (optional, default = NULL) Original argument from [wholebrain::segment()].
#' @param pre (optional, default = NULL) Original argument from [wholebrain::segment()].
#' @param get.contour (optional, default = FALSE) Original argument from [wholebrain::segment()].
#' @param channel (optional, default = 0) Original argument from [wholebrain::segment()].
#' @return returns *segs* A list of length 2. Each list element is described below:
#' 1. A vector of the z-image number corresponding to each segmented slice stored in the 2nd element.
#' 2. A list of segmentation information for each segmentated image.
#' @seealso See also the [wholebrain::segment()] function.
#' @export
#' @md


seg_loop <- function(setup, filter, numthresh = 8, downsample = 0.25, post = NULL,
                                 pre = NULL, get.contour = FALSE, channel = 0) {
  tictoc::tic()
  if (length(setup) > 9) {
    # If whole brain dataset, segment every N images, where N is equal to 'setup$seg_step.'
    # Store image numbers to segment in 'seg_z'
    seg_z <- seq(from = setup$first_z, to = setup$last_z, by = setup$seg_step)
  } else {
    # If not wholebrain dataset, segment all the images in setup$regi_z
    seg_z <- setup$regi_z
  }
  # Create segmentation list
  segs        <- vector("list", length=2)
  names(segs) <- c("seg_z", "segmentations")
  segs$seg_z  <- seg_z
  segs$segmentations <- vector("list", length = length(seg_z))


  for (s in 1:length(seg_z)) {
   # Setup loop variables
   imnum   <- seg_z[s]
   im_path <- setup$image_paths$seg_paths[imnum]

   # Segment and store segmentation in segs list
   segs$segmentations[[s]] <- wholebrain::segment(im_path, numthresh = 8, downsample = downsample, filter = filter,
                            post = post, pre = pre, get.contour = get.contour, channel = channel,
                            display = FALSE)
  }
  return(segs)
  # Return processing
  time <- tictoc::toc(quiet = TRUE)
  cat("\n", toString(round((time$toc - time$tic)/60, digits = 2)), "min elapsed")
}
