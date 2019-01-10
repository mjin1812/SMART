#' @title Alignment choice game to align internal reference planes **(W)**.
#' @description  User friendly way to play the wholebrain choice game to align
#'   internal reference atlas plates. Automatically saves images of chosen
#'   aligned images in the folder specified by
#'   *savepaths$out_reference_aligned*. For use with mapping whole brain only.
#' @param setup (required) Setup list is used to access internal reference AP
#'   coordinates.
#' @param savepaths (required) Paths to data directories.
#' @param image_paths (required) Paths to access registration images.
#' @param touchup (default = NA) Enter a vector of reference AP coordinates. The
#'   choice game will replay for just those points. Note: Numbers entered will
#'   be rounded to the nearest atlas plates. No need to enter exact atlas AP
#'   numbers.
#' @param midpoint (default = FALSE) Play the midpoint game following choice the
#'   choice game.
#' @param filetype (optional, default = "tif") Image format to save. Setting
#'   this argument as a vector of image formats, e.g. c("tif", "png") will save
#'   both image types. Options: common image formats like "tiff", "png", "jpeg",
#'   "gif", "rgb" or "rgba". See [magick] package documentation for more info on
#'   the [image_write()] function
#' @param xpos (optional, default = c(0, 660, 1250)) A vector of x-positions in
#'   pixels justified to the left of the main computer screen for the three
#'   choice windows.
#' @param brightness (optional, default = 70) Popup image brightness.
#' @param font_col (optional, default = "white") Image annotation font color.
#' @param font_size (optional, default = 80) Image annotation font size.
#' @param font_location (optional, default  = "+100+30") Text position from
#'   center of gravity parameter in pixels
#' @param choice_step (optional, default= c(200,100,30,10)) Determines the
#'   successive "zoom" of images picked on either side of the center image.
#' @return The argument assigned to *setup* is returned with aligned internal z
#'   numbers matching internal AP coordinates.
#' @md
#' @export

choice <- function(setup, savepaths, image_paths, touchup = NA, midpoint = FALSE, filetype = c("tif"),
                   xpos = c(0, 660, 1250), brightness = 70,
                   font_col = "white", font_size = 80, font_location = "+100+30",
                   gravity = "southwest", choice_step = c(200,100,30,10)) {

  # Interpolate z numbers base on first and last aligned imagesgi
  fl_AP       <- c(setup$first_AP, setup$last_AP)
  fl_z        <- c(setup$first_z, setup$last_z)
  ref_AP      <- sort(c(fl_AP, setup$internal_ref_AP), decreasing = TRUE)
  ref_z       <- sort(c(fl_z, setup$internal_ref_z))

  if (isFALSE(midpoint) && all(is.na(touchup))) {

    # Estimate z numbers
    my_function <- approxfun(fl_AP, fl_z, method = "linear")
    loop_z       <- sort(c(fl_z, round(my_function(setup$internal_ref_AP))))
    loop_AP      <- ref_AP

  } else if (isTRUE(midpoint) && all(is.na(touchup))) {

    my_function2  <- approxfun(ref_AP, ref_z, method = "linear")
    midpnt_ref_AP <- roundAP((ref_AP[2:length(ref_AP)]+ref_AP[1:length(ref_AP)-1])/2)
    midpnt_ref_z  <- round(my_function2(midpnt_ref_AP))

    loop_AP <- c()
    loop_z  <- c()

    cat("Please the check the estimated midpoints.
        If you do not like them, midpoints will be used as another reference point.")

    for (n in 1:length(midpnt_ref_z) ) {
      refpath  <-  image_paths$regi_paths[midpnt_ref_z[n]]
      ref_im  <- magick::image_read(refpath)
      ref_im  <- magick::image_normalize(ref_im)
      ref_im  <- magick::image_modulate(ref_im, brightness = brightness)
      ref_im  <- magick::image_contrast(ref_im)
      ref_im  <- magick::image_annotate(ref_im, paste0("Plate ", toString(platereturn(midpnt_ref_AP[n])),", AP ",
                                                       toString(round(midpnt_ref_AP[n], digits=2)), ", est. z ",
                                                       toString(midpnt_ref_z[n])), gravity = gravity, size= font_size,
                                        color = font_col, location = font_location)
      quartz(canvas="black", title= paste("z-slice ", toString(midpnt_ref_z[n])))
      plot(ref_im)
      line  <- TRUE
      while (line != "Y" & line!="y" & line !="N" & line != "n"){
        line <- readline("Do you like the image alignment? Y/N:" )
        if (line == "N" || line == "n"){
          loop_AP   <- c(loop_AP, midpnt_ref_AP[n])
          loop_z    <- c(loop_z , midpnt_ref_z[n])
        }
      }
      graphics.off()
    }

    if(is.null(new_z)) {
      stop("You liked all the midpoints! You don't need to play the choice game again.")
    } else {
      cat("Play the choice game for your newly added reference points!")
    }

  } else if (is.numeric(touchup)) {
    overlap <- setup$internal_ref_AP %in% roundAP(touchup)
    if (all(!overlap)) {
      stop("You did not enter any reference AP values from `setup$internal_ref_AP.` to `touchup`")
    } else {
      loop_z  <- setup$internal_ref_z[overlap]
      loop_AP <- setup$internal_ref_AP[overlap]
    }
  }


### Choice game ####

  cat("Play the choice game!")

  # looping through reference points
  for (n in 1:length(loop_z)){

    # Corresponding z number and AP number
    ref_num  <-  loop_z[n]
    AP       <-  loop_AP[n]

    if (!(AP == fl_AP[1]) && !(AP == fl_AP[2])) {
      # Index of the choice_step vector
      stpcnt    <- 1
      # Set to zero when closest image is picked
      while (stpcnt != 0) {
        # calculating reference images to plot and choose from
        for (i in 1:3) {
          im_num   <- ref_num + i*choice_step[stpcnt]-2*choice_step[stpcnt]

          # accounts for calculated images references that are out-of-bounds
          if (im_num <= 0) {
            im_num <- 1
          } else if (im_num >= length(image_paths$regi_paths)) {
            im_num <- length(image_paths$regi_paths)
          }

          # autonormalizing image brightness and contrast and plotting
          refpath <- image_paths$regi_paths[im_num]
          ref_im <- magick::image_read(refpath)
          ref_im <- magick::image_normalize(ref_im)
          ref_im <- magick::image_modulate(ref_im, brightness = brightness)
          ref_im <- magick::image_contrast(ref_im)
          ref_im <- magick::image_annotate(ref_im, paste(toString(i), ", ", toString(im_num)),
                                           gravity = gravity, size = font_size, color = font_col , location = font_location)
          quartz(canvas="black", title= paste("z-slice ", toString(im_num)), xpos= xpos[i])
          plot(ref_im)
        }

        cat(paste("\nYour reference AP is", toString(round(AP, digits=2)),
                  "\nYour current image sampling choice_step is ", toString(choice_step[stpcnt]), '\n\n'))

        # use input to choose reference image
        inp <- readline("Which image matches best?\nEnter 1, 2 or 3:")
        while (inp != '1' && inp != '2' && inp != '3'   ) {
          inp <- readline("Which image matches best?\nEnter 1, 2 or 3:")
        }

        # closing all graphics windows
        graphics.off()

        if( inp =='1'){
          ref_num <- ref_num - choice_step[stpcnt]
        } else if (inp =='2') {
          stpcnt <- stpcnt + 1    # 'zoom' in and change to a different choice_step count
          if (stpcnt==length(choice_step)+1) {
            stpcnt <- 0
          }
        } else if (inp == '3'){
          ref_num <- ref_num + choice_step[stpcnt]
        }
      }
      # Saving chosen image
      loop_z[n] <- ref_num
    } else {
      refpath  <-  image_paths$regi_paths[ref_num]
    }

    # Saving chosen image
    ref_im  <- magick::image_read(refpath)
    ref_im  <- magick::image_normalize(ref_im)
    ref_im  <- magick::image_modulate(ref_im, brightness = brightness)
    ref_im  <- magick::image_contrast(ref_im)
    ref_im  <- magick::image_annotate(ref_im, paste0( "Plate ", toString(platereturn(AP)),", AP ",
                                                      toString(round(AP, digits=2)), ", z ", toString(ref_num)),
                                      gravity = gravity, size= font_size , color = font_col, location = font_location)

    for (t in 1:length(filetype)) {
      magick::image_write(ref_im, path = paste0(savepaths$out_reference_aligned,"/z_",toString(ref_num),
                                                "_plate_", toString(platereturn(AP)), "_AP_",
                                                toString(round(AP, digits=2)),".", filetype[t]), format = filetype[t])
    }
  }

### choice game end ###

  # Remove any values belonging to first or last reference points
  loop_AP2 <- setdiff(loop_AP, fl_AP)
  loop_z2   <- loop_z[loop_AP %in% loop_AP2]

  # Overwrite any old values
  setup$internal_ref_z[setup$internal_ref_AP %in% intersect(loop_AP2, setup$internal_ref_AP)] <- loop_z2[loop_AP2 %in% intersect(loop_AP2, setup$internal_ref_AP)]

  # Consolidate any new mid points
  new_AP <- setdiff(loop_AP2, setup$internal_ref_AP )
  new_z  <- loop_z2[loop_AP2 %in% new_AP]

  setup$internal_ref_AP <- sort(c(setup$internal_ref_AP, new_AP), decreasing = TRUE)
  setup$internal_ref_z  <- sort(c(setup$internal_ref_z, new_z))

# store new aligned imaged choices in setup list
  return(setup)

}


