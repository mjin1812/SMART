#' @title Console user interface built on top of [registration()] function from the wholebrain package.
#' @description Allows you to alter correspondance points with user interface with the console
#' This uses the original [registration()] function from wholebrain and retains the same arguments.
#' @param input (required) A character vector consisting of the full path name to 16-bit raw tif image files.
#' @param coordinate (default = NULL) Matching coordinates in the main plane to be registered to (in millimeters).
#' @param plane (default = "coronal") The main plane to register to: "coronal", "sagittal".
#' @param right.hemisphere (default = NULL)
#' @param interpolation (default = "tps")
#' @param intrp.param (default = NULL)
#' @param brain.threshold (default = 200) An integer value, which determinesthe segmentation of the brain slice.
#' @param blurring (default = c(4, 15))
#' @param pixel.resolution (default = 0.64)
#' @param resize (default = (1/8)/4)
#' @param correspondance (default = NULL)
#' @param resolutionLevel (default = c(4, 2))
#' @param num.nested.objects (default = 0)
#' @param plateimage (default = FALSE)
#' @param forward.warp (default = FALSE)
#' @param filter (default = NULL)
#' @param output.folder (default = "../")
#' @param batch.mode (default = FALSE)
#' @param channel (default = 0)
#' @param verbose (default = TRUE) If TRUE diagnostic output is written to the R console. Default is true.
#' @param closewindow (default = TRUE) If TRUE, windows will close after every correspondance point alteration.
#' @param width (default = 18) Plotting window width in inches.
#' @param height (default = 10.2 ) Plotting window height in inches.
#' @return returns *regi*, a vector list of registration information for each registration slice.
#' @seealso See also [wholebrain::registration()] for the original registration function from wholebrain.
#' @export
#' @md

registration2 <- function(input, coordinate = NULL, plane = "coronal",
                            right.hemisphere = NULL, interpolation = "tps", intrp.param = NULL,
                            brain.threshold = 200, blurring = c(4, 15),
                            pixel.resolution = 0.64, resize = (1/8)/4, correspondance = NULL,
                            resolutionLevel = c(4, 2), num.nested.objects = 0,
                            plateimage = FALSE, forward.warp = FALSE, filter = NULL,
                            output.folder = "../", batch.mode = FALSE, channel = 0,
                            verbose = TRUE, closewindow = TRUE, width = 18, height = 10.2) {

  regi <- wholebrain::registration(input, coordinate = coordinate, plane = plane,
                       right.hemisphere = right.hemisphere, interpolation = interpolation, intrp.param = intrp.param,
                       brain.threshold = brain.threshold, blurring = blurring,
                       pixel.resolution = pixel.resolution, resize = resize, correspondance = correspondance,
                       resolutionLevel = resolutionLevel, num.nested.objects = num.nested.objects,
                       plateimage = plateimage, forward.warp = forward.warp, filter = filter,
                       output.folder =  output.folder, batch.mode = batch.mode, channel = channel,
                       verbose = verbose)

  ### START OF REGISTRATION IMPROVEMENT LOOP ###

  # Initialize value for while loop check
  regi_done <- FALSE
  while (!regi_done ) {
      corr_done <- FALSE

      while (corr_done!= "A" & corr_done!= "a" & corr_done!= "C" & corr_done!= "c" & corr_done!= "R" & corr_done !=
             "r" & corr_done!= "F" & corr_done!= "f" & corr_done!= "Z" & corr_done!= "z" ) {
        # Loop ends when user has entered whether they want to adjust correspondance points
        corr_done <- readline(paste0("Do you want to modify any correspondance points?\n",
                                     "If add, enter               : A\n",
                                     "If change, enter            : C\n",
                                     "If remove, enter            : R\n",
                                     "If finished, enter          : F\n",
                                     "If revert to previous change: Z\n"))

        if (corr_done == "Z" | corr_done == "z") {
          # If user doesn't like recent changes, revert to previous registration file
          cat("This is your previous registration.\n\n")
          quartz(width, height)
          regi_new <- wholebrain::registration(input, coordinate = coordinate, plane = plane,
                                               right.hemisphere = right.hemisphere, interpolation = interpolation, intrp.param = intrp.param,
                                               brain.threshold = brain.threshold, blurring = blurring,
                                               pixel.resolution = pixel.resolution, resize = resize, correspondance = regi,
                                               resolutionLevel = resolutionLevel, num.nested.objects = num.nested.objects,
                                               plateimage = plateimage, forward.warp = forward.warp, filter = filter,
                                               output.folder =  output.folder, batch.mode = batch.mode, channel = channel,
                                               verbose = verbose)
        } else {
          if (exists("regi_new")) {
            regi <- regi_new
          }

          if (corr_done == "A" | corr_done == "a") {

            ## User wants to add correspondance pts ##
            pts <- FALSE
            while (!is.integer(pts)) {
              pts <- readline ("How many points?  ")
              pts <- suppressWarnings(as.integer(pts))               # Convert input from string into integer

              if(is.na(pts)){                                        # Adjusts for input that isn't numerical
                pts <- FALSE
              }
            }

            # plot new registration based on added points
            regi_new <- wholebrain::add.corrpoints(regi, pts)

            # Close previous window depending on user argument
            if (closewindow) {
              dev.off()
            }
            quartz(width, height)
            regi_new <- wholebrain::registration(input, coordinate = coordinate, plane = plane,
                                                 right.hemisphere = right.hemisphere, interpolation = interpolation, intrp.param = intrp.param,
                                                 brain.threshold = brain.threshold, blurring = blurring,
                                                 pixel.resolution = pixel.resolution, resize = resize, correspondance = regi_new,
                                                 resolutionLevel = resolutionLevel, num.nested.objects = num.nested.objects,
                                                 plateimage = plateimage, forward.warp = forward.warp, filter = filter,
                                                 output.folder =  output.folder, batch.mode = batch.mode, channel = channel,
                                                 verbose = verbose)
          } else if (corr_done =="C" | corr_done =="c") {
            ## User wants to change correspondance pts ##
            val <- TRUE
            while (val) {
              pts <- readline ("Which points do you want to change? ")
              pts <- unlist(strsplit(pts,","))
              pts_col  <- grep(":", pts, value=TRUE)                # points with colon
              pts_sing <- grep(":", pts, value=TRUE, invert=TRUE)   # points without colon in string

              if (length(pts_col)!=0) {
                colvec <-  c()
                flag <- FALSE
                for (p in 1:length(pts_col)){
                  suppressWarnings(curvec <- as.integer(unlist(strsplit(pts_col[p], ":"))))
                  if (sum(is.na(curvec))> 0) {
                    flag <- TRUE
                    break                                            # break out of loop
                  } else {
                    colvec <- c(colvec, curvec[1]:curvec[2])
                  }
                }
                pts_sing <- suppressWarnings(as.integer(pts_sing))    # Convert input from string into integer
                pts <- sort(c(pts_sing, colvec))
                suppressWarnings(
                  if(sum(is.na(pts)) == 0 & flag == FALSE){
                    val <- FALSE
                  })
              } else if (length(pts_sing)!=0) {
                pts <- suppressWarnings(sort(as.integer(pts_sing)))
                suppressWarnings(
                  if(!sum(is.na(pts))){
                    val <- FALSE
                  })
              }
            }
            # Plot new registration based on changed points
            regi_new <- wholebrain::change.corrpoints(regi, pts)

            # Close previous window depending on user argument
            if (closewindow) {
              dev.off()
            }

            quartz(width, height)
            regi_new <- wholebrain::registration(input, coordinate = coordinate, plane = plane,
                                                 right.hemisphere = right.hemisphere, interpolation = interpolation, intrp.param = intrp.param,
                                                 brain.threshold = brain.threshold, blurring = blurring,
                                                 pixel.resolution = pixel.resolution, resize = resize, correspondance = regi_new,
                                                 resolutionLevel = resolutionLevel, num.nested.objects = num.nested.objects,
                                                 plateimage = plateimage, forward.warp = forward.warp, filter = filter,
                                                 output.folder =  output.folder, batch.mode = batch.mode, channel = channel,
                                                 verbose = verbose)

          } else if (corr_done == "R" | corr_done == "r") {

            ## User wants to remove correspondance pts ##
            val <- TRUE
            while (val) {
              pts <- readline ("Which points do you want to change? ")
              pts <- unlist(strsplit(pts,","))
              pts_col  <- grep(":", pts, value=TRUE)                # points with colon
              pts_sing <- grep(":", pts, value=TRUE, invert=TRUE)   # points without colon in string

              if (length(pts_col)!=0) {
                colvec <-  c()
                flag <- FALSE
                for (p in 1:length(pts_col)){
                  suppressWarnings(curvec <- as.integer(unlist(strsplit(pts_col[p], ":"))))
                  if (sum(is.na(curvec))> 0) {
                    flag <- TRUE
                    break                                            # break out of loop
                  } else {
                    colvec <- c(colvec, curvec[1]:curvec[2])
                  }
                }
                pts_sing <- suppressWarnings(as.integer(pts_sing))    # Convert input from string into integer
                pts <- sort(c(pts_sing, colvec))
                suppressWarnings(
                  if(sum(is.na(pts)) == 0 & flag == FALSE){
                    val <- FALSE
                  })
              } else if (length(pts_sing)!=0) {
                pts <- suppressWarnings(sort(as.integer(pts_sing)))
                suppressWarnings(
                  if(!sum(is.na(pts))){
                    val <- FALSE
                  })
              }
            }
            # Plot new registration based on removed points
            regi_new <- wholebrain::remove.corrpoints(regi, pts)

            # Close previous window depending on user argument
            if (closewindow) {
              dev.off()
            }
            quartz(width, height)
            regi_new <- wholebrain::registration(input, coordinate = coordinate, plane = plane,
                                                 right.hemisphere = right.hemisphere, interpolation = interpolation, intrp.param = intrp.param,
                                                 brain.threshold = brain.threshold, blurring = blurring,
                                                 pixel.resolution = pixel.resolution, resize = resize, correspondance = regi_new,
                                                 resolutionLevel = resolutionLevel, num.nested.objects = num.nested.objects,
                                                 plateimage = plateimage, forward.warp = forward.warp, filter = filter,
                                                 output.folder =  output.folder, batch.mode = batch.mode, channel = channel,
                                               verbose = verbose)
          } else if (corr_done =="F" | corr_done =="f") {
            ## User is done changing pts --> exit registration improvement loop  ##
            if (exists("regi_new")) {
              rm("regi_new")
            }
            regi_done <- TRUE
          }
        }
      }
  }
  ### END OF REGISTRATION IMPROVEMENT LOOP ###
  return(regi)
}
