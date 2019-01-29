#' @title Automatically loops through the image registration process
#' @description  User friendly code to alter atlas to image registration.
#' When run with defaults, this function will automatically create a list called *regis* with the length of setup$regi_z.
#' Each element of that list will be NULL until manual correction to that registration is done.
#' The function will loop through every single image listed in setup$regi_z in order.
#' A simple user interface built into the console is provided to improve each registration.
#' The user has the option to save the environment after every registration.
#' @param setup (required) Setup list from [setup_pl()].
#' @param filter (required) Filter optimized for registration of images in the registration channel.
#' @param regis (optional, default = NULL) List for storing registration output.
#' On the first run of this function, the regis list will be automatically created in the global environment.
#' If it exists in the global environment, this
#' argument **must** be set to *regis = regis*.
#' @param plane (optional, default = "coronal") Atlas plane to register to. options: "coronal", "sagittal"
#' @param closewindow (optional, default = TRUE) If TRUE, windows will close after every correspondance point alteration.
#' @param filetype (optional, default = "tif") Image type to save as. See the *type* argument in the function [savePlot()].
#' Options: "tif", "tiff", "wmf", "emf", "png", "jpg", "jpeg", "bmp","ps", "eps", "pdf"
#' @param autoloop (optional, default = FALSE) Run autoloop to save wholebrain's first pass at
#' registering all registration planes. This argument takes precendence over arguments 'touchup' and 'reference'
#' @param reference (optional, default = FALSE)  Run registration loop on reference planes only if TRUE.
#' This argument takes precendence over argument 'touchup' UNLESS the project is not a whole brain project.
#' Entering argument TRUE on a partial brain project will be ignored.
#' @param touchup (optional, default = FALSE) Run registration loop on a subset of registration plates.
#' Options:
#' + touchup = TRUE runs a user friendly interface allowing user to enter which plates they want to fix.
#' + touchup = numeric vector of plate numbers user wants to fix (plates should be whole integers).
#' @param popup (optional, default = TRUE) Popup the original raw image to be registered.
#' This can help with internal structure visualization.
#' @param brightness (optional, default = 70) Popup image brightness.
#' @param font_col (optional, default = "white")
#' @param font_size (optional, default = 40)
#' @param font_location (optional, default  = "+100+100") Move text from center of gravity parameter in pixels.
#' @param gravity (optional, "southwest") Location of text annotation. e.g. "southwest" means bottom left of screen.
#' @param width (default = 18) Registration plotting window width in inches.
#' @param height (default = 10.2 ) Registration plotting window height in inches.
#' @details This functions creates a vector list of registration information for each registration slice.
#' Note: This is not a return. The vector *regis* is automatically created.
#' @md
#' @export

regi_loop <- function(setup, filter = NULL, regis = NULL, plane = "coronal", closewindow = TRUE,
                                 filetype = c("tif", "tiff", "wmf", "emf", "png", "jpg", "jpeg", "bmp","ps", "eps", "pdf"),
                                 autoloop = FALSE, touchup = FALSE, reference = FALSE, popup = TRUE, brightness = 70, font_col = "white",
                                 font_size = 40, font_location = "+100+100", gravity = "southwest", width = 18, height = 10.2) {

  # Match filetype argument
  filetype <- match.arg(filetype)

  # Create regis vector if necessary
  if (is.null(regis) & !exists("regis", envir = .GlobalEnv)) {
    # Create vector regis to store registration data
    regis <<- vector("list", length = length(setup$regi_z))
  } else if (is.null(regis) & exists("regis", envir = .GlobalEnv)) {
    stop(paste0("The 'regis' vector already exists in the global environment.\n",
                "\nPlease assign the 'regis' variable to the regis argument."))
  }

  if (autoloop) {
    tictoc::tic()
    # Run autoloop if you want to automatically run first pass registration.
    # Output images will be saved in folder structure setup$savepaths$out_auto_registration.
    for (s in 1:length(setup$regi_z)){

      # Get image number and AP number
      imnum <- setup$regi_z[s]
      AP <- setup$regi_AP[s]

      # Register image
      quartz(width, height)
      regis[[s]] <<- wholebrain::registration(setup$image_paths$regi_paths[imnum], AP,
                                              plane = plane, filter = filter, display = TRUE,
                                              output.folder = setup$savepaths$out_registration_warps)

      ## save, annotate, then resave the registration image using magick.
      savepath <- paste0(setup$savepaths$out_auto_registration, "/registration_z_", toString(imnum),
                         "_plate_", toString(platereturn(AP)),
                         "_AP_", toString(round(AP, digits=2)),".", filetype)

      # Saving image. Note: the extra window is to correct for a bug in savePlot in windows
      curwin <- dev.cur()
      quartz()
      savePlot(filename = savepath, type = filetype, device = curwin)
      graphics.off()

      # Annotate and save images using magick package
      image  <- magick::image_read(savepath)
      image  <- magick::image_annotate(image, paste0("Plate ", toString(platereturn(AP)),", AP ",
                                                        toString(round(AP, digits=2)), ", z ", toString(imnum)),
                                        gravity = gravity, size= font_size , color = font_col, location = font_location)
      magick::image_write(image, path = savepath)
    }
    cat("\nRegistration autoloop was successfully completed!\n")
    time <- tictoc::toc(quiet = TRUE)
    cat("\n", toString(round((time$toc - time$tic)/60, digits = 2)), "min elapsed")
  } else {
    # If not auto loop, use registration improvement function for manual improvement

    if (reference & length(setup) > 8 ) {
      # Register ONLY REFERENCE plates. Checks if project is whole brain project first
      # Store reference z and AP values as values to loop through
      loop_z  <- c(setup$first_z, setup$internal_ref_z ,setup$last_z)
      loop_AP <- c(setup$first_AP, setup$internal_ref_AP ,setup$last_AP)

    } else if (isTRUE(touchup) || is.numeric(touchup)) {
      # If modification of a previous registration is wanted
        if (isTRUE(touchup)) {
          # Ask for user input of plate numbers wanted for touchup

          cat("\nInstructions:\nBelow enter the plate numbers of the registrations you want to modify in ascending order.",
              "\nNOTE: non-numeric input will be omitted.")

          val <- TRUE
          while (val) {
            plates <- readline(paste0("Which plates do you want to change? ",
                                      "\nNote: You may use ':' to indicate a range & ',' to separate values."))
            plates <- unlist(strsplit(plates,","))
            plates_col  <- grep(":", plates, value=TRUE)                # points with colon
            plates_sing <- grep(":", plates, value=TRUE, invert=TRUE)   # points without colon in string

            if (length(plates_col)!=0) {
              colvec <-  c()
              flag <- FALSE
              for (p in 1:length(plates_col)){
                suppressWarnings(curvec <- as.integer(unlist(strsplit(plates_col[p], ":"))))
                if (sum(is.na(curvec))> 0) {
                  flag <- TRUE
                  break                                            # break out of loop
                } else {
                  colvec <- c(colvec, curvec[1]:curvec[2])
                }
              }
              plates_sing <- suppressWarnings(as.integer(plates_sing))    # Convert input from string into integer
              plates <- c(colvec, plates_sing)
              suppressWarnings(
                if(sum(is.na(plates)) == 0 & flag == FALSE){
                  val <- FALSE
                })
            } else {
              plates <- suppressWarnings(as.integer(plates_sing))
              suppressWarnings(
                if(!sum(is.na(plates))){
                  val <- FALSE
                })
            }
          }

          # # Remove any NA numbers
          # plates <- plates[!is.na(plates)]

        } else if (is.numeric(touchup)){
          # Round vector input to whole numbers and convert to integers if not already
          plates <- as.integer(round(touchup))
        }

        ## Based on the plate values entered, match up with corresponding AP coordinates in the regi_AP vector
        # store plate numbers that match up to AP values
        regi_plates <- platereturn(setup$regi_AP)

        # Get indices matching values in plates to regi_plates
        indices <- which(regi_plates %in% plates)

        # Store AP and z values of registrations plates needing touchup
        loop_z   <- setup$regi_z[indices]
        loop_AP  <- setup$regi_AP[indices]
    } else {
      # Import regis variable from global environment into the local function environment
      regis <- get('regis', envir= .GlobalEnv)

      # Detect ONLY registration plates that are NULL values (meaning they have not been registered previously)
      indices <- which(unlist(lapply(regis, is.null)))

      if (length(indices) == 0){
        stop("There are no plates that haven't been registered! If you like to change an existing registration, please set touchup = TRUE and regis = regis.")
      }

      # Store AP and z values of registrations plates to loop through
      loop_z   <- setup$regi_z[indices]
      loop_AP  <- setup$regi_AP[indices]
    }

    #### Loop through slices
    for (s in 1:length(loop_z)){
      cat(" __________________________________________________________________________\n",
          "                 You are starting a new registration!               \n",
          "__________________________________________________________________________\n")

      imnum   <- loop_z[s]
      AP      <- loop_AP[s]
      im_path <- setup$image_paths$regi_paths[imnum]
      index   <- which(imnum==setup$regi_z)

      if (popup) {
        # plot popup image annotated
        # quartz()    # dummy window
        image <- magick::image_read(im_path)
        image <- magick::image_normalize(image)
        image <- magick::image_modulate(image, brightness = brightness)
        image <- magick::image_contrast(image)
        image <- magick::image_annotate(image, paste0("Plate ", toString(platereturn(AP)),", AP ",
                                                      toString(round(AP, digits=2)), ", z ", toString(imnum)),
                                        gravity = gravity, size= font_size , color = font_col, location = font_location)
        quartz(canvas="black", title= paste("z-slice ", toString(imnum)))
        popup_cur <- dev.cur()
        plot(image)

        quartz(width, height)    # dummy window
        # Run registration improvement loop
        regis[[index]] <<- registration2(im_path, coordinate = AP, filter = filter,
                                         correspondance = regis[[index]], plane = plane,
                                         closewindow = closewindow,
                                         output.folder = setup$savepaths$out_registration_warps,
                                         width = width, height = height)
      } else {
        # Run registration loop without popup window
        # window
        quartz(width, height)

        # Run registration improvement loop
        regis[[index]] <<-list(NULL) # Clear memory space
        regis[[index]] <<- registration2(im_path, coordinate = AP, filter = filter,
                                         correspondance = regis[[index]], plane = plane,
                                         closewindow = closewindow,
                                         output.folder = setup$savepaths$out_registration_warps,
                                         width = width, height = height)
      }

      ## save, annotate, then resave the registration image using magick.
      savepath <- paste0(setup$savepaths$out_registration, "/registration_z_", toString(imnum),
                         "_plate_", toString(platereturn(AP)),
                         "_AP_", toString(round(AP, digits=2)),".", filetype)

      # Saving image. Note: the extra window is to correct for a bug in savePlot in windows
      curwin <- dev.cur()
      quartz()
      savePlot(filename = savepath, type = filetype, device = curwin)
      graphics.off()

      # Annotate and save images using magick package
      image  <- magick::image_read(savepath)
      image  <- magick::image_annotate(image, paste0("Plate ", toString(platereturn(AP)),", AP ",
                                                     toString(round(AP, digits=2)), ", z ", toString(imnum)),
                                       gravity = gravity, size= font_size , color = font_col, location = font_location)
      magick::image_write(image, path = savepath)

      # Ask user whether they would like to save the environment after every registration
      done <- FALSE
      while (!done) {
        input <- readline("Would you like to save the global environment after this registration? Y/N")
        if (input == 'Y' | input == 'y') {
          save.image(file = setup$savepaths$envir_savepath)
          done <- TRUE
        } else if (input == 'N' | input == 'n') {
          done <- TRUE
        }
      }
    }
  }
}
