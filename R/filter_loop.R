#' @title Loop through reference slices
#' @description This function will take a filter and loop through all images in a partial brain
#' or just the reference images across the brain in either the registration or segmentation channel.
#' The user can check qualitatively whether the filter needs adjustment after every image check. There is a console
#' menu option for the user to adjust filter parameters if they need. If the filter has been changed by the
#' user, either through the GUI or through the console menu, the function can reloop through
#' the start of the image sequence.
#' @param setup (required) Setup list from [setup_pl()].
#' @param channel (optional, default = "regi") Which channel you are checking the filter for.
#' @param filter (optional, default = NULL) User entered custom filter.
#' @param console (optional, default = TRUE) Display console interface to modify filter parameters
#' instead of using the built-in GUI.
#' @seealso See also the [wholebrain::segment()] function in wholebrain.
#' @return Returns *filter* with adjusted values.
#' @export
#' @md

filter_loop <- function(setup, channel = c("regi", "seg"), filter = NULL, console = TRUE){

  #________________SETUP________________

  # Match arguments
  channel <- match.arg(channel)

  if (channel == "regi") {
    c <- 1
  } else {
    c <- 2
  }

  # Setup loop for whole or partial brain
  if (length(setup) > 9) {
    # If a whole brain
    loop_z  <- c(setup$first_z, setup$internal_ref_z, setup$last_z)
  } else {
    # If a partial brain
    loop_z  <- setup$regi_z
  }

  if (console) {
    # Setup user prompts for filter adjustments
    user_prompt <- vector(mode = "list", length = 9)
    user_prompt[[1]]  <- paste0("1) Enter your alim: ",
                                "\n Please enter 2 integer numbers with values separated by a ','\n" )
    user_prompt[[2]]  <- paste0("2) Enter your threshold range: ",
                                 "\n Please enter 2 integer numbers with values separated by a ','\n" )
    user_prompt[[3]]  <- "Enter your eccentricity value: \n"
    user_prompt[[4]]  <- "Enter your Max value: \n"
    user_prompt[[5]]  <- "Enter your Min value: \n"
    user_prompt[[6]]  <- "Enter your brain.threshold: \n"
    user_prompt[[7]]  <- "Enter your resize parameter: \n"
    user_prompt[[8]]  <- "Enter your blur value: \n"
    user_prompt[[9]]  <- "Enter your downsample value: \n"
  }

  # If no filter is set, we create a filter and defaults (based on our own imaging parameters).
  if (is.null(filter)){
    # Set your own if you'd like to adjust the parameters of your own filter
    if (c == 1){
      filter<-structure(list(alim = c(50, 50),
                             threshold.range = c(50000, 60000L),
                             eccentricity = 999L,
                             Max = 1000,
                             Min = 150,
                             brain.threshold = 250L,
                             resize = 0.25,
                             blur = 7L,
                             downsample = 2))
    } else if (c == 2) {
      filter<-structure(list(alim = c(6, 100),
                                threshold.range = c(1700, 10000L),
                                eccentricity = 300L,
                                Min = 700,
                                Max = 10000,
                                brain.threshold = 400L,
                                resize = 0.25,
                                blur = 7L,
                                downsample = 2))
    }
  }

#______________Loop start_______________

  # Initialized counter for ref slice position
  s   <- 1

  while(s <= length(loop_z)) {
    old_filter <- filter
    z <- loop_z[s]
    seg <- wholebrain::segment(setup$image_paths[[c]][z], filter = filter)
    filter <- seg$filter

#________console interface start ________
    if (console) {
      change_done <-FALSE
      while (!change_done) {
        print(filter)
        cat("Please review your filter parameters above: ")
        inp <- readline("Do you want to change any settings: Y/N?" )
        if (inp=="Y" || inp=="y") {

          # change settings
          cat("\n1) alim",
              "\n2) threshold.range",
              "\n3) eccentricity",
              "\n4) Max",
              "\n5) Min",
              "\n6) brain.threshold",
              "\n7) resize",
              "\n8) blur",
              "\n9) downsample")

          num_done <- FALSE
          while (!num_done) {
            pts <- readline("Enter the number(s) of the setting(s) you want to change: ")
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
              pts <- c(colvec, pts_sing)
              suppressWarnings(
                if(sum(is.na(pts)) == 0 & flag == FALSE){
                  if (sum(pts > 9 | pts < 1) < 1) {
                    pts <- sort(pts)
                    num_done <- TRUE
                  }
                })
            } else {
              pts <- suppressWarnings(as.integer(pts_sing))
              suppressWarnings(
                if(!sum(is.na(pts))){
                  if (sum(pts > 9 | pts < 1) < 1) {
                    pts <- sort(pts)
                    num_done <- TRUE
                  }
                })
            }
          }

          # Loop through and ask for settings
          for (p in pts ) {
            if ( p == 1 | p == 2) {
              filter[[p]] <- readline(user_prompt[[p]])
              filter[[p]] <- sort(as.numeric(unlist(strsplit(filter[[p]], ","))))
            } else if (p == 7 | p == 9 ) {
              filter[[p]] <- as.numeric(readline(user_prompt[[p]]))
            } else {
              filter[[p]] <- as.integer(readline(user_prompt[[p]]))
            }
          }
        } else if ( inp=="N" || inp == "n") {
          # exit out of loop
          change_done  <- TRUE
        }
      }
    }
#________console interface end ________

    # restart loop option if filter has changed
    if (!isTRUE(all.equal(old_filter, filter))){
      end <- FALSE
      while (!end ) {
        inp <- readline("Do you want to restart the loop? Y/N")
        if (inp == "y" | inp == "Y") {
          s <- 1
          end <- TRUE
        } else if (inp == "n" | inp == "N") {
          end <- TRUE
        }
      }
    } else {
      s <- s + 1
    }
  }
#______________Loop end_______________
  return(filter)
}
