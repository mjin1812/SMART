#' A function to sort image paths for large imaging datasets
#' @description User friendly way to sort images from the registration channel.
#'   Asks for user input to account for flexible naming conventions for images.
#' @param setup (required) Argument used to access registration and segmentation
#'   folders.
#' @param extension (optional, default = "tif") File extension of the imaging
#'   data.
#' @param separator (optional, default = "_") Separator character to parse
#'   filename.
#' @param position (optional) Position of z number in filename. If this argument
#'   is called, the user input is skipped.
#' @details Compared to the [list.files()] function, this function will sort 100+
#' files in their appropriate order by z plane.
#' @return *setup* is returned with *setup$image_paths* filled as a list of
#'   sorted paths. The first element is a sorted character vector of
#'   registration channel paths. The second element is a sorted character vector
#'   of segmentation channel paths.
#' @md
#' @export

im_sort <- function(setup, extension = "tif", separator = "_", position = NULL) {

  # Extracting registration image file path names
  regi_paths <- list.files(setup$regi_channel, pattern = extension)

  if (length(regi_paths)==0 ) {
    stop(paste0("No files of filetype ", extension))
  }

  if (is.null(position)) {
    cat("\nDuring sorting, your image naming convention will found and split based on the separator you entered (default = '_').",
        "\n\n For example, '1-1-2018_Mouse1_coronal_Z001_C01.tif' will be split to:",
        "\n1) '1-1-2018'",
        "\n2) 'Mouse1'",
        "\n3) 'coronal'",
        "\n4) 'z001'",
        "\n5) 'C01.tif'",
        "\n\nIn your naming convention, what name position is your z number if the file name is split by your separator?",
        "\n\n For example, 'z001' is in position 4 of the file name above.")

    # Example file path
    cat(paste0("\n\nYour first image is named '", regi_paths[1], "'",
               "\nIt was split to: "))

    slice <- unlist(strsplit(regi_paths[1], separator))
    for (s in 1:length(slice)) {
      cat("\n", paste0(toString(s), ") ", slice[s]))
    }

    done <- FALSE
    while(!done ) {
      suppressWarnings(position <- as.integer(readline("What name position is your z number (integer only): " )))
      if (!is.na(position)){
        done <- TRUE
      }
    }
  } else if (is.na(as.integer(position))) {
    stop("Not a valid argument for 'position'")
  }

  #### Registration channel images
  im_length <- length(regi_paths)
  slice_in  <- vector(mode= "numeric", length = im_length)

  ## Finds and accounts for any characters like "z" before the z number
  z_pos <- which(!is.na(as.integer(unlist(strsplit(unlist(strsplit(regi_paths[1], separator))[position], "\\D+")))))[1]

  ## sorting image paths
  for (s in 1:im_length) {
    slice       <- unlist(strsplit(regi_paths[s], separator))
    slice_in[s] <- slice[position]
    slice_in[s] <- strsplit(slice_in[s], "\\D+")[[1]][z_pos]
  }

  slice_in      <- as.numeric(slice_in)
  test          <- sort.int(slice_in, index.return=TRUE)   # Sorting slice number
  regi_paths    <- file.path(setup$regi_channel, regi_paths[test$ix])

  #### Segmentation channel images

  seg_paths <- list.files(setup$seg_channel , pattern = extension)
  slice_in  <- vector(mode= "numeric", length = im_length)

  ## sorting image paths
  for (s in 1:im_length) {
    slice       <- unlist(strsplit(seg_paths[s], separator))
    slice_in[s] <- slice[position]
    slice_in[s] <- strsplit(slice_in[s], "\\D+")[[1]][z_pos]
  }

  slice_in      <- as.numeric(slice_in)
  test          <- sort.int(slice_in, index.return=TRUE)   # Sorting slice number
  seg_paths     <- file.path(setup$seg_channel, seg_paths[test$ix])

  #### Return a list of sorted image path files
  setup$image_paths <- list(regi_paths = regi_paths,
                           seg_paths  = seg_paths )
  return(setup)
}
