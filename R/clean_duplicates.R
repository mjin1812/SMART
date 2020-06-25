#'@title Duplicate cell cleanup **(W)**
#'@description Duplicate cell count clean up of segmentation output from the
#'  [seg_loop()] function. For use with mapping whole brain only.
#'  Note: if the imaging dataset is large this will be a time intensive process.
#'  Processing time will be printed once the function is finished.
#'@param setup (required) Contains setup parameters.
#'@param segs (required) Segmentation data.
#'@param xy_thresh (optional, default = 1) Threshold in the xy planes for the
#'  minimum distance where two center points can be the same cell.
#'@param z_thresh (optional, default = 10) Threshold (in um) in the z-plane for
#'  the maximum z distance before a cell is counted as another cell.
#'@param compare_depth (optional, default = 200) Comparision depth in (um).
#'  Compare a single image with adjacent images up to 200 um posterior.
#'@return Returns the segmentation data in *segs* with clean cell output. All
#'  duplicate counts have been assigned NA.
#'@export
#'@md

clean_duplicates <- function(setup, segs, xy_thresh = 1, z_thresh = 10, compare_depth  = 200){

 # Set up clean up parameters
  tictoc::tic()
  sep_z   <- floor(z_thresh/(1000*setup$z_space*setup$seg_step))
  n_cols  <- floor(compare_depth/(setup$z_space*1000*setup$seg_step))
    for (s in 1:(length(segs$seg_z)-1)){

    # For the end of the image stack
    if (length(segs$seg_z)-s < n_cols) {
      n_cols <- length(segs$seg_z)-s
    }

    # Data arranged in matrix where N rows are total number of cells detects
    # in base slice. All other z-images will be compared against these cells.
    # N cols are number of z-slices being compared to these cells.
    n_rows <- length(segs$segmentations[[s]]$soma$x)

    # # Initialization & parameters
    # # to store overlapping cell counts
    # exact_overlap  <- 0
    # thresh_overlap <- 0

    # comp_matrix (Comparison matrix) gives corresponding indices of cells
    # that have matched in x,y coordinates on subsequent z-planes.
    comp_matrix <- matrix(data = NA, nrow = n_rows, ncol= n_cols+1)

    col_1 <- 1:n_rows
    col_1[is.na(segs$segmentations[[s]]$soma$x)]<-NA
    comp_matrix[,1] <- col_1

    # exact_overlap_cnt  <- 0
    # thresh_overlap_cnt <- 0

    # Search and identify cells that have the same x,y coordinate
    for (r in 1:n_rows){       # r for row
      for (c in 1:n_cols) {    # c for column
        if (!is.na(segs$segmentations[[s]]$soma$x[r])) {
          ind_x <- which.min(abs(segs$segmentations[[s+c]]$soma$x- segs$segmentations[[s]]$soma$x[r]))
          ind_y <- which.min(abs(segs$segmentations[[s+c]]$soma$y- segs$segmentations[[s]]$soma$y[r]))
          if((length(ind_x) != 0) & (length(ind_y) != 0)){
            if (ind_x==ind_y){
              comp_matrix[r,c+1] <- ind_x
              # exact_overlap_cnt <- exact_overlap_cnt + 1

            } else {
              # in cells isolated by x, threshold by difference in y
              diff_y <- abs(segs$segmentations[[s+c]]$soma$y[ind_x] - segs$segmentations[[s]]$soma$y[r])

              # in cells isolated by y, threshold by difference in x
              diff_x <- abs(segs$segmentations[[s+c]]$soma$x[ind_y] - segs$segmentations[[s]]$soma$x[r])

              if (diff_y < diff_x & diff_y < xy_thresh) {
                comp_matrix[r,c+1] <- ind_x
                # thresh_overlap_cnt <- thresh_overlap_cnt + 1

              } else if (diff_x < diff_y & diff_x < xy_thresh) {
                comp_matrix[r,c+1] <- ind_y
                # thresh_overlap_cnt <- thresh_overlap_cnt + 1
              }
            }
          }
        }
      }
    }

    # sep_z   <- z_thresh/(1000*setup$z_space)
    for (r in 1:n_rows) {

      c       <- 1   # counter for columns
      sep_cnt <- 1   # counter for consecutive NAs in a row


      ### start if else statement for last 5 columns here
      while (sep_cnt< sep_z &&  c != n_cols+1) {
        prev <- is.na(comp_matrix[r,c])
        cur  <- is.na(comp_matrix[r,c+1])
        if (cur) {
          if(prev) {
            sep_cnt <- sep_cnt+1
          } else {
            sep_cnt <- 1
          }
        }
        c <- c+1
      }

      if (sep_cnt < sep_z ){
        end_soma <- c
      } else {
        end_soma <- c-sep_z
      }

      # Store intensities up to end soma in vector and compare them
      if (end_soma != 0) {
        intensities <- vector(length=end_soma)

        for (c in 1:end_soma) {
          if (!is.na(comp_matrix[r,c])) {
            intensities[c] <- segs$segmentations[[s+c-1]]$soma$intensity[comp_matrix[r,c]]
          } else {
            intensities[c] <- NA
          }
        }

        center <- which.max(intensities)

        if (!length(center)==0){

          # Keep only the cell with the highest intensity
          for (c in 1:end_soma) {
           if (c!=center) {
              segs$segmentations[[s+c-1]]$soma$x[comp_matrix[r,c]]         <- NA
              segs$segmentations[[s+c-1]]$soma$y[comp_matrix[r,c]]         <- NA
              segs$segmentations[[s+c-1]]$soma$intensity[comp_matrix[r,c]] <- NA
              segs$segmentations[[s+c-1]]$soma$area[comp_matrix[r,c]]      <- NA
            }
          }
        }
      }
    }
  }
  return(segs)
  # Return processing time
  time <- tictoc::toc(quiet = TRUE)
  cat("\n", toString(round((time$toc - time$tic)/60, digits = 2)), "min elapsed")
}


