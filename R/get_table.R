#' @title Create a data frame of left/right counts and their ratio of regions of
#'   interest
#' @description Generates a dataframe showing region acronyms, their full name,
#'   hierachical paths, total cell counts, left and right counts, and cell count
#'   percentages.
#' @param dataset (required) Whole brain dataset returned as an output of the
#'   [forward_warp()] function.
#' @param rois (optional, default = c("CH", "BS")) The acronyms of the regions
#'   of interest. Acronyms must follow  Allen Brain Atlas abbreviation
#'   standards.
#' @param base (optional, default = "grey") The base parent region you want
#'   subregions traced back to. Note, base region MUST be a higher parent level
#'   or the same level of the roi/rois provided to the *rois* argument.
#' @return Returns *table*, a dataframe showing total, left and right cell
#'   counts and count percentages according region acryonym.
#' @examples
#' # Get cell counts of the hypothalamus
#' table <- get_table(dataset, rois = "HY", base = "HY")
#'
#' # Get cell counts of all grey matter
#' table <- get_table(dataset, rois = "grey", base = "grey")
#'
#' # Get cell counts of hypothalamus and all parent structures of the hypothalamus up to gray matter
#' table <- get_table(dataset, rois = "HY", base = "grey")
#' @export
#' @md

get_table <- function(dataset, rois = c("CH", "BS"), base =  "grey"){

  children <- get_all_children(rois)

  # Add necessary parent regions
  parents <- c()
  for (p in 1:length(rois)){
    if (rois[p] != base) {
      parent  <- wholebrain::get.acronym.parent(rois[p])
      while (parent != base) {
        parents <- c(parent, parents)
        parent  <- wholebrain::get.acronym.parent(parent)
      }
    }
  }
  rois <- unique(c(base, parents, rois, children))

  # Initialize paths
  paths <- c()

  # Initilize counts
  counts <- c()

  for (c in 1:length(rois)){
    if ( rois[c] == base ){
      path <- base
    } else {
      # Get tree path to current region
      parent <- wholebrain::get.acronym.parent(rois[c])
      path   <- c(parent, rois[c])
      while (parent != base){
        parent <- wholebrain::get.acronym.parent(parent)
        path <- c(parent, path)
      }

      path <- paste0(paste0(path[-length(path)], sep = '-', collapse = ""),
                     tail(path,1), collapse = "")
    }
    # Add to paths
    paths <- c(paths, path)

    # Get current cell counts
    cur_cnt <- get_count(dataset, roi = rois[c])
    counts <- c(counts, cur_cnt$cell.count)
  }


  # get vector of region names
  region_names <- wholebrain::name.from.acronym(rois)

  # get hemisphere counts

  # Left
  left_counts  <- get_hemi_cnts(dataset, roi = rois, hemi = "Left")
  left_counts  <- left_counts$cell.count
  # Right
  right_counts <- counts - left_counts
  # Ratio
  L_R_ratio    <- round((left_counts/(right_counts)), digits = 2)

  # calculate percentages
  percentages  <- c()
  max_counts   <- max(counts)
  for(i in 1:length(rois)){
    new_percentage <- counts[i]/max_counts
    percentages    <- c(percentages, new_percentage)
  }
  percentages <- paste0(round(percentages * 100, digits = 2), "%")

  data_table  <- data.frame("Region" = rois,
                           "Name" = region_names,
                           "Path" = paths,
                           "Count" = counts,
                           "Left" = left_counts,
                           "Right" = right_counts,
                           "Left_Right_Ratio" = L_R_ratio,
                           "Percentage" = percentages)

  return(data_table)
}


