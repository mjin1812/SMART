#' @title Create a dataframe of hierarchical region cell count data.
#' @description Generates a dataframe of showing nested relationship of brain
#'   regions, their cell counts, and their cell count densities. Output can also
#'   be used for simple plots showing nested heirarchical relationships, such as
#'   treemaps. As a convention, higher level columns will indicate child regions
#'   while lower
#'   level columns indicate parent regions.
#' @param dataset (required) Whole brain dataset returned as an output of the
#'   [forward_warp()] function.
#' @param rois (optional, default = c("CH", "BS")) The acronyms of the regions
#'   of interest. Acronyms must follow  Allen Brain Atlas abbreviation
#'   standards.
#' @param base (optional, default = "grey") The base parent region you want
#'   subregions traced back to. This will be stored in the first hierarchy level
#'   (col 1) of the dataframe. Note, base region MUST be a lower level parent of
#'   all rois provided to the *rois* argument.
#' @param subtract (optional, default = TRUE) Subtract the cell counts of all
#'   child regions from parent regions. All counts number then uniquely belong
#'   to a single acronym.
#' @return Returns *tree*, a dataframe storing the hierarchical relationship
#'   between subregions. Cell counts, region colors, and cell count density is
#'   also stored. Descriptions of columns of the dataframe are below:
#'
#'   + Col 1-10. Each higher level column to the right is a sublevel of parent
#'   column to the left.
#'   + Col 11. Cell counts of regions
#'   + Col 12. ABA color
#' @export
#' @md

# Function to store hierarchical data in a dataframe,
# rois = regions of interest to find the subregions of
# base = parent region to store in the first column of the dataframe


get_tree <- function(dataset, rois = c("CH", "BS"), base =  "grey", subtract = TRUE){
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

  # After checking the nested structure of the ontology
  # there shouldn't be more than 10 hierarchy levels from "grey"
  tree <- data.frame(rep(NA, times = length(rois)) , #1
                     rep(NA, times = length(rois)) , #2
                     rep(NA, times = length(rois)) , #3
                     rep(NA, times = length(rois)) , #4
                     rep(NA, times = length(rois)) , #5
                     rep(NA, times = length(rois)) , #6
                     rep(NA, times = length(rois)) , #7
                     rep(NA, times = length(rois)) , #8
                     rep(NA, times = length(rois)) , #9
                     rep(NA, times = length(rois)) , #10
                     rep(NA, times = length(rois)) , #11 Store cell counts
                     # rep(NA, times = length(rois)) , #12 Store region density
                     rep(NA, times = length(rois)))  #13 Store region color
    colnames(tree) <- c(paste0("L", 1:10), "counts", "color")
  # colnames(tree) <- c(paste0("L", 1:10), "counts", "density", "color")

  # Initialize cell countss
  counts <- c()

  # Loop through regions
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
    }

    # Get current cell counts
    cur_cnt <- get_count(dataset, roi = rois[c])

    # Subtract cell counts from parent regions
    if (isTRUE(subtract)) {
      # subtract cell counts of child regions
      child_rois <- wholebrain::get.acronym.child(rois[c])

      if (!anyNA(child_rois)) {
        child_cnt <- get_count(dataset, roi = child_rois)
        cnt <- cur_cnt$cell.count - sum(child_cnt$cell.count)
      } else {
        cnt <- cur_cnt$cell.count
      }
    } else if (isFALSE(subtract)) {
      # DONT subtract cell counts of child regions
        cnt <- cur_cnt$cell.count
    }

    # Add to cell counts list
    counts <- c(counts, cnt)
    for (l in 1:length(path)){
      tree[c,l] <- path[l]
    }
  }

  # Assign cell countss to the tree
  tree$counts <- counts

  # Get current color
  tree$color <-  wholebrain::color.from.acronym(rois)

  # # Get region density counts
  # densities <- wholebrain::normalize.volume(dataset)
  # densities <- densities[rois]
  # tree$density <- densities

  # Return allows the user to assign to a variable of their choosing
  return(tree)
}



