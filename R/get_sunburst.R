#' @title Generate a sunburst plot using a forward warped dataset.
#' @param dataset (required) Whole brain dataset returned as an output of the
#'   [forward_warp()] function.
#' @param rois (optional, default = c("CH", "BS")) The acronyms of the regions
#'   of interest. Acronyms must follow  Allen Brain Atlas abbreviation
#'   standards.
#' @param parent (optional, default = TRUE) TRUE allows multiple rois to be
#'   plotted on the same sunburst, with the base parent layer set to 'grey'. If
#'   FALSE, the base will be set to the first ROI. Other ROIs will be ignored
#'   because the sunburst cannot accomodate more than one base layer.
#' @return Returns an object of class sunburst.
#' @export
#' @md

get_sunburst <- function(dataset, rois = c("CH", "BS"), parent = TRUE){

 if (isTRUE(parent)) {
   # If parent is true, the base will be set to "grey"
   tree <- get_tree(dataset, rois = rois, base = "grey")

 } else if (isFALSE(parent)) {
   # If parent is FALSE, the base will be set to the first ROI.
   tree <- get_tree(dataset, rois = rois[1],
                    base = rois[1])
 }

 # Convert tree to a JSON
 d3_data <- d3r::d3_nest(tree, value_cols = c("counts", "color"))
 # d3_data <- d3r::d3_nest(tree, value_cols = c("counts", "density" ,"color"))

 # Generate sunburst
 sb_obj <- sunburstR::sunburst(data = d3_data, valueField = "counts", count = TRUE, legend = FALSE,
                               colors = htmlwidgets::JS("function(d){return d3.select(this).datum().data.color;}"), withD3 = TRUE)
 return(sb_obj)
}
