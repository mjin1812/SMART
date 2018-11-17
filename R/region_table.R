#' @title Plot a region table.
#' @description Plot a region table based on cell count density of the
#' data.frame information assigned to variable [dataset].
#' @param
#' @param
## @export
#' @md
#'
#'



region_table <- function(dataset, savepaths, saveplot = TRUE,
                            region.lab = "Input region:", xlab = "Cell count") {


  wholebrain::dot.plot(dataset, device = TRUE,region.lab = region.lab, xlab = xlab)


  # # To be added in another function
  # # Save a region table
  # if (savetable){
  #   dot.plot(dataset = data)
  #   path <- paste0(savepaths$out_segmentation_tables, data"/table_z_", toString(segs$seg_z[n]),
  #                  "_AP_", toString(round(regi$coordinate, digits = 2)), "_plate_",
  #                  platereturn(round(regi$coordinate, digits = 2)), ".", filetype)
  #   savePlot(filename = path, type = filetype)
  #   dev.off()
  # }

}
