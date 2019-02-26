#' @title Pull up atlas plate in graphics device.
#' @description Enter in an AP coordinate from the Allen Mouse Brain Atlas and
#'   pull up the atlas in a graphics device.
#' @param AP Anterior-posterior mouse atlas coordinate.
#' @param xpos x-position in pixels justified to the left of the main computer
#'   screen
#' @param x Coordinates where the text should be written. Argument from [text()]
#'   function.
#' @param adj one or two values in [0, 1] which specify the x (and optionally y)
#'   adjustment of the labels. On most devices values outside that interval will
#'   also work. Argument from [text()] function.
#' @param cex Character expension factor. Argument from [text()] function.
#' @export
pull_atlas <- function(AP, xpos = 0, x = 0, adj = c( -1, 0), cex = 1.5){
  quartz(title = paste0("Plate ", toString(platereturn(AP)),", AP ", toString(round(roundAP(AP), digits=2))), xpos = xpos)
  wholebrain::schematic.plot(dataset = NULL, mm.grid=F, coordinate = roundAP(AP), region.colors =  TRUE, device = F)
  text(x = x, paste0("Plate ", toString(platereturn(AP)),", AP ", toString(round(roundAP(AP), digits=2))), adj = adj, cex = cex)
}


