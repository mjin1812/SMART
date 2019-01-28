#' @title Pull up atlas plate in graphics device.
#' @description Enter in an AP coordinate from the Allen Mouse Brain Atlas and
#'   pull up the atlas in a graphics device.
#' @param AP Anterior-posterior mouse atlas coordinate.
#' @export
pull_atlas <- function(AP, font_size = 400, font_location = "+250+150"){
  im_path <- file.path(path.package("SMART", quiet = TRUE), "extdata/atlas", paste0(AMBA_return(AP), "_3.svg"))
  if (!file.exists(im_path)){
    stop("This plate doesn't currently exist. You may be too far anterior or too far posterior. Try again!")
  }
  image <- magick::image_read(im_path)
  image <- magick::image_annotate(image, paste0("Plate ", toString(platereturn(AP)),", AP ", toString(round(roundAP(AP), digits=2))),
                                  gravity = "southwest", size= font_size, color = "black", location =  font_location)
  quartz(title= paste0("Plate ", toString(platereturn(AP)),", AP ", toString(round(AP, digits=2))), xpos = 0)
  plot(image)
}
