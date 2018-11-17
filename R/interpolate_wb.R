## interpolate_wb()
## Interpolate all the AP and z numbers corresponding to atlas plates in between the first and last image. For use with mapping whole brain only (W).
## 9-24-2018 MJ
##
## Arguments:
## setup (required)
## method (optional) - interpolation method;  (default = "l")
##                     Options: "l" (linear interpolation), "s" (monotonic spline interpolation)
## Returns:
## setup - returns interpolated AP and z numbers based on the reference AP and z planes (element 16 and 17 of the setup list)


#' @title Interpolate all AP and z numbers for atlas plates in a whole brain project.
#' @description This function interpolates all corresponding z and AP values for atlas plates that are not reference plates.
#' For use with mapping whole brain only.
#' @param setup (required) Setup list will be used as the return variable.
#' @param method (optional, default = "l") Interpolation method.
#' Options: "l" (linear interpolation), "s" (monotonic spline interpolation)
#' @export

interpolate_wb <- function(setup, method = "l") {

  # ###### Get necessary functions NOTE: these will be modified to be internal package functions later
  # source('wb_functions.R')
  # ######

  # Combine first, last, and internals
  AP <- roundAP(c(setup$first_AP, setup$internal_ref_AP, setup$last_AP))
  z  <- c(setup$first_z, setup$internal_ref_z, setup$last_z)

  # Get AP coordinate values needed to interpolate
  # Then remove AP values that are already matched
  AP_interp <- roundAP(seq(setup$first_AP, setup$last_AP , by = -setup$regi_step))
  AP_interp <- AP_interp[!(AP_interp %in% AP)]

  if(method == "l" ) {
    # Get linear interpolation function
    my_function <- approxfun(AP, z, method = "linear")
    z_interp    <- round(my_function(AP_interp))
  } else if (method == "s") {
    my_function <- splinefun(AP, z, method="monoH.FC")
    z_interp    <- round(my_function(AP_interp))
  } else {
    stop("Not a valid argument for 'method'")
  }

  # Store interpolated values into the setup list
  setup$regi_AP <- sort(c(AP_interp, setup$internal_ref_AP), decreasing = TRUE)
  setup$regi_z <- sort(c(z_interp, setup$internal_ref_z))
  return(setup)
}
