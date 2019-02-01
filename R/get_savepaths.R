#' Automatically generates savepaths and save directories.
#' @description Create standardized save directories based on the setup
#'   information from [setup_pl()].
#' Returns a list of savepaths to subfolders in the output folder.
#' @param setup (required) The output folder from the setup list is used to
#'   generate save directories
#' @return *setup* is returned with *setup$savepaths* filled containing
#'   savepaths to the data directories where analysis output will be stored.
#' @details Data directories are automatically created in the output folder.
#' @md
#' @export

get_savepaths <- function (setup) {

  ## Creating data directories
  # Folder to store R data. Note: script will periodically save to this folder throughout analysis
  out_R_data <- file.path(setup$output, "R_data")
  envir_savepath <- paste0(out_R_data,"/","Animal_",toString(setup[[1]]),
                           "_", Sys.Date(),"_", setup[[2]], ".RData")
  # Folder for aligned reference images
  out_reference_aligned <- file.path(setup$output, "Reference_aligned")
  # Folder to store automated registration images
  out_auto_registration <- file.path(setup$output, "Registrations_auto")
  # Folder for manually registered references images
  out_registration <- file.path(setup$output, "Registrations_manual")
  # Folder for warped registration images (output folders created by 'registration' function in wholebrain)
  out_registration_warps <- file.path(setup$output, "Registration_warps")
  # Folder to store forward warp images
  out_segmentation_warps  <- paste0(setup$output, "/Segmentation_warpimages")
  # Folder to store schematic plots
  out_segmentation_schem  <- paste0(setup$output, "/Segmentation_schematics")
  # Folder for morphed brain image
  out_RC_brain_morph <- file.path(setup$output, "RC_brain_morph")

  if (!file.exists(out_R_data)){
    # Create directories
    dir.create(out_R_data)
    dir.create(out_auto_registration)
    dir.create(out_registration)
    dir.create(out_registration_warps)
    dir.create(out_segmentation_warps )
    dir.create(out_segmentation_schem)

    if (length(setup)>9) {
      # Create folder for brain morph if whole brain project
      dir.create(out_RC_brain_morph)
      dir.create(out_reference_aligned)
    }
  }

  # Return a list of the paths
  if (length(setup)>9) {
    ## Create list of savepaths
    setup$savepaths <-list(envir_savepath = envir_savepath,
                           out_reference_aligned = out_reference_aligned,
                           out_RC_brain_morph = out_RC_brain_morph,
                           out_auto_registration = out_auto_registration,
                           out_registration = out_registration,
                           out_registration_warps = out_registration_warps,
                           out_segmentation_warps = out_segmentation_warps,
                           out_segmentation_schem = out_segmentation_schem)

  } else {
    ## Create list of savepaths
    setup$savepaths <- list(envir_savepath = envir_savepath,
                            out_auto_registration = out_auto_registration,
                            out_registration = out_registration,
                            out_registration_warps = out_registration_warps,
                            out_segmentation_warps = out_segmentation_warps,
                            out_segmentation_schem = out_segmentation_schem)
  }
  return(setup)
}



