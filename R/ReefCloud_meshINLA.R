
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
ReefCloud_meshINLA <- function(data.sub, tier5.sf, tier5_buffer.list, FOCAL_TIER) {
  list2env(tier5_buffer.list, envir = environment())

  ReefCloud_tryCatch({
      cli::cli_progress_bar("SPDE INLA cellmeans", type = "iterator", total = 10, clear = TRUE)
      ##full.grid, full.coords
      full.grid.list <- reefCloudPackage::ReefCloud_meshINLA_full_grid(data.sub, tier5_buffer.sf)
      cli::cli_progress_update(force=TRUE)

      ##data.grid, coords - NOTE - DATA ASSIGNED LAT/LONG OF TIER5 GRID
      obs.grid.list <- reefCloudPackage::ReefCloud_meshINLA_obs_grid(data.sub, tier5.sf, tier5_buffer.sf)
      cli::cli_progress_update(force=TRUE)

      ## Newdata is just the observed locations so that this can be compared to simpler models
      newdata.grid.list <- reefCloudPackage::ReefCloud_meshINLA_newdata_grid(data.sub, FOCAL_TIER)
      cli::cli_progress_update(force=TRUE)

      mesh <- reefCloudPackage::ReefCloud_meshINLA_mesh(full.grid.list, tier5_buffer.sf)
      cli::cli_progress_update(force=TRUE)

      ##i.spatial, stack.est
      spatial_and_stack.list <- reefCloudPackage::ReefCloud_meshINLA_SPDEandStacks(mesh, full.grid.list, obs.grid.list, year = 'rw1')
      cli::cli_progress_update(force=TRUE)

      form <- reefCloudPackage::ReefCloud_meshINLA_formula(obs.grid.list, spatial_and_stack.list, year='rw1')
      cli::cli_progress_update(force=TRUE)

      reefCloudPackage::ReefCloud_meshINLA_fit(form, spatial_and_stack.list)
      cli::cli_progress_update(force=TRUE)

      cellmeans.full <- reefCloudPackage::ReefCloud_meshINLA_cellmeans(mesh, newdata.grid.list,
                                                        spatial_and_stack.list)
      cli::cli_progress_update(force=TRUE)

      reefCloudPackage::ReefCloud_meshINLA_tier5TemporalFocal(data.sub, cellmeans.full, FOCAL_TIER)
      cli::cli_progress_update(force=TRUE)

      cellmeans.tier5.sf <- reefCloudPackage::ReefCloud_meshINLA_tier5Spatiotemporal(tier5.sf, cellmeans.full)
      cli::cli_progress_update(force=TRUE)

  }, logFile=LOG_FILE, Category='--Modelling fitting routines--',
  msg=paste0('SPDE INLA cell means for ', stringr::str_to_title(GROUP), ' Tier ', TIER), return=NULL)
}
