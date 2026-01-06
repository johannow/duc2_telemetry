#' Aggregate a SpatRaster over time and save outputs
#'
#' Aggregates a time-enabled terra SpatRaster using terra::tapp(),
#' saves a PNG plot of the aggregated raster,
#' and writes the aggregated raster to NetCDF.
#'
#' @param raster_obj A terra SpatRaster with a valid time dimension.
#' @param fun Character string giving the aggregation function passed to
#'   terra::tapp() (e.g. "mean", "median", "sum").
#' @param index Character string defining the temporal aggregation unit
#'   (e.g. "months", "years") or a vector compatible with terra::tapp().
#' @param dir Character. Directory where output files will be written.
#' @param model_info Character. Text describing the model; added as a title
#'   annotation to the output plot.
#'
#' @details
#' The function performs three steps:
#' \enumerate{
#'   \item Temporal aggregation of the raster using terra::tapp().
#'   \item Saving a PNG visualization of the aggregated raster.
#'   \item Writing the aggregated raster to a NetCDF file.
#' }
#'
#' Output filenames are constructed from the raster variable name,
#' aggregation function, and index.
#'
#' @return A terra SpatRaster containing the aggregated layers.
#'
#' @examples
#' \dontrun{
#' monthly_med <- aggregate_save_raster(
#'   raster_obj = predictions_owf_dist,
#'   fun = "median",
#'   index = "months",
#'   dir = pred_dir,
#'   model_info = "XGBoost model – daily predictions"
#' )
#' }
#'
#' @export
#' 
aggregate_save_raster <- function(raster_obj,
                                  fun = "median",
                                  index = "months",
                                  dir = pred_dir,
                                  model_info) {

   # 1. aggregate 
  aggregated_raster <- tapp(
    raster_obj,
    index = index,
    fun = fun,
    na.rm = TRUE
  )
  
  # file_path <- paste0(dir, "/", varnames(raster_obj) %>% unique(),"_aggr_", fun,"_", index)
  file_path <- paste0(dir, "/", deparse(substitute(raster_obj)) %>% unique(),"_", fun,"_", index)
  
  # 2. save plot
  png(paste0(file_path,".png"), width = 2000, height = 2000, res = 300)
  plot(aggregated_raster)
  graphics::mtext(model_info,
        side = 3,      # top
        line = 3,
        cex  = 0.75)
  dev.off()
  
  # 3. save aggregated file
  suppressWarnings(
  terra::writeCDF(x = aggregated_raster,
                  filename = paste0(file_path,".nc"),
                  varname = "aggregated predicted count",
                  overwrite = TRUE))
  
  # return the aggregated obj
  return(aggregated_raster)
  
}