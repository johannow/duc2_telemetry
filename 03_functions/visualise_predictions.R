#' Aggregate a SpatRaster over time and save outputs
#'
#' Aggregates a time-enabled terra SpatRaster using terra::tapp(),
#' saves a PNG plot of the aggregated raster,
#' and writes the aggregated raster to NetCDF.
#'
#' @param raster_obj A terra SpatRaster.
#' @param aggregate boolean. If TRUE, then raster_obj will get aggregated. It should then have a valid time dimension.
#' @param fun Character string giving the aggregation function passed to
#'   terra::tapp() (e.g. "mean", "median", "sum").
#' @param index Character string defining the temporal aggregation unit
#'   (e.g. "months", "years") or a vector compatible with terra::tapp().
#' @param varname Character. variable written into the spatraster that is saved.
#' @param dir Character. Directory where output files will be written.
#' @param model_info Character. Text describing the model; added as a title
#'   annotation to the output plot.
#' @param range c(max,min). Range that raster is plotted.
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
                                  aggregate = TRUE,
                                  fun = "median",
                                  index = "months",
                                  varname = "",
                                  dir = aggregations_dir,
                                  model_info = "",
                                  filename = NULL,
                                  range = NULL,
                                  plot_type = NULL) {

   # 1. aggregate 
  if(aggregate){
  raster <- tapp(
    raster_obj,
    index = index,
    fun = fun,
    na.rm = TRUE
  )}else{raster <- raster_obj}
  
  #set variable name
  varnames(raster) <- varname
  
  # if filename is explicitly stated
  if(is.null(filename)){
    filename <- deparse(substitute(raster_obj)) # set the name of the raster obj as filename
    }
  
  # make filepath
  if(aggregate){
    file_path <- paste0(dir, "/", filename %>% unique(),"_", fun,"_", index)
  }else{
    file_path <- paste0(dir, "/", filename %>% unique())
  }
  
  # 2. save plot
  png(paste0(file_path,".png"), width = 2000, height = 2000, res = 300)
  if(is.null(range)){
    plot(raster)
  }else{terra::plot(raster, range = range)}
  graphics::mtext(model_info,
        side = 3,      # top
        line = 3,
        cex  = 0.75)
  dev.off()
  
  # 3. save aggregated file
  if(aggregate){ #only save a .nc file if the raster obj was aggregated (otherwise the raster obj doesn't change)
  suppressWarnings(
  terra::writeCDF(x = raster,
                  filename = paste0(file_path,".nc"),
                  varname = varname,
                  overwrite = TRUE))}
  
  # return the aggregated obj
  invisible(raster)
  
}