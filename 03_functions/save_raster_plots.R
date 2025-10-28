#' Save monthly raster prediction plots
#'
#' This function generates and saves two sets of plots from a raster stack or brick 
#' with monthly prediction layers (named `Jan`–`Dec`):
#' 
#' 1. A faceted `ggplot2` raster plot with a common color scale across all months.
#' 2. A `terra::plot()` raster image with separate color scales for each month.
#'
#' @param raster_obj A `SpatRaster` (from the **terra** package) or similar object 
#'   containing 12 monthly prediction layers named `Jan` through `Dec`.
#' @param model_name Character string giving the model name, used in plot titles and filenames.
#' @param dir Output directory where plots will be saved. Defaults to `pred_gam_dir`.
#'
#' @return Invisibly returns `NULL`. Side effect: saves two PNG files to `dir`.
#'
#' @details 
#' - The ggplot image (`*_prediction_ggplot.png`) uses a pseudo-log color scale 
#'   (`scales::pseudo_log_trans(base = 10)`).
#' - The terra plot (`*_prediction_terraplot.png`) uses `hcl.colors(100, "Viridis")`.
#'
#' @examples
#' \dontrun{
#' save_monthly_raster_plots(sst_rast, model_name = "my_model")
#' }
#'
#' @export

save_monthly_raster_plots <- function(raster_obj, model_name, dir = pred_gam_dir) {

    # File 1: Faceted plot with ggplpot2 with a color scale across all months

    df <- 
        raster_obj |>
        as.data.frame(xy = TRUE) |>
        tidyr::pivot_longer(cols = Jan:Dec, names_to = "Month", values_to = "Prediction")

    # Order months correctly (instead of alphabetical)
    df$Month <- factor(df$Month, 
                            levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                    "Jul","Aug","Sep","Oct","Nov","Dec"))

    p <- ggplot(df, aes(x, y, fill = Prediction)) +
    geom_raster() +
    scale_fill_viridis_c(option = "H", trans = scales::pseudo_log_trans(base = 10)) +
    coord_equal() +
    facet_wrap(~Month, ncol = 4) +
    labs(title = "Predicted detection probability", subtitle = paste0("Model: ", model_name), x = "Longitude", y = "Latitude") +
    theme_bw()

    # File 2: `terra::plot()` image with a color scale per month
    ggsave(filename = file.path(dir, paste0(model_name, "prediction_ggplot.png")), p, width = 12, height = 10, dpi = 300)

    png(file.path(pred_gam_dir,paste0(model_name, "_prediction_terraplot.png")), width=1000, height=1200)

    terra::plot(raster_obj,
        main = 1:12,
        col = hcl.colors(100, "Viridis"), 
        axes = FALSE) 

    dev.off()

    return(p)
}
