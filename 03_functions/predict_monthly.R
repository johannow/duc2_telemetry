#' Predict Monthly Presence Probability with a GAM Model
#'
#' Uses a fitted GAM workflow to make monthly raster predictions
#' on environmental layers. Predictions can be restricted to inside or outside
#' offshore wind farm (OWF) areas by masking on the `min_dist_owf` raster.
#'
#' @param model A bundled tidymodels GAM model object (output of `train_gam_model()`).
#' @param predictors A named list of SpatRasters with predictors. 
#'   Must include:
#'   \describe{
#'     \item{habitat}{Categorical habitat raster (already cleaned/resampled).}
#'     \item{bathy}{Bathymetry raster (numeric).}
#'     \item{sst}{SpatRaster with 12 monthly SST layers.}
#'     \item{lod}{SpatRaster with 12 monthly LOD layers.}
#'     \item{OWF_dist}{Raster of minimum distance to OWF (0 = inside).}
#'     \item{shipwreck_dist}{Raster of minimum distance to shipwrecks.}
#'   }
#'   The rasters were created with file `05_create_env_grids_to_predict-on.R`,
#'   and should be in the folder "~./01_data/02_processed_data".
#'
#' @param inside_OWF Logical or NA. If TRUE, restrict predictions to OWF cells;
#'   if FALSE, restrict to outside OWF; if NA, predict everywhere.
#' @param owf_layer SpatRaster with OWF distance layer.
#' @param predprob Function passed to `terra::predict()` to extract predicted
#'   probabilities. Defaults to `function(...) predict(..., type = "prob")$.pred_1`.
#'
#' @return A SpatRaster with 12 prediction layers (one per month).
#' @export
#'
#' @examples
#' \dontrun{
#' preds_inside <- predict_monthly(
#'   model = model_inside_owf,
#'   predictors = list(
#'     habitat = habitats_clean,
#'     bathy = bathy,
#'     sst = sst_avg_month,
#'     lod = lod_avg_month,
#'     OWF_dist = OWF_dist,
#'     shipwreck_dist = shipwreck_dist
#'   ),
#'   inside_OWF = TRUE
#' )
#' terra::plot(preds_inside)
#' }
#' 
# Tests
# model <- model_inside_owf
# predictors <- c(habitats_clean, bathy, sst_avg_month, lod_avg_month, OWF_dist, shipwreck_dist)

predict_monthly <- function(model,
                            predictors = c(habitats_clean, bathy, sst_avg_month, lod_avg_month, OWF_dist, shipwreck_dist),
                            inside_OWF = TRUE,
                            owf_layer = OWF_dist,
                            predprob = function(...) predict(..., type = "prob")$.pred_1) {
                            
  # model_backup <- model
  # unwrap the fitted mgcv model
  model <- bundle::unbundle(model)$fit$fit

## model # inspect
# model_backup |> class()
# model |> class()

# determine the predictor names from the model
predictor_names <-
  (model |> 
    parsnip::extract_fit_engine() |>
    formula() |>
    all.vars())[-1] # deselect the predictor variable

# predictor_names # inspect

names(predictors)

#   # ensure required predictors exist
#   required <- c("habitat", "bathy", "sst", "lod", "OWF_dist", "shipwreck_dist")
#   missing <- setdiff(required, names(predictors))
#   if (length(missing) > 0) {
#     stop("Missing predictors in list: ", paste(missing, collapse = ", "))
#   }

  predictions <- list()
# i <- 3

  for (i in 1:12) {

    # Step 1: figure out which predictors to keep
    layers_to_use <- names(predictors) |>
    # keep only those that match the model's predictor names
    keep(function(nm) {
        any(str_detect(nm, paste0("^", predictor_names, collapse = "|")))
    })
    # layers_to_use #inspect

    # Step 2: if a predictor is time-varying, select only layer for current month
    layers_to_use2 <- purrr::map_chr(predictor_names, function(pn) {
        # find all matching layers for this predictor
        matches <- grep(pn, names(predictors), value = TRUE)
        if (length(matches) > 1) {
            # assume monthly layers are suffixed with "_month_X"
            month_match <- matches[str_detect(matches, paste0("_", i, "$"))]
            if (length(month_match) == 1) return(month_match)
            else stop("Could not uniquely match month ", i, " for predictor ", pn)
        } else {
            return(matches)
        }
    })
    # layers_to_use2 # inspect

    # Step 3: subset predictors
    pred_stack <- predictors[[layers_to_use2]] # <- that is already the pred_stack of predictors

    # Step 4: rename to match predictor_names (order preserved)
    names(pred_stack) <- predictor_names

    # pred_stack # inspect
    # Step 5: mask based on inside_OWF
    # get OWF layer --> rewrite such that the OWF layer does not have to be in the model (and therefore not in the predictor list)
    # owf_layer <- predictors[[grep("owf", names(predictors), ignore.case = TRUE)]] # match 'OWF' to predictor names
    # owf_layer # inspect
   
   # mask based on inside/outside OWF
    if (is.na(inside_OWF)) {
    masked <- pred_stack
    } else if (inside_OWF) {
    owf_mask <- terra::ifel(owf_layer == 0, 1, NA)
    masked   <- pred_stack * owf_mask
    } else {
    owf_mask <- terra::ifel(owf_layer != 0, 1, NA)
    masked   <- pred_stack * owf_mask
    }

    # terra::plot(masked) # inspect  

    # run prediction
    predictions[[i]] <- terra::predict(masked, model = model, fun = predprob)
  }

  # combine to raster pred_stack with month names + time stamps
  predictions <- terra::rast(predictions)
#   terra::plot(predictions) # inspect

  names(predictions) <- lubridate::month(1:12, label = TRUE) # Not sure if this is the best method bc we only have months and not years
#   terra::time(predictions) <- as.POSIXct(seq(
#     lubridate::ymd("2022-01-01"),
#     by = "month",
#     length.out = 12
#   ))

  return(predictions)
#   terra::plot(predictions) # inspect
}
