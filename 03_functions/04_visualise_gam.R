#' Visualise a GAM using gratia (with guidance messages)
#'
#' @param model A fitted mgcv GAM object.
#' @param ... Additional arguments for gratia::draw()
#'
visualise_gam <- function(model, ...) {
  
  if (!inherits(model, "gam")) {
    stop("`model` must be a mgcv GAM object.")
  }
  
  message("🎨 Starting visual diagnostics with gratia...")
  
  # --- gratia::appraise ---
  message("\n📊 Plotting appraise() diagnostics...")
  message("   • QQ-plot: large deviations from the line suggest poor residual distribution.")
  message("   • Residuals vs fitted: patterns imply nonlinearity or missing covariates.")
  message("   • Histogram: skewness or long tails may indicate overdispersion.")
  print(gratia::appraise(model))
  
  # --- smooths ---
  message("\n📈 Plotting smooth terms with draw()...")
  message("   • Check for overly wiggly smooths (high EDF).")
  message("   • Flat smooths suggest the covariate may have little effect.")
  print(gratia::draw(model, ...))
  
}
