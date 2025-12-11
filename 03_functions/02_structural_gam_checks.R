check_gam_structure <- function(model) {
  
  message("🔹 Checking Effective Degrees of Freedom (EFD) and concurvity")
  # EDF and smooth names
  sm_summ <- summary(model)
  edf_vals <- sm_summ$edf
  smooth_names <- names(edf_vals)
  high_edf <- which(edf_vals > 8)

  # High EDF warning
  if (length(high_edf) > 0) {
    warning("⚠️ Some smooths have very high EDF (>8): ",
            paste(smooth_names[high_edf], collapse = ", "), "\n",
            "   → May indicate overfitting. Consider shrinkage smooths or reducing k.")
  } else {
    message("✓ EDF values acceptable for all smooths.")
  }

  # Concurvity
  cc <- mgcv::concurvity(model, full = TRUE)
  est <- cc["estimate", ] |> as.numeric()
  high_concur <- which(est > 0.8)
  if (length(high_concur) > 0) {
    warning("⚠️ High concurvity (>0.8) detected for: ",
            paste(colnames(cc)[high_concur], collapse = ", "), "\n",
            "   → Consider removing/merging correlated predictors or using tensor smooths.")
  } else {
    message("✓ No major concurvity issues detected.")
  }

  # TODO: maybe save as .csv in model folder
  invisible(list(
    summary = sm_summ,
    edf = edf_vals,
    concurvity = cc,
    high_concurv_smooths = colnames(cc)[high_concur]
  ))
}
