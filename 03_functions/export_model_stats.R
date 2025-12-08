

export_model_stats <- function(
  model_obj,
  cv_stats = FALSE,
  model_name = NULL, #if cv stats != NULL then model name has to be != NULL
  dir = mod_dir
) {

  #tmp
  # model_obj <- m1_io
  # bundled_model_obj <- readRDS(file.path(mod_gam_dir, "inside_owf_slodk10+habitat_gam.rds"))


  # --- 1. Check if model is bundled, unbundle if necessary ---
  if (inherits(model_obj, "bundled_workflow") || inherits(model_obj, "bundled_model")) {
    message("ℹ Detected bundled model, unbundling...")
    model_unbundled <- bundle::unbundle(model_obj)$fit$fit
  } else {
    message("ℹ Model is already unbundled.")
    model_unbundled <- model_obj
  }
  
  # --- 2. Extract GAM engine (mgcv::gam object) ---
  gam_obj <- tryCatch(
    workflows::extract_fit_engine(model_unbundled),
    error = function(e) {
      message("⚠️ Could not extract GAM engine — assuming this is already a mgcv::gam object.")
      return(model_unbundled)
    }
  )


summary_gam <- summary(gam_obj)

# --- 3. Extract model formula ---
  model_formula <- tryCatch(
    as.character(formula(gam_obj)) |> paste(collapse = " "),
    error = function(e) NA_character_
  )

edf_tbl <- gratia::edf(gam_obj)

model_stats <- tibble::tibble(
  model_name = model_name,
  smooth = c(gratia::smooths(gam_obj), "model_overall"),
  edf = c(edf_tbl$.edf, sum(gam_obj$edf)),  # ✅ use .edf here
  aic = c(rep(NA_real_, nrow(edf_tbl)), AIC(gam_obj)),
  bic = c(rep(NA_real_, nrow(edf_tbl)), BIC(gam_obj)),
  deviance_explained = c(rep(NA_real_, nrow(edf_tbl)), summary_gam$dev.expl),
  r_sq = c(rep(NA_real_, nrow(edf_tbl)), summary_gam$r.sq),
  sp_mean = mean(gam_obj$sp, na.rm = TRUE),
  summary_text = c(
    rep(NA_character_, nrow(edf_tbl)),
    capture.output(summary(gam_obj)) |> paste(collapse = "\n")
  )
)
  
# # --- 6. Optional: CV metrics ---
# if (isTRUE(cv_metrics & !is.null(model_name))) {
#   cv_metrics <- read.csv(paste0(dir, "/", model_name, "_cv_metrics.csv"))
#   # best_params <- tune::show_best(tuned_obj, n = 1)
#   # cv_metrics <- tune::collect_metrics(tuned_obj) |>
#   #   dplyr::filter(.config == best_params$.config)
  
#   overall_tbl <- overall_tbl |>
#     dplyr::mutate(
#       cv_roc_auc  = cv_metrics |> dplyr::filter(.metric == "roc_auc")  |> dplyr::pull(mean),
#       cv_tss      = cv_metrics |> dplyr::filter(.metric == "tss")      |> dplyr::pull(mean),
#       cv_pr_auc   = cv_metrics |> dplyr::filter(.metric == "pr_auc")   |> dplyr::pull(mean),
#       cv_sens     = cv_metrics |> dplyr::filter(.metric == "sens")     |> dplyr::pull(mean),
#       cv_accuracy = cv_metrics |> dplyr::filter(.metric == "accuracy") |> dplyr::pull(mean)
#     )
# }

  
# --- 8. Write or append results ---
results_path <- paste0(dir, "/", model_name, "_model_stats.csv")
if (file.exists(results_path)) {
  readr::write_csv(model_stats, results_path, append = TRUE)
} else {
  readr::write_csv(model_stats, results_path)
}
  
message("✅ Model statistics exported to: ", results_path)
return(model_stats)
}
