#' Fit a Generalized Additive Model (GAM) with Cross-Validation and Tuning
#'
#' This function fits a GAM using `mgcv` via the tidymodels and parsnip framework.
#' It performs data splitting, recipe preprocessing, cross-validation,
#' parameter tuning, and saves the final fitted model to disk.
#'
#' @param model_formula List. List containing one gam formula. (default = list(acoustic_detection ~ s(lod, k = 10)))
#' @param dataset A data frame containing predictor variables and a binary
#'   response column named `acoustic_detection`.
#' @param n_partitions Integer. Number of cross-validation folds (default = 5).
#' @param grid_levels Integer. Number of levels for the grid search (default = 4).
#' @param dir Character. Path to the directory where the fitted model
#'   will be saved.
#'
#' @return Invisibly returns the fitted and finalized workflow. Also saves a
#'   bundled model object as an `.rds` file in `dir`.
#' @export
#'
#' @examples
#' \dontrun{
#' fit_gam_formula(
#'   model_formula = list(acoustic_detection ~ s(lod, k = 10)),
#'   dataset = my_data,
#'   n_partitions = 5,
#'   dir = "~./04_results/01_brt/models"
#' )
#' }
train_gam_model <- function(model_formula = acoustic_detection ~ s(lod, k = 10) + habitat,
                            dataset = inside_owf,
                            n_partitions = 5,
                            grid_levels = 4,
                            dir = mod_gam_dir) {

  # # tmp
  # model_formula <- acoustic_detection ~ s(lod, k = 5)
  # dataset <- inside_owf
  # n_partitions <- 5
  # grid_levels <- 4
  # dir <- mod_gam_dir

  # Ensure results directory exists
  dir.create(dir, recursive = TRUE, showWarnings = FALSE) 

  # Generate a unique model name for saving
  model_name <- get_model_name(dataset, model_formula)
  # when testing
  # model_name <- get_model_name(inside_owf, model_formula)

  # Split the raw data
  data_split <- rsample::initial_split(
    dataset,
    prop = 0.8,
    strata = acoustic_detection
  )
  train_data <- rsample::training(data_split)
  test_data  <- rsample::testing(data_split)

  # Recipe: normalize numeric predictors + drop zero variance
  model_recipe <- recipes::recipe(acoustic_detection ~ ., data = train_data) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  # Cross-validation folds
  # cv_folds <- rsample::vfold_cv(train_data, v = n_partitions)
  cv_folds <- rsample::vfold_cv(train_data, v = n_partitions, strata = acoustic_detection) #ensure each fold has both classes

  # Grid control
  ctrl_grid <- tune::control_grid(
    save_pred = TRUE,
    save_workflow = TRUE,
    parallel_over = "resamples"
  )

  # Parameter grid
  gam_grid <- dials::grid_regular(
    dials::adjust_deg_free(),
    levels = grid_levels
  )

  # Define workflow
  workflow <- workflows::workflow() |>
    workflows::add_recipe(model_recipe) |>
    workflows::add_model(
      parsnip::gen_additive_mod(adjust_deg_free = tune::tune()) |>
        parsnip::set_engine(
          "mgcv",
          family = binomial(link = "logit"),
          method = "REML"
          # , select = TRUE #lets mgcv automatically shrink/penalize problematic smooths | doesnt work
        ) |>
        parsnip::set_mode("classification"),
      formula = model_formula
    )

  # Run tuning - this is the time-consuming step
  # If this error returns: → A | warning: Fitting terminated with step failure - check results carefully, it's from this command
  tuned <- tune::tune_grid(
    workflow,
    resamples = cv_folds,
    grid = gam_grid,
    control = ctrl_grid,
    metrics = yardstick::metric_set(
      yardstick::accuracy,
      yardstick::roc_auc,
      tidysdm::boyce_cont,
      tss,
      yardstick::pr_auc,
      yardstick::sens
    )
  )

  # Save tuning results
  cv_metrics <- tuned |> tune::collect_metrics() |>
    dplyr::filter(.config == best_params$.config)

  write.csv(cv_metrics, file = file.path(dir,
              paste0(model_name, "_cv_metrics.csv")))

  # Pick best
  best_params <- tuned |> tune::select_best(metric = "roc_auc")

  # Finalize workflow
  final_wf <- workflow |> tune::finalize_workflow(best_params)

  # Fit final model
  final_fit <- final_wf |> parsnip::fit(train_data)

  # Bundle and save
  bundled <- bundle::bundle(final_fit)
  saveRDS(
    bundled,
    file.path(dir,
              paste0(model_name, ".rds"))
  )

  invisible(final_fit)
}
