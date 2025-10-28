#' Fit and Tune a XGBoost model
#'
#' This function fits and tunes a XGBoost classification model.
#'
#' @param folds A resampling object (e.g., created with `vfold_cv`)
#' for cross-validation.
#' @param ensemble_ctrl A control object for the tuning process,
#' created with `control_grid()`.
#' @param recipe A preprocessed recipe object specifying
#' the data transformations.
#' @return A `tune_results` object containing the results
#' of the model tuning process.
#' @details This function:
#'          1. Defines custom tunable hyperparameters
#'              (`splitrule` and `max.depth`).
#'          2. Specifies a Random Forest model using the `ranger` engine.
#'          3. Creates a tuning grid for hyperparameters.
#'          4. Constructs a workflow combining the model and the recipe.
#'          5. Tunes the model using grid search and evaluates it
#'              with specified metrics.
#' @examples
#' # Example usage:

# Ensure required libraries are loaded
# library(parsnip)
# library(tidymodels)
# library(ggplot2)
# library(dials) # Load dials for the parameters function
# library(workflows) # Load workflows for the workflow function
# library(tune) # Load tune for the tune_grid function
# library(yardstick) # Load yardstick for the metric_set function

# # Define or import tss metric
# tss <- function(data, ...) {
#   # Custom implementation of the TSS metric
#   # Replace this with the actual implementation or import
#   stop("tss metric is not implemented.") # nolint
# }

# # Define or import boyce_cont metric
# boyce_cont <- function(data, ...) {
#   # Custom implementation of the Boyce index metric
#   # Replace this with the actual implementation or import
#   stop("boyce_cont metric is not implemented.") # nolint
# }
#' fitted_ranger <- ranger_fit(
#'   folds = knndm_folds,
#'   ensemble_ctrl = ctrl_grid,
#'   recipe = Occurrence_rec
#' )
#' @export
xgb_fit <- function(folds, ensemble_ctrl, recipe, early_stopping_rounds = 10) { # Create model specification # nolint
  print("Starting grid search...")
  print(Sys.time())

  xgb_opt <-
    parsnip::boost_tree(
      trees = tune(),
      tree_depth = tune(),
      learn_rate = tune()#,
      # sample_size = tune(), # nolint
      # mtry = tune() # nolint
    ) |>
    set_engine("xgboost",
    # Add early stopping parameter # nolint
              early_stopping_rounds = early_stopping_rounds, # nolint
               # Ensure parallelization
              nthread = parallel::detectCores() - 1) %>%  # nolint
    set_mode("classification")


  # Create tuning grid for XGBoost hyperparameters
  xgb_grid <- dials::grid_regular(
    trees(),
    tree_depth(c(3, 20)),
    learn_rate(c(-4, 0)),
    levels = c(trees = 5, tree_depth = 3, learn_rate = 3)
  )
  # # Create tuning grid
  # xgb_gri <-
  #   dials::grid_regular(
  #     parameters(trees(),
  #     tree_depth(c(3, 20)), # nolint: indentation_linter.
  #     learn_rate(c(-4, 0))),
  #     levels = c(trees = 5, tree_depth = 3, learn_rate = 3)
  #   )

  # # Alternative to regular grid search: Create a random grid search for XGBoost hyperparameters
  # xgb_grid_random <- dials::grid_random(
  #   trees(),
  #   tree_depth(c(3, 20)),
  #   learn_rate(c(-4, 0)),
  #   size = 10  # Generate 10 random combinations
  # )

  # Create workflow
  xgb_wf <-
    workflow() %>%
    add_model(xgb_opt) %>%
    add_recipe(recipe)

  # fit model
  start_time <- Sys.time() # nolint
  xgb_fit <- tune_grid(xgb_wf,
    resamples = folds,
    grid = xgb_grid,
    control = ensemble_ctrl,
    metrics = metric_set(
      accuracy,
      roc_auc,
      # boyce_cont,
      # tss,
      pr_auc,
      sens
    )#,
    # show_progress = TRUE
  )

  print("Finished tuning!")
  print(Sys.time())

  end_time <- Sys.time() # nolint
  return(xgb_fit) # nolint
}
