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


#' @export
xgb_tune <- function(folds, ensemble_ctrl, recipe, 
  tree_num_min = 1000, tree_num_max = 4000, 
  tree_depth_min = 3, tree_depth_max = 15, 
  learn_rate_min = -3, learn_rate_max = 0,
  spw_min = 5, spw_max = 70,
  levels_spw = 3,  levels_treenum = 5, 
  levels_treedepth = 3, 
  levels_learnrate = 3) { 
  print("Starting grid search...")
  print(Sys.time())


  #tune hyperparameters
  xgb_opt <-
    parsnip::boost_tree(
      trees = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      # sample_size = tune(), # nolint
      # mtry = tune() # nolint
    ) |>
    set_engine("xgboost", scale_pos_weight = tune()) |>
    # set_engine("xgboost",
    # # Add early stopping parameter
    #            # Ensure parallelization
    #           nthread = parallel::detectCores() - 1) %>%  # nolint
    set_mode("classification")


  # Create tuning grid for XGBoost hyperparameters
  xgb_grid <- dials::grid_regular(
    trees(c(tree_num_min, tree_num_max)),
    tree_depth(c(tree_depth_min, tree_depth_max)),
    learn_rate(c(learn_rate_min, learn_rate_max)),
    scale_pos_weight(c(spw_min, spw_max)),
    levels = c(trees = levels_treenum,
               tree_depth = levels_treedepth,
               learn_rate = levels_learnrate,
               scale_pos_weight = levels_spw)
  )

  # Create workflow
  xgb_wf <-
    workflow() %>%
    add_model(xgb_opt) %>%
    add_recipe(recipe)

  # fit model
  start_time <- Sys.time() # nolint
  xgb_tune <- tune_grid(xgb_wf,
    resamples = folds,
    grid = xgb_grid,
    control = ensemble_ctrl,
    metrics = metric_set(
      roc_auc, accuracy, sens, spec
      # roc_auc, sens, spec # for classification
    # rmse,     # Root Mean Squared Error
    # rsq,      # R-squared
    # mae       # Mean Absolute Error
    # boyce_cont, # optional, if your custom regression metric is ready
    )#,
    # show_progress = TRUE
  )

  print("Finished tuning!")
  print(Sys.time())

  end_time <- Sys.time() # nolint
  return(xgb_tune) # nolint
}
