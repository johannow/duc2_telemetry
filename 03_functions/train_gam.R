
#' Train gam
#' 
#' This helper function aggregates several steps related to training a model.
#' 1) training the model using mgcv::gam()
#' 2) bundling and saving the trained model object
#' 3a) saving model metadata information in a .csv file
#' 3b) saving statistics of the model in a separate .csv file
#'
#' @param formula model formula as formulated in any GLM/GAM
#' @param family model family, e.g. nb, tweedie, ...
#' @param method model estimation method (string)
#' @param dataset dataset obj to be used for the modelling
#' @param dir directory to save the model, its statistics, and metadata in (string)
#' @param model_name name of the model to be used when saving (string)
#'
#' @returns trained model object
#' @export
#'
#' @examples
train_gam <- function(formula, family = "nb", method = "REML", dataset, dir, model_name){
  # Step 0: create directory for the model inside the given dir
  model_folder <- file.path(dir, model_name)
  
  if (!dir.exists(model_folder)) {
    dir.create(model_folder)
  }
  
  # Step 1) Train model

    start_time <- Sys.time()
    model <- mgcv::gam(formula,
                  family = family,
                  method = method,
                  data = dataset)
    end_time <- Sys.time()
    
    # log time it took the model to run
    model_runtime = end_time - start_time
    print(model_runtime) # +- 1min
  
  # Step 2) Bundle and save model
    # bundle model
    model_bundle <- bundle::bundle(model)
    
    # save bundled model to .rds
    saveRDS(model_bundle, file.path(model_folder, paste0(model_name, ".rds")))
    
  # Step 3)
    # export model statistics
    export_model_stats(model, model_name = model_name, dir = model_folder)
    
  # Step 4
    # save metadata of model
    model_metadata <-
      tibble(name = model_name,
             dataset = dataset,
             formula = formula |>
               deparse1(collapse = ""),
             family = family,
             method = method,
             runtime_s = model_runtime |> as.numeric(),
             directory = model_folder)

    readr::write_csv(model_metadata, file = file.path(model_folder, paste0(model_name, "_model_metadata.rds")))
    
    return(model)
}