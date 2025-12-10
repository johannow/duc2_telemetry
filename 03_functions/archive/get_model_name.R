#' Generate a unique model name from a dataset and formula
#'
#' This helper function creates a standardized string identifier
#' for saving or tracking models. It combines the name of the dataset
#' (as passed to the function) with a cleaned version of the model formula.
#'
#' @param dataset A data frame or tibble used to fit the model.
#'   Only the object name is used in the generated string.
#' @param model_formula A formula describing the model structure
#'   (e.g., `acoustic_detection ~ s(sst) + s(bathy)`).
#'
#' @return A character string with the format:
#'   `datasetname_modelterms_gam`
#'   where terms are collapsed and stripped of spaces, operators, and
#'   the response variable `acoustic_detection`.
#'
#' @examples
#' df <- data.frame(x = 1:10, y = rnorm(10))
#' f <- y ~ x
#' get_model_name(df, f)
#'
#' @export

get_model_name <- function(dataset, model_formula) {
 
  dataset_name <- deparse(substitute(dataset))
  
  formula_str <- deparse1(model_formula, collapse = "")

  # Convert to compact name
  m_name <- formula_str %>%
    gsub("acoustic_detection\\s*~\\s*", "", .) %>%  # remove response
    gsub("\\s+", "", .) %>%                          # remove all whitespace
    gsub("\\(", "_", .) %>%                          # replace '(' with '_'
    gsub("\\)", "", .) %>%                           # remove ')'
    gsub(",", "", .) %>%                             # remove commas
    gsub("=", "", .) %>%                             # remove '='
    gsub('"', "", .) %>%                             # remove quotes
    gsub("\\+", "__", .)                             # replace '+' with double underscore
  
  m_name

  name <- paste0(dataset_name, "_", m_name, ".rds")
  
  return(name)       
}


