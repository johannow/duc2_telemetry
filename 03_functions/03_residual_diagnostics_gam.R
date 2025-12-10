check_gam_residuals <- function(model, data, dir, model_name) {
  if (missing(data)) stop("Data must be provided for DHARMa residual checks.")
  
  # create directory for the model inside the given dir
  model_folder <- file.path(dir, model_name)
  if (!dir.exists(model_folder)) {
    dir.create(model_folder)
  }
  
  output <- utils::capture.output({ # for logfile
    message("🔹 Running DHARMa residual diagnostics ...")
    
    # Simulate residuals (safe version that avoids mgcViz requirement)
    options(DHARMa.use.mgcViz = FALSE)
    sim <- DHARMa::simulateResiduals(model, n = 500, plot = FALSE)
    
    # Tests
    message("🔹🔹 Testing the uniformity of simulated residuals \n (by applying a stats::ks.test)")
    test_uniform <- DHARMa::testUniformity(sim)
    message("🔹🔹 Testing Dispersion \n by running simulation-based tests for over/underdispersion")
    test_dispersion <- DHARMa::testDispersion(sim)
    message("🔹🔹 Testing for Outliers \n (if the number of observations outside the simulatio envelope are larger or smaller than expected)")
    test_outliers <- DHARMa::testOutliers(sim)
    
    # === SHOW THE PLOTS ===
    plot(sim)  # QQ, residual-fitted, histogram
    
    message("\n🔍 DHARMa Test Results")
    print(test_uniform)
    print(test_dispersion)
    print(test_outliers)
    
    # === SAVE THE RESULTS ===
    file_path <- file.path(model_folder, "gam_residuals_plots.pdf")
    
    pdf(file_path, width = 8, height = 6)
    DHARMa::simulateResiduals(model, n = 500, plot = FALSE)
    DHARMa::testUniformity(sim, plot = FALSE)
    DHARMa::testDispersion(sim, plot = FALSE)
    DHARMa::testOutliers(sim, plot = FALSE)
    dev.off()
    message("🔹 DHARMa residuals plots saved to: ", file_path)
    
    ## --------------------------
    ## INTERPRETATION SECTION
    ## --------------------------
    
    cat("\n\n📘 *Interpretation Help*\n")
    
    # 1️⃣ Uniformity / QQ plot
    cat("\n1️⃣ QQ Plot / Uniformity Test\n")
    cat("- Checks if residuals follow the expected distribution under the model.\n")
    cat("- Look for: points near 45° line; no S-shape or curvature.\n")
    if (test_uniform$p.value < 0.05) {
      cat("❗ Result: Significant deviation from uniformity (p < 0.05)\n")
      cat("→ Possible causes: wrong family/link, unmodelled nonlinearity, missing predictors.\n")
      cat("→ Suggested actions:\n")
      cat("   * Try a different family (NB, Tweedie, Gamma).\n")
      cat("   * Add smooths or interactions.\n")
      cat("   * Check covariates for missing dynamics.\n")
    } else {
      cat("✓ Result: No significant deviation (residual distribution looks OK).\n")
    }
    
    # 2️⃣ Dispersion test
    cat("\n2️⃣ Dispersion Test\n")
    cat("- Tests if residual variance matches expectations.\n")
    cat("- Red line = observed SD; histogram = simulated SD.\n")
    if (test_dispersion$p.value < 0.05) {
      cat("❗ Result: Dispersion significantly different from expected.\n")
      
      # classify over vs underdispersion
      fitted_sd <- sd(sim$scaledResiduals)
      sim_sd <- mean(sim$refit$sds)
      
      if (fitted_sd > sim_sd) {
        cat("→ **Overdispersion detected.**\n")
        cat("   * Try Negative Binomial, Tweedie, quasi-families.\n")
        cat("   * Add missing predictors or random effects.\n")
      } else {
        cat("→ **Underdispersion detected.** (often indicates zero-inflation)\n")
        cat("   * Check for zero-inflation or bounded responses.\n")
        cat("   * Consider Beta/Beta-inflated or hurdle/ZI models.\n")
      }
    } else {
      cat("✓ Result: No dispersion problems detected.\n")
    }
    
    # 3️⃣ Outlier test
    cat("\n3️⃣ Outlier Test\n")
    cat("- Checks if there are more extreme residuals than expected.\n")
    if (test_outliers$p.value < 0.05) {
      cat("❗ Result: Significant outliers detected.\n")
      cat("→ Suggested actions:\n")
      cat("   * Inspect high residual points: `which(sim$scaledResiduals > 0.9)`.\n")
      cat("   * Check for data errors or missing covariates explaining extremes.\n")
      cat("   * Consider more flexible smooths or interaction terms.\n")
    } else {
      cat("✓ Result: Outlier count within expected range.\n")
    }
    
    ## Pass/fail flag
    dharma_pass <- (
      test_uniform$p.value    >= 0.05 &&
        test_dispersion$p.value >= 0.05 &&
        test_outliers$p.value   >= 0.05
    )
    
    if (!dharma_pass) {
      warning("⚠️ DHARMa flagged at least one residual issue.\n",
              "   → See interpretation notes above.")
    } else {
      message("✓ All DHARMa residual diagnostics look healthy.")
    }
  })
  
  # Write log to file.path()
  log_path <- file.path(model_folder, "gam_residuals_log.txt")
  writeLines(output, log_path)
  
  message("📄 Log saved to: ", log_path)
  
  invisible(list(
    sim = sim,
    tests = list(
      uniformity = test_uniform,
      dispersion = test_dispersion,
      outliers = test_outliers
    ),
    dharma_pass = dharma_pass
  ))
}


# check_gam_residuals <- function(model, data) {
#   if (missing(data)) stop("Data must be provided for DHARMa residual checks.")
#   
#   message("🔹 Running DHARMa residual diagnostics ...")
#   sim <- DHARMa::simulateResiduals(model, n = 500)
#   
#   test_uniform <- DHARMa::testUniformity(sim)
#   test_dispersion <- DHARMa::testDispersion(sim)
#   
#   print(test_uniform)
#   print(test_dispersion)
#   
#   dharma_pass <- TRUE
#   if (test_uniform$p.value < 0.05 || test_dispersion$p.value < 0.05) {
#     warning("⚠️ DHARMa detected potential residual problems.\n",
#             "   → Consider checking residual vs predictors or alternative families.")
#     dharma_pass <- FALSE
#   } else {
#     message("✓ DHARMa residuals look healthy.")
#   }
#   
#   invisible(list(simulation = sim, dharma_pass = dharma_pass))
# }
