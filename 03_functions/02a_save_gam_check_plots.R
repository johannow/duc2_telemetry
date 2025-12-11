check_and_save_gam <- function(model, dir, model_name) {
  
  # ---- Create model folder ----
  model_folder <- file.path(dir, model_name)
  if (!dir.exists(model_folder)) dir.create(model_folder, recursive = TRUE)
  
  pdf_path <- file.path(model_folder, "gam_checks.pdf")
  # log_path <- file.path(model_folder, "gam_checks_log.txt")
  
  # Capture *all* output and messages
  # log_text <- capture.output(
  #   {
      message("🔹 Model summary")
      print(summary(model))
      
      message("🔹 Running mgcv::gam.check()...")
      pdf(pdf_path, width = 8, height = 6)
      mgcv::gam.check(model, rep = 500)
      dev.off()
      message("🔹 GAM check plots saved to: ", pdf_path)
      
      message("🔹 Interpretation help for gam.check()")
        cat("\n1️⃣ QQ plot of residuals\n")
        cat("- Checks if residuals follow the expected distribution\n")
        cat("- Look for: points near 45° line, heavy tails, S-shaped curves\n\n")

        cat("2️⃣ Residuals vs linear predictor\n")
        cat("- Checks for heteroskedasticity or missing non-linear patterns\n")
        cat("- Look for: funnel shapes, curves\n\n")

        cat("3️⃣ Histogram of residuals\n")
        cat("- Checks residual distribution\n")
        cat("- Look for: skew, heavy tails, bimodality\n\n")

        cat("4️⃣ Response vs fitted values\n")
        cat("- Checks prediction accuracy and link function\n")
        cat("- Look for: points away from 45° line, trends\n")
  #   },
  #   type = "output"   # <- captures prints + cat + message()
  # )
  
  # # Write clean log file (overwrite)
  # writeLines(log_text, log_path)
  # 
  # # Also print everything to console
  # cat(paste(log_text, collapse = "\n"))
  # 
  # message("📄 Log saved to: ", log_path)
  
  # invisible(list(log = log_path, plots = pdf_path))
}


# check_and_save_gam <- function(model, dir, model_name) {
#   
#   # ---- Create model folder ----
#   model_folder <- file.path(dir, model_name)
#   if (!dir.exists(model_folder)) dir.create(model_folder, recursive = TRUE)
#   
#   # Output file paths
#   pdf_path <- file.path(model_folder, "gam_checks.pdf")
#   log_path <- file.path(model_folder, "gam_checks_log.txt")
#   
#   # ---- Open ONE connection ----
#   con <- file(log_path, open = "wt")
#   
#   # ---- Direct outputs to SAME connection ----
#   sink(con, split = TRUE)                               # normal output → console + file
#   sink(con, type = "message", split = TRUE)             # messages → console + file
#   
#   # Ensure cleanup on exit
#   on.exit({
#     sink(type = "message")
#     sink()
#     close(con)
#   }, add = TRUE)
#   
#   # ---- Everything below prints AND logs ----
#   
#   message("🔹 Model summary")
#   sm_summ <- summary(model)
#   print(sm_summ)
#   
#   message("🔹 Running mgcv::gam.check()...")
#   pdf(pdf_path, width = 8, height = 6)
#   mgcv::gam.check(model, rep = 500)
#   dev.off()
#   message("🔹 GAM check plots saved to: ", pdf_path)
#   
#   message("🔹 Interpretation help for gam.check()")
#   
#   cat("\n1️⃣ QQ plot of residuals\n")
#   cat("- Checks if residuals follow the expected distribution\n")
#   cat("- Look for: points near 45° line, heavy tails, S-shaped curves\n\n")
#   
#   cat("2️⃣ Residuals vs linear predictor\n")
#   cat("- Checks for heteroskedasticity or missing non-linear patterns\n")
#   cat("- Look for: funnel shapes, curves\n\n")
#   
#   cat("3️⃣ Histogram of residuals\n")
#   cat("- Checks residual distribution\n")
#   cat("- Look for: skew, heavy tails, bimodality\n\n")
#   
#   cat("4️⃣ Response vs fitted values\n")
#   cat("- Checks prediction accuracy and link function\n")
#   cat("- Look for: points away from 45° line, trends\n")
#   
#   # ---- At this point we are still inside sink ----
#   
#   # cleanup happens from on.exit()
#   
#   # ---- AFTER sink is closed, show message only in console ----
#   # (cannot put this above!)
#   
#   message("📄 Log saved to: ", log_path)
#   
#   invisible(list(log = log_path, plots = pdf_path))
# }


# check_and_save_gam <- function(model, dir, model_name) {
#   
#   # ---- Create model folder ----
#   model_folder <- file.path(dir, model_name)
#   if (!dir.exists(model_folder)) dir.create(model_folder)
#   
#   # Output file paths
#   pdf_path <- file.path(model_folder, "gam_checks.pdf")
#   log_path <- file.path(model_folder, "gam_checks_log.txt")
#   
#   # ---- Open ONE connection ----
#   con <- file(log_path, open = "wt")
#   
#   # ---- Direct all outputs to the same connection ----
#   sink(con, split = TRUE)                      # normal output → console + file
#   sink(con, type = "message")                  # messages → same file + console
#   # on.exit({
#   #   sink(type = "message")
#   #   sink()
#   #   close(con)
#   # })
#   # 
#   # ---- Everything below prints AND logs ----
#   
#   message("🔹 Model summary")
#   sm_summ <- summary(model)
#   print(sm_summ)
#   
#   message("🔹 Running mgcv::gam.check()...")
#   pdf(pdf_path, width = 8, height = 6)
#   mgcv::gam.check(model, rep = 500)
#   dev.off()
#   message("🔹 GAM check plots saved to: ", pdf_path)
#   
#   plot(mgcv::gam.check(model, rep = 500))
#   mgcv::gam.check(model, rep = 500)
#   
#   message("🔹 Interpretation help for gam.check()")
#   
#   cat("\n1️⃣ QQ plot of residuals\n")
#   cat("- Checks if residuals follow the expected distribution\n")
#   cat("- Look for: points near 45° line, heavy tails, S-shaped curves\n")
#   cat("- Actions: alternative family, transform response, inspect outliers\n")
#   
#   cat("\n2️⃣ Residuals vs linear predictor\n")
#   cat("- Checks for heteroskedasticity or missing non-linear patterns\n")
#   cat("- Look for: funnel shapes, curves\n")
#   cat("- Actions: add predictors, smooths, interactions, consider weights\n")
#   
#   cat("\n3️⃣ Histogram of residuals\n")
#   cat("- Checks residual distribution\n")
#   cat("- Look for: skew, heavy tails, bimodality\n")
#   cat("- Actions: transform response, robust GAM, check subpopulations\n")
#   
#   cat("\n4️⃣ Response vs fitted values\n")
#   cat("- Checks prediction accuracy and link function\n")
#   cat("- Look for: points away from 45° line, trends\n")
#   cat("- Actions: adjust family or link, add smooths/interactions\n")
#   
#   # ---- Stop sinking BEFORE final message ----
#   sink(type = "message")
#   sink()
#   close(con)
#   
#   # ---- Now this prints ONLY in console ----
#   message("📄 Log saved to: ", log_path)
#   
#   invisible(list(log = log_path, plots = pdf_path))
#   
# }


# check_and_save_gam <- function(model, dir, model_name) {
#   # create directory for the model inside the given dir
#   model_folder <- file.path(dir, model_name)
#   if (!dir.exists(model_folder)) {
#     dir.create(model_folder)
#   }
#   file_path <- file.path(model_folder, "gam_checks.pdf")
#   
#   output <- utils::capture.output({ # for logfile
#   
#     message("🔹 Model summary")
#     sm_summ <- summary(model)
#     print(sm_summ)
#     
#     message("🔹 Running mgcv::gam.check()...")
#     # save gam check plots
#     pdf(file_path, width = 8, height = 6)
#     mgcv::gam.check(model, rep = 500)
#     dev.off()
#     message("🔹 GAM check plots saved to: ", file_path)
#     
#     message("🔹 Interpretation help for gam.check()")
#     
#     cat("\n1️⃣ QQ plot of residuals\n")
#     cat("- Checks if residuals follow the expected distribution\n")
#     cat("- Look for: points near 45° line, heavy tails, S-shaped curves\n")
#     cat("- Actions: alternative family, transform response, inspect outliers\n")
#     
#     cat("\n2️⃣ Residuals vs linear predictor\n")
#     cat("- Checks for heteroskedasticity or missing non-linear patterns\n")
#     cat("- Look for: funnel shapes, curves\n")
#     cat("- Actions: add predictors, smooths, interactions, consider weights\n")
#     
#     cat("\n3️⃣ Histogram of residuals\n")
#     cat("- Checks residual distribution\n")
#     cat("- Look for: skew, heavy tails, bimodality\n")
#     cat("- Actions: transform response, robust GAM, check subpopulations\n")
#     
#     cat("\n4️⃣ Response vs fitted values\n")
#     cat("- Checks prediction accuracy and link function\n")
#     cat("- Look for: points away from 45° line, trends\n")
#     cat("- Actions: adjust family or link, add smooths/interactions\n")
#   })
#   
#   # Write log to file.path()
#   log_path <- file.path(model_folder, "gam_residuals_log.txt")
#   writeLines(output, log_path)
#   
#   message("📄 Log saved to: ", log_path)
# }
