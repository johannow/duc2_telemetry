source("renv/activate.R")
.libPaths(c(renv::paths$library(), "/usr/local/lib/R/site-library"))
# Set system library as renv cache
options(renv.config.use.system.library = TRUE)  # allow renv to see system packages
renv::restore(rebuild = FALSE)