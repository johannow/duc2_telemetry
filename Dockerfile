# Base: R in a Debian Linux environment
FROM rocker/r-ver:4.2.2

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libpng-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    zlib1g-dev \
    libudunits2-dev \
    libsodium-dev \
    build-essential \
    pkg-config \
    python3 python3-venv python3-pip \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /usr/src/app

# Copy only renv.lock first (cache layer)
COPY renv.lock renv/ ./

# Install renv and restore packages
RUN R -e 'install.packages("renv", repos="https://cloud.r-project.org")' \
    && R -e 'renv::restore(clean = TRUE)'

# Copy rest of repo contents into container
COPY . .


# Default command: run orchestrating script
CMD ["Rscript", "source_scripts.R"]