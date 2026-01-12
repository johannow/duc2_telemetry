#' Export a NetCDF layer to S3 as a stars object
#'
#' Converts a NetCDF-compatible raster layer to a \code{stars} object,
#' writes it to disk as a NetCDF file, and uploads it to an S3-compatible
#' object store.
#'
#' @param layer A raster object (e.g. \code{SpatRaster}) to be converted to
#'   a \code{stars} object.
#' @param layer_name Character. Name to assign to the stars layer.
#' @param layer_units Character. Units attribute for the layer.
#' @param layer_description Character. Description metadata for the layer.
#' @param dir Character. Directory where the NetCDF file will be written.
#' @param filename Character or \code{NULL}. Optional output filename
#'   (without extension). If \code{NULL}, the object name of \code{layer}
#'   is used.
#' @param compress Logical. Should NetCDF compression be applied?
#' @param bucket_name Character. Name of the S3 bucket. Defaults to the
#'   \code{bucket_name} environment variable.
#'
#' @details
#' This function uses \pkg{stars} to write NetCDF files and \pkg{paws}
#' to upload the resulting file to an S3-compatible endpoint (e.g. MinIO).
#' AWS credentials and endpoint configuration are read from environment
#' variables.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effects
#'   (file creation and S3 upload).
#'
#' @seealso
#' \code{\link[stars]{write_stars}},
#' \code{\link[paws]{s3}}
#'
#' @export

library(dplyr)
library(terra)
library(stars)
library(paws)

export_layer_as_s3 <- function(layer, layer_name, layer_units, layer_description,
                               dir, filename = NULL, compress = FALSE, bucket_name = Sys.getenv("bucket_name")){
  # 1. convert into stars obj
  s_layer <- stars::st_as_stars(layer) %>% suppressWarnings()
  names(s_layer) <- layer_name %>% as.character()
  attr(s_layer, "units") <- layer_units %>% as.character()
  attr(s_layer, "description") <- layer_description %>% as.character()
  
  # 2. write stars .nc file
  if(is.null(filename)){ #make the filename the name of the layer obj
    filename <- paste0(deparse(substitute(layer)), ".nc")
  }else{
    filename <- paste0(filename,".nc")
    }
  
  nc_dir <- file.path(dir, filename)
  
  if(isTRUE(compress)){
  write_stars(s_layer, nc_dir, options = c("COMPRESS=4")) # optional compression
  }else{
    write_stars(s_layer, nc_dir)
  }
  
  # 3. Initialize S3 client
  minio <- paws::s3(
    config = list(
      credentials = list(
        creds = list(
          access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
          secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
          session_token = Sys.getenv("AWS_SESSION_TOKEN")
        )
      ),
      endpoint = paste0("https://", Sys.getenv("AWS_S3_ENDPOINT")),
      region = Sys.getenv("AWS_DEFAULT_REGION")
    )
  )
  
  # 4. Upload NetCDF to S3

  minio$put_object(
    Bucket = bucket_name,
    Key = filename,
    Body = nc_dir
  )
  
  # 5. Verify
  message(paste("Uploaded", nc_dir, "to bucket", bucket_name))
}