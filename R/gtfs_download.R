#' Download and Extract GTFS Data
#'
#' Downloads a GTFS (General Transit Feed Specification) ZIP file from a URL
#' and extracts it to a temporary directory. Implements caching and fallback
#' to cached data if download fails.
#'
#' @param agency_name Character. Name of the transit agency (used for caching
#'   and temporary directory naming). Examples: "cta", "pace", "metra"
#' @param zip_link Character. Full URL to the GTFS ZIP file
#'
#' @return Character. Path to the temporary directory containing extracted GTFS files
#'
#' @details
#' The function implements a robust download strategy:
#' \enumerate{
#'   \item Creates agency-specific temp directory in system temp
#'   \item Downloads GTFS ZIP from provided URL with 60-second timeout
#'   \item Caches successful downloads to \code{gtfs_cache/} directory
#'   \item Falls back to cached data if download fails
#'   \item Extracts ZIP contents to temp directory
#' }
#'
#' The cache directory (\code{gtfs_cache/}) is created in the working directory
#' if it doesn't exist. Cached files are named \code{<agency_name>_gtfs.zip}.
#'
#' @section Error Handling:
#' If download fails and no cache exists, the function stops with an error.
#' If download fails but cache exists, a warning is issued and cached data is used.
#'
#' @examples
#' \dontrun{
#' # Download CTA GTFS data
#' cta_dir <- download_and_extract_gtfs(
#'   "cta",
#'   "https://www.transitchicago.com/downloads/sch_data/google_transit.zip"
#' )
#'
#' # List extracted files
#' list.files(cta_dir)
#' }
#'
#' @export
download_and_extract_gtfs <- function(agency_name, zip_link) {
  temp_dir <- file.path(tempdir(), agency_name)
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }

  temp_file <- file.path(temp_dir, paste0(agency_name, "_gtfs.zip"))

  cache_dir <- "gtfs_cache"
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir)
  }
  cache_file <- file.path(cache_dir, paste0(agency_name, "_gtfs.zip"))

  tryCatch({
    options(timeout = 60)

    response <- httr::GET(
      zip_link,
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"),
      httr::write_disk(temp_file, overwrite = TRUE),
      httr::timeout(60)
    )

    if (httr::status_code(response) != 200) {
      stop(paste0("Failed to download with status code: ", httr::status_code(response)))
    }

    file.copy(temp_file, cache_file, overwrite = TRUE)

  }, error = function(e) {
    message(paste0("Download failed for ", agency_name, ": ", e$message))
    if (file.exists(cache_file)) {
      message(paste0("Using cached GTFS data for ", agency_name, " from ", cache_file))
      file.copy(cache_file, temp_file, overwrite = TRUE)
    } else {
      stop(paste0("Could not download GTFS data for ", agency_name, " and no cache available."), call. = FALSE)
    }
  })

  gtfs_files <- unzip(temp_file, exdir = temp_dir)
  return(temp_dir)
}
