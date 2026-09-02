# Internal environment that holds the prediction data once downloaded.
# Not exported.
msa_data_env <- new.env(parent = emptyenv())

#' Download and Cache MSA Health-Utility Prediction Data
#'
#' Downloads the Bayesian utility prediction dataset used by
#' \code{\link{calculate_MSA_utility}} from Zenodo (\doi{10.5281/zenodo.22229796})
#' and caches it locally so it is downloaded only once. This happens
#' automatically the first time \code{\link{calculate_MSA_utility}} is
#' called in a session, so most users will not need to call this directly.
#'
#' @param refresh Logical. If \code{TRUE}, re-download even if a cached
#'   copy already exists. Default \code{FALSE}.
#'
#' @return Invisibly returns the loaded list of prediction grids.
#'
#' @details
#' The dataset (Kuchin, 2026) is licensed under CC BY 4.0 and is
#' downloaded from Zenodo (\doi{10.5281/zenodo.22229796}) the first time
#' it is needed. It is cached at
#' \code{tools::R_user_dir("MSACalcQALY", "cache")}, so later sessions
#' do not re-download it. Requires an internet connection on first use.
#'
#' @source \doi{10.5281/zenodo.22229796}
#'
#' @export
#'
#' @examples
#' \donttest{
#' get_MSA_data()
#' }
get_MSA_data <- function(refresh = FALSE) {

  cache_dir <- tools::R_user_dir("MSACalcQALY", which = "cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  cache_file <- file.path(cache_dir, "MSA_Utility_epred.rda")

  if (!file.exists(cache_file) || isTRUE(refresh)) {

    zenodo_url <- "https://zenodo.org/records/22229796/files/MSA_Utility_epred.rda?download=1"

    message(
      "Downloading MSA utility prediction data (~41 MB) from Zenodo ",
      "(https://doi.org/10.5281/zenodo.22229796).\n",
      "This happens once; the file will be cached at:\n  ", cache_file
    )

    tmp <- tempfile(fileext = ".rda")
    dl_result <- tryCatch(
      utils::download.file(zenodo_url, destfile = tmp, mode = "wb", quiet = TRUE),
      error = function(e) e
    )

    if (inherits(dl_result, "error") || !file.exists(tmp) || file.size(tmp) == 0) {
      stop(
        "Failed to download the required data from Zenodo. Check your ",
        "internet connection and try again, or download it manually from\n",
        "  https://doi.org/10.5281/zenodo.22229796\n",
        "and place it at:\n  ", cache_file,
        call. = FALSE
      )
    }

    expected_md5 <- "a05cffcde7ee725b833d67e31bde8f1b"
    actual_md5 <- unname(tools::md5sum(tmp))
    if (!identical(actual_md5, expected_md5)) {
      unlink(tmp)
      stop(
        "Downloaded file failed an integrity check (MD5 mismatch). ",
        "Please try again, or contact the package maintainer.",
        call. = FALSE
      )
    }

    file.copy(tmp, cache_file, overwrite = TRUE)
    unlink(tmp)
  }

  load(cache_file, envir = msa_data_env)
  invisible(msa_data_env$MSA_Utility_epred)
}
