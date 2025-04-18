##' Retrieve a versioned copy of the leaf13C dataset
##'
##' This function provides access to a versioned dataset hosted on GitHub
##' using the \pkg{datastorr} framework. It will download and cache the dataset
##' if not already available locally, and return it as a data frame.
##'
##' If no version is specified, the most recent available version will be used.
##' If no path is specified, a suitable persistent directory will be chosen
##' automatically using \code{rappdirs::user_data_dir()}.
##'
##' @title Get leaf13C dataset
##'
##' @param version Optional character string specifying the dataset version to retrieve.
##'   If \code{NULL} (default), the most recent available version will be used.
##'
##' @param path Optional file path to store and access the dataset locally.
##'   If \code{NULL} (default), a system-appropriate location will be used,
##'   determined via \code{rappdirs::user_data_dir("leaf13C")}.
##'
##' @return A data frame containing the leaf13C dataset.
##'
##' @export
##'
##' @examples
##' # Load the most recent version of the dataset
##' data <- get_data()
##'
##' # Load a specific version of the dataset
##' data <- get_data(version = "0.1.0")
##'
##' # Use a custom path
##' data <- get_data(path = "~/my_data")
get_data <- function(version = NULL, path = NULL) {
  resolved_path <- if (is.null(path)) {
    rappdirs::user_data_dir("leaf13C")
  } else {
    path
  }
  
  resolved_version <- if (is.null(version)) {
    datastorr::github_release_version_current(dataset_info(resolved_path), local = TRUE)
  } else {
    version
  }
  
  cat("Using version:", resolved_version, "\n")
  cat("Local path to data:", resolved_path, "\n")
  
  datastorr::github_release_get(dataset_info(resolved_path), resolved_version)
}


##' Internal dataset metadata used by datastorr
##'
##' This function defines metadata needed by datastorr, including repository,
##' filename, and the function to read the file.
##'
##' @param path Local path to store/retrieve the dataset.
##'
##' @return A datastorr dataset info object.
##' @keywords internal
##' @export
dataset_info <- function(path) {
  datastorr::github_release_info("wcornwell/leaf_13C",
                                 filename = "leaf13C.csv",
                                 read = read_csv,
                                 path = path)
}

dataset_get <- function(version=NULL, path=NULL) {
  datastorr::github_release_get(dataset_info(path), version)
}

##' @export
##' @rdname get_data
##' @param local Logical indicating if local or github versions should
##'   be polled.  With any luck, \code{local=FALSE} is a superset of
##'   \code{local=TRUE}.  For \code{mydata_version_current}, if
##'   \code{TRUE}, but there are no local versions, then we do check
##'   for the most recent github version.
dataset_versions <- function(local=FALSE, path=NULL) {
  datastorr::github_release_versions(dataset_info(path), local)
}

##' @export
##' @rdname get_data
dataset_version_current <- function(local=FALSE, path=NULL) {
  datastorr::github_release_version_current(dataset_info(path), local)
}

##' @export
##' @rdname get_data
dataset_del <- function(version, path=NULL) {
  datastorr::github_release_del(dataset_info(path), version)
}

read_csv <- function(...) {
  read.csv(..., stringsAsFactors=FALSE)
}

dataset_release <- function(description, path=NULL, ...) {
  datastorr::github_release_create(dataset_info(path),
                                   description=description, ...)
}
