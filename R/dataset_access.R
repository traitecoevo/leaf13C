
##' This function provides access to the versioned dataset
##'
##'
##' @title get_data
##'
##' @param version Version number.  The default will load the most
##'   recent version on your computer or the most recent version known
##'   to the package if you have never downloaded the data before.
##'   With \code{plant_lookup_del}, specifying \code{version=NULL}
##'   will delete \emph{all} data sets.
##'
##' @param path Path to store the data at.  If not given,
##'   \code{datastorr} will use \code{rappdirs} to find the best place
##'   to put persistent application data on your system.  You can
##'   delete the persistent data at any time by running
##'   \code{mydata_del(NULL)} (or \code{mydata_del(NULL, path)} if you
##'   use a different path).
##'
##' @export
##' @examples
##' #
##' # see the format of the resource
##' #
##' #
##' #

get_data <- function(version = NULL, path = NULL) {
  # Resolve path using rappdirs logic
  resolved_path <- if (is.null(path)) {
    rappdirs::user_data_dir("leaf13C")  # "leaf13C" is your dataset name
  } else {
    path
  }
  
  # Resolve version
  resolved_version <- if (is.null(version)) {
    dataset_version_current(local = TRUE, path = resolved_path)
  } else {
    version
  }
  
  # Print the resolved values
  cat("Using version:", resolved_version, "\n")
  cat("Path:", resolved_path, "\n")
  
  # Get the dataset
  dataset_get(version = resolved_version, path = resolved_path)
}



## This one is the important part; it defines the three core bits of
## information we need;
##   1. the repository name (traitecoevo/taxonlookup)
##   2. the file to download (plant_lookup.csv)
##   3. the function to read the file, given a filename (read_csv)
dataset_info <- function(path) {
  datastorr::github_release_info("wcornwell/leaf_13C",
                                 filename="leaf13C.csv",
                                 read=read_csv,
                                 path=path)
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
