#' @export
get_snps <- function(x, ...) UseMethod("get_snps", x)

#' @export
get_snps.default <- function(x, ...) "Unknown class"

#' @export
get_subjects <- function(x, ...) UseMethod("get_subjects", x)

#' @export
get_subjects.default <- function(x, ...) "Unknown class"
