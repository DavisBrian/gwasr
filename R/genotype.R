# TBD / Ideas
# - [ method to keep class and AAC / AFF correct  when subsetting
# - c method to combine genotype matrices

#' @title Create a genotype object
#'   
#' @description This function creates a genotype object.
#' 
#' @param Z A numeric genotype matrix (dosage matrix) or coercible with 
#'   as.matrix - rows correspond to individuals and columns correspond to SNPs. 
#'   Use 'NA' for missing values. The column names of this matrix should 
#'   correspond to SNP names in the \code{snpinfo} object. The row names of this
#'   matrix should correspond to the id in the \code{phenotype} object.
#' @param subject.include (optional) character vector of the subjects in
#'   \code{rownames(Z)} to be included in the analysis.  See Details.
#' @param subject.exclude (optional) character vector of the subjects in
#'   \code{rownames(Z)} to be excluded in the analysis.  See Details.
#' @param snp.include (optional) character vector of the SNPs in
#'   \code{colnames(Z)} to be included in the analysis.  See Details.
#' @param snp.exclude (optional) character vector of the SNPs in
#'   \code{colnames(Z)} to be excluded in the analysis.  See Details.
#'   
#' @details Z should be a numeric matrix
#' 
#'   Typically either \code{subject.include} or \code{subject.exclude}, or both,
#'   is set to \code{NULL}. It is important to note that return \code{include}
#'   and \code{exclude} are taken from the subjects in \code{Z} and not from the
#'   input parameters.
#'   
#'   Typically either \code{snp.include} or \code{snp.exclude}, or both, is set
#'   to \code{NULL}. It is important to note that return \code{include} and 
#'   \code{exclude} are taken from the snps in \code{Z} and not from the input
#'   parameters.
#'   
#' @return an object of class 'genotype'.  
#'
#'   An object of class 'genotype' is a list containing at least the following
#'   components:
#'   
#' \itemize{
#'  \item data data frame of the data for analysis 
#'  \item AAC alternate allele count.
#'  \item AAF alternate allele frequency. 
#'  \item gender column name containing the gender of the subjects
#'  \item include the subjects in \code{data} that will be included is further analysis.
#'  \item exclude the subjects in \code{data} that will be excluded is further analysis.    
#' }
#' 
#' It is important to note that return \code{include} and \code{exclude} are taken from data and not from the input parameters. 
#' @export
#
# [TBD]
#  - sparse matrix
#  - model type (additive, etc) support
#  -add "problems" (a la readr)
genotype <- function(Z, subject.include = NULL, subject.exclude = NULL, 
                     snp.include = NULL, snp.exclude = NULL) {
  
    if (!is.matrix(Z)) {
      Z <- as.matrix(Z)
    }
    
    # make sure the data is numeric
    if (!is.numeric(Z)) {
      stop("Genotype matrix is ", typeof(Z), " must be numeric (integer or double).")
    }
  
  # [TBD] - Check range
  
  # [TBD] - check for missing
    
#     subjects_all <- rownames(Z)
#     snps_all <- colnames(Z)
    
    data <- Z
    
#     data <- reduce_data(
#       Z,
#       row.include = subject.include,
#       row.exclude = subject.exclude,
#       col.include = snp.include,
#       col.exclude = snp.exclude
#     )
    new_class <- class(data)
    
    structure(
      data,
      AAC = colSums(data, na.rm = TRUE),
      AAF = colMeans(data, na.rm = TRUE) / 2.0,
      included = list(subjects = rownames(data), snps = colnames(data)),
      excluded = list(
        subjects = setdiff(rownames(Z), rownames(data)),
        snps = setdiff(colnames(Z), colnames(data))
      ),
      class = c("genotype", new_class)
    )
  }


#' @rdname genotype
#' @export
is_genotype <- function(x) inherits(x, "genotype")


# get functions  ---------------------------------------------------------------

#' @export
get_subjects.genotype <- function(x, excluded=FALSE) {
  stopifnot(length(excluded) == 1L)
  if (excluded) {
    attr(x, "excluded")[["subjects"]]  
  } else {
    intersect(attr(x, "included")[["subjects"]], rownames(x))
  }
}


#' @export
get_snps.genotype <- function(x, excluded=FALSE, ...) {
  stopifnot(length(excluded) == 1L)
  if (excluded) {
    attr(x, "excluded")[["snps"]]  
  } else {
    intersect(attr(x, "included")[["snps"]], colnames(x))
  }
}


# single verbs -----------------------------------------------------------------

#' @export
reduce.genotype <- function(g, common) {
  
  # subjects
  subjects_dropped <- setdiff(get_subjects(g), common$subjects)
  if (length(subjects_dropped) == 0L) {
    subjects_dropped <- NA
  } 
  o_subjects <- match(common$subjects, rownames(g))
  
  # snps
  snps_dropped <- setdiff(get_snps(g), common$snps)
  if (length(snps_dropped) == 0L) {
    snps_dropped <- NA
  } 
  o_snps <- match(common$snps, colnames(g))
  
  data <- g[o_subjects, o_snps]
 
  structure(
    data,
    AAC = colSums(data, na.rm = TRUE),
    AAF = colMeans(data, na.rm = TRUE) / 2.0,
    included = attr(g, "included"),
    excluded = attr(g, "excluded"),
    dropped = list(subjects = subjects_dropped, snps = snps_dropped),
    class = class(g)
  )
}


