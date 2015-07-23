#' @title Create a snpinfo object
#'   
#' @description This function creates a snpinfo object.
#' 
#' @param si a data frame containing the SNP information.  Must contain fields given in '.snpNames' and '.aggregateBy'. 
#' @param snpNames The field of si where the SNP identifiers are found. Default is 'Name'
#' @param aggregateBy The field of si on which the results were aggregated. Default is 'gene'. For single snps which are intended only for single variant analyses, it is reccomended that they have a unique identifier in this field.
#' @param chr (optional) The field of si where the chromosome identifiers are found. See Details.
#' @param pos (optional) The field of si where the snp position identifiers are found. See Details.
#' @param ref (optional) The field of si where the reference allele for the snps is found. See Details.
#' @param alt (optional) The field of si where the alternate allele for the snps is found. See Details.
#' @param filterBy (optional) The (typically logical) field in si in which to keep in further analysis. See Details
#' @param filterFun (optional) a function to apply to multiple '.filterBy' fields via \code{Reduce}. See Details.
#' @param otherCols (optional) other fields of si to propagate through. See Details.
#' 
#' @details It is HIGHLY recommended that \code{.chr}, \code{.pos}, \code{.ref}, and \code{.alt} be supplied.  For
#' 
#' \code{.chr} and \code{.pos} are used for branching and output in subsequent
#' analysis.  Useful for analysis where the X chromosome is handled differently
#' than other chromosomes.
#' 
#' \code{ref} and \code{alt} are used in calculating various SNP and subject
#' summary statistics in 'genotype' objects.
#' 
#' \code{.filterBy} and \code{.filterFun} are used to create filters, typically 
#' at the meta-analysis stage (e.g., because they are intronic or common). 
#' \code{TRUE} indicates that the SNP should be kept.  If \code{.filterFun} is
#' specified \code{Reduce} will be applied to the columns specified by
#' \code{.filterBy}.  Note \code{Reduce} is called the result must be a logical
#' vector for each entry in \code{si}. If \code{.filterFun} produces
#' \code{NA}'s, these will be changed to \code{FALSE} with a warning.
#' 
#' \code{.otherCols} 
#' 
#' @seealso \code{Reduce}
#'   
#' @return an object of class 'snpinfo'.  This is a list typicaly used to feed
#'   into an analysis function.
#'   
#'   An object of class 'snpinfo' is a list containing at least the following
#'   components:
#'   
#' \itemize{
#'  \item data containing the SNP information
#'  \item snpNames column name containing the names of SNPs
#'  \item aggregateBy column name which results are to be aggregated
#'  \item chrCol column name containing the chromosome a SNP is found at. 
#'  \item posCol column name containing the position in a chromosome a SNP is found at. 
#'  \item refCol column name containing the reference allele.
#'  \item altCol column name containing the alternate allele.   
#' }
#' 
#' It is important to note that return \code{include} and \code{exclude} are taken from data and not from the input parameters. 
#'   
#' @export
snpinfo <- function(data, snpNames="Name", aggregateBy="gene", chr=NULL, pos=NULL, ref=NULL, alt=NULL, filterBy=NULL, filterFun=NULL, otherCols=NULL) {
  
  if (is.data.frame(data)) {
    old_class <- class(data)
    old_attributes <- attributes(data)
  } else {
    stop("data must be a data.frame or a class which extends a data.frame.")
  }
  
  # [TBD] check column names are in data
  
  # [TBD] check snpNames
  #        - check it is in data
  #        - check it is of type character
  
  # [TBD] check aggregateBy
  #        - check it is in data
  #        - check it is of type character
  
  # [TBD] check chr
  #        - check it is in data
  #        - check it is of type character
  #        - what is the x chromosome?
  
  # [TBD] check pos
  #        - check it is in data
  #        - check it is of type integer
  
  # [TBD] check ref
  #        - check it is in data
  #        - check it is of type character
  
  # [TBD] check alt
  #        - check it is in data
  #        - check it is of type character
  
  # [TBD] check ref
  #        - check it is in data
  #        - check it is of type character
  
  # [TBD] check filterBy
  #        - check each column is in data
  #        - check it is of type logical or can be converted to logical
  
  # [TBD] check filterFun
  #        - check it is a function
  
  # [TBD] Create Filter
  #       - check the results are a singular logical vector
  #       - check if there are any missing in "keep"
  #       - warn if NA are set to FALSE
  #       - if all NA stop
  
  # [TBD] Drop columns not specified
  
#   data <- as_data_frame(unique(data.frame(data[, cn, drop=FALSE], .keep=keep)))
   new_class <- class(data)
  
  structure(
    data,
    snpNamesCol = snpNames,
    aggregateByCol = aggregateBy,
    chrCol = chr,
    posCol = pos,
    refCol = ref,
    altCol = alt,
    class = c("snpinfo", new_class)
  )
}

#' @rdname snpinfo
#' @export
is_snpinfo <- function(x) inherits(x, "snpinfo")

# metadata  functions  --------------------------------------------------------

# get functions
#' @export
get_snps.snpinfo <- function(x, ...) unique(x[[attr(x, "snpNamesCol")]])

#' @export
get_aggregateBy <- function(x, ...) unique(x[[attr(x, "aggregateByCol")]])

#' @export
get_chr <- function(x, ...) unique(x[[attr(x, "chrCol")]])

#' @export
get_snpNamesCol <- function(x) attr(x, "snpNamesCol")

#' @export
get_aggregateByCol <- function(x) attr(x, "aggregateByCol")



