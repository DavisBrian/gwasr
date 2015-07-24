# phenotype

#' @title Create a phenotype object
#'   
#' @description This function creates a phenotype object.
#' 
#' @param data a data frame (or object coercible by as.data.frame to a data 
#'   frame) containing the variables in the model.
#' @param formula an object of class "formula" (or one that can be coerced to 
#'   that class): a symbolic description of the model to be fitted.The details 
#'   of model specification are given under 'Details' in the "lm" help file.
#' @param id (optional) column name identifier of the unique subjects in
#'   \code{data}.  If given the phenotype sample ids will become the rownames
#'   of \code{data}. If \code{id} is \code{NULL} the suject id's are assumed
#'   to be the rownames of \code{data}.
#' @param gender (optional) column name identifier for the gender in \code{data}
#' @param include (optional) character vector of the subjects in \code{id} to be
#'   included in the analysis.  See Details.
#' @param exclude (optional) character vector of the subjects in \code{id} to be
#'   excluded in the analysis.  See Details.
#' @param reduce logical.  Should the dataset be reduced to only columns used in
#' formula.  See Details
#'   
#' @details \code{data} and \code{formula} are similar to what is needed for
#'   \code{lm}.  If a formula is not specifed, no further analysis will run.
#'   
#'   If the \code{id} is not specified it is assumed that the rownames in
#'   \code{data} are the unique subject identifier.  If \code{id} is specifed
#'   then \code{rownames(data)} will be set to \code{id}.  Thus the subject id's
#'   must be unique and without duplication.
#'   
#'   If the gender column is not specified it will be set to \code{NULL}. 
#'   
#'   Typically either \code{include} or \code{exclude}, or both, is set to
#'   \code{NULL}. It is important to note that return \code{include} and
#'   \code{exclude} are taken from the subjects in \code{data} and not from the input parameters.
#'   [TBD: explain include/exclude]
#'   
#'   If \code{reduce == TRUE} then \code{data} is reduced to a data.frame
#'   containing the variables used in \code{formula} plus \code{gender}. See
#'   get_all_vars for specifics.
#'   
#'  @seealso get_all_vars 
#'   
#' @return an object of class 'phenotype'.  This is a list typicaly used to feed
#'   into an analysis function.
#'   
#'   An object of class 'phenotype' is a list containing at least the following
#'   components:
#'   
#' \itemize{
#'  \item data data frame of the data for analysis 
#'  \item formula formula to be used in further analysis  
#'  \item gender column name containing the gender of the subjects
#'  \item include the subjects in \code{data} that will be included is further analysis.
#'  \item exclude the subjects in \code{data} that will be excluded is further analysis.    
#' }
#' 
#' It is important to note that return \code{include} and \code{exclude} are taken from data and not from the input parameters. 
#'   
#' @export
#
# [TBD]
#  -verbose option?
#  -add genderChar??? something to demote which character is "MALE/FEMALE"
#  -add "family" (gaussian/binomial/survival)
#  -add print method to show the meta data
#  -add "problems" (a la readr)
phenotype <- function(data, formula=NULL, family=NULL, id=NULL, gender=NULL, 
                      include=NULL, exclude=NULL) {
  
  if(is.data.frame(data)) {
    old_class <- class(data)
    old_attribures <- attributes(data)
  } else {
    stop("data must be a data.frame or a class which extends a data.frame.")
  }
  
  # [TBD] check column names are in data
  cols <- colnames(get_all_vars(formula, data))
  
  # [TBD] check formula
  #        - make sure all varaibles are in the data frame
  #        - make sure after na.omit there are rows
  #        - make sure a basic model.frame can be made ???

  # [TBD] check family
  #        - if "gaussian" response is_numeric
  #        - if "binomial" response is_dicotomous
  #        - if "survival" ???
  #        - make sure formula can be run with the family argument
  
  # [TBD] check check id
  #        - if not given make the rownames a column (.id) and set idCol
  #        - make sure they are characters
  #        - make sure they are unique
  #        - set idCol
  cols <- unique(c(cols, id))
  subjects_all <- data[ , id]
  
  # [TBD] check check gender
  #        - is a type than can be grouped
  #        - has no more than 2 groups
  #        - can be converted to TRUE/FALSE??
  cols <- unique(c(cols, gender))
  
  # [TBD] check check include
  #        - the subjects are in the dataset

  # [TBD] check check exclude
  #        - the subjects are in the dataset
  
  # include / exclude    

  if (!is.null(include)) {
    subjects <- intersect(include, data[ , id])
    data <- data[(data[, id] %in% subjects), , drop = FALSE]
  }
  
  # exclude
  if (!is.null(exclude)) {
    subjects <- setdiff(data[ , id], exclude)
    data <- data[(data[, id] %in% subjects), , drop = FALSE]
  }
  
  data <- na.omit(data[ , cols])
  
  subjects_include <- data[ , id]
  
  subjects_exclude <- setdiff(subjects_all, subjects_include)
  if (length(subjects_exclude) == 0L) {
    subjects_exclude <- NULL
  }
  
  new_class <- class(data)
  
  structure(
    data,
    formula = formula,
    family = family,
    idCol = id,
    genderCol = gender,
    included = subjects_include,
    excluded = subjects_exclude,
    class = unique(c("phenotype", new_class))
  )
}

#' @rdname phenotype
#' @export
is_phenotype <- function(x) inherits(x, "phenotype")

# get functions  ---------------------------------------------------------------

#' @export
get_subjects.phenotype <- function(x, excluded = FALSE) {
  # [TBD] check exlude is  a logical of length 1
  stopifnot(length(excluded) == 1L)
  if (excluded) {
    attr(x, "excluded")
  } else {
    intersect(x[[attr(x, "idCol")]], attr(x, "included"))
  }
}

#' @export
get_formula <- function(x) { attr(x, "formula") }

#' @export
get_family <- function(x) { attr(x, "family") }

#' @export
get_idCol <- function(x) { attr(x, "idCol") }

#' @export
get_genderCol <- function(x) { attr(x, "genderCol") }

#' @export
get_included <- function(x) { attr(x, "included") }

#' @export
get_excluded <- function(x) { attr(x, "excluded") }


# single verbs -----------------------------------------------------------------

#' @export
reduce.phenotype <- function(p, common) {
  
  dropped <- setdiff(get_subjects(p), common$subjects)
  if (length(dropped) == 0L) {
    attr(p, "dropped") <- NA
  } else {
    attr(p, "dropped") <- dropped
  }
  
  o <- match(common$subjects, p[ , get_idCol(p)])
  p[o, ]
}
