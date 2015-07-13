#' @export
Intersect <- function(geno = NULL, pheno = NULL, snpinfo = NULL, ...) {
  
  # get common subjects
  if(!is.null(geno) && !is.null(pheno)) {
    if(!is_genotype(geno)) {
      stop("geno must be a 'genotype' object")
    }
    if(!is_phenotype(pheno)) {
      stop("pheno must be a 'phenotype' object")
    }
    subjects <- intersect(get_subjects(pheno), get_subjects(geno))
    if (length(subjects) == 0L) {
      warning("geno and pheno have no subjects in common.")
    }
  } else {
    subjects <- NULL
  }
  
  # get common snps
  if(!is.null(geno) && !is.null(snpinfo)) {
    if(!is_genotype(geno)) {
      stop("geno must be a 'genotype' object")
    }
    if(!is_snpinfo(snpinfo)) {
      stop("snpinfo must be a 'snpinfo' object")
    }  
    snps <- intersect(get_snps(geno), get_snps(snpinfo))
    if (length(snps) == 0L) {
      warning("geno and snpinfo have no snps in common.")
    }
  } else {
    snps <- NULL
  }
  
  # return them
  if (length(subjects) == 0L && length(snps) == 0L) {
    stop("No subjects or snps in common")
  } 
  list(subjects = subjects, snps = snps) 
}
  
#' @export
reduce_data <- function(P, G, S, silevel = "aggregateBy") {
  common_data <- Intersect(geno = G, pheno = P, snpinfo = S)
  
  # [ TBD ] - Check silevel 
  #           - is length 1
  #           - is character
  #           - is a valid option
  
  if (silevel == "aggregateBy") {
    si_col <- attr(S, "aggregateByCol")
  } else if (silevel == "chr") {
    si_col <- attr(S, "chrCol")
  } else {
    stop("Unknown silevel")
  }
  
  keep_set <- unique(S[get_snps(S) %in% common_data$snps, si_col])
  idx <- which(S[ , si_col] %in% keep_set)
  if (length(idx) == 0L) {
    stop("No data in snpinfo subset")
  }
  
  list(subjects = common_data$subjects, 
       snps = common_data$snps,
       snpinfo_idx = idx)
  
}
  