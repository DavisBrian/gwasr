#' @export
gwas_list <- function(geno, pheno, snpi) {
    cmn <- Intersect(geno = geno, pheno = pheno, snpinfo = snpi)
    
    data <-     list(geno = reduce(geno, cmn), pheno = reduce(pheno, cmn), snpinfo = reduce(snpi, cmn, get_chrcol(snpi)))
    structure(
      data,
      subjects = cmn$subjects,
      snps = cmn$snps,
      class = c("gwas_list")
    )
}

