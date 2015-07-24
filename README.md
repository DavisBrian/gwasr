# gwasr

The aim of **gwasr** is to provide common structures and interface for conducting 
GWAS (Genome-Wide Association Studies).  `gwasr` provides a robust set of tools
to interface with various GWAS testing methods and eliminate (or at least catch 
early) common analysis mistakes.  The main goal of `gwar` is to allow analyst
greater focus on the analysis at hand and reduce focus from the nuances of the
underlying data manipulation.

Fundamentally `gwasr` is a set of tools for a common set of problems.  **split** the
genotypes into individual snps or genes (or gene like structures), **apply** a
test or calculate a metric (T1, SKAT, regression, etc), and **combine** the results
back together.


Effort has been put into making `gwasr` fast and memory efficient.  


## Installation

`gwasr` is currently not on CRAN, but you can get it from github with:

```R
# install.packages("devtools")
devtools::install_github("DavisBrian/gwasr")
```

## Learning gwasr

[TBD] - Vignettes


## Key structures

The key objects in gwasr are:

 * phenotype
 * genotypes
 * snpinfo


## Single Object Verbs

 * summary

## Multi Object Verbs


## Usage / Examples

The following example uses `gwasr` to solve a fairly realistic problem: With phenotype, genotype, and a snpinfo file for each "gene", run a T5 test.

[TBD]


## Future plans


