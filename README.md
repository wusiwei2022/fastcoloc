# fastcoloc
Pacakge fastcoloc is built to streamline the colocoalization and multi-trait colocalization analysis.<br>  

## Background
Colocalization is one of the most important methods for analyzing GWAS summary data. The analysis can be run with coloc and moloc R package.
However, both coloc and moloc package requires researchers to pre-format their GWAS summary data set.
We build up this tool to faciliate the preprocessing and preformatting of the GWAS summary data for coloc/moloc analysis.

## Usage
```{r}
if(!require(devtools)){install.packages("devtools")}
devtools::install_github("wusiwei2022/fastcoloc")
```

## To do
Current fastcoloc::fast.coloc function was build on coloc::coloc_test function and therefore only accept beta and se or p, maf, and n as input to calculate Bayesian factor.
In the future, fast.coloc will allow users to calculate Bayesian factor from Z statistics directly.<br>
Function statement, example data, and example codes will also be made available soon.<br>
Regional genomic plots tailed for colocalization analysis will be available in the future updata.<br>

