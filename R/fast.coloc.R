#' @title fast.coloc
#' To run a conventional colocalization using 2 GWAS summary data
#' 
#' @param coloc.data The harmonised GWAS summary data. 
#' @param type.a Type of outcome of the 1st GWAS. "quant" stands for continuous outcome while "cc" stands for case-control design.
#' @param type.b Type of outcome of the 2nd GWAS. "quant" stands for continuous outcome while "cc" stands for case-control design.
#' @return Colocalization result as coloc.abf in package coloc
#' @export
fast.coloc = function(coloc.data, type.a = c("quant", "cc"), type.b = c("quant", "cc"), sdY.a = NULL, sdY.b = NULL){
  type.a = match.arg(type.a)
  type.b = match.arg(type.b)
  # A Bayes Factor for each SNP and each trait 1 and 2 should be computed using the Wakerfield Approximate Bayes Factor method. 
  # Bayes factor can be calculated directly from beta and se if beta and se are available for each SNP in GWAS summary.
  # If beta and se are not provided, se can be estimated with maf and n for continuous outcomes. P value can be used instead of beta and se to calculate Z statistics.
  # If beta and se are not provided, se of continuous outcome can be estimated with maf and n while se of binary outcome can be estimated with maf, n, and proportion of samples that are cases. 
  # Standard deviation for prior Beta ~ N(0, W) is required to calculate Bayes factor.
  # W is set 0.2 for binary outcomes (i.e. case control design) and 0.15 for quantitative outcomes.
  # However, when bayes factors are calculated from beta and se and the outcome is quantitative, W is adjusted with sdY.
  # sdY can be provided if know. It can also be inferred with n, maf, and var(beta). Otherwise, sdY is set 1 by default because most GWAS normalized the quantative outcome before model fitting.
  # Format the data structure for the 1st GWAS
  if("beta.a" %in% names(coloc.data) & "se.a" %in% names(coloc.data)){
    if(type.a == "cc"){coloc.data.a = list(snp = coloc.data$rsid, beta = coloc.data$beta.a, varbeta = (coloc.data$se.a)^2, type = type.a)}
    if(type.a == "quant"){coloc.data.a = list(snp = coloc.data$rsid, beta = coloc.data$beta.a, varbeta = (coloc.data$se.a)^2, type = type.a, sdY = ifelse(is.null(sdY.a), 1, sdY.a))}
  }else if(type.a == "quant" & "p.a" %in% names(coloc.data) & "maf.a" %in% names(coloc.data) & "n.a" %in% names(coloc.data)){
    coloc.data.a = list(snp = coloc.data$rsid, pvalues = coloc.data$p.a, MAF = coloc.data$maf.a, N = coloc.data$n.a, type = type.a)
  }else if(type.a == "cc" & "p.a" %in% names(coloc.data) & "maf.a" %in% names(coloc.data) & "n.a" %in% names(coloc.data) & "ncase.a" %in% names(coloc.data)){
    coloc.data.a = list(snp = coloc.data$rsid, pvalues = coloc.data$p.a, MAF = coloc.data$maf.a, N = coloc.data$n.a, s = coloc.data$ncase.a/coloc.data$n.a, type = type.a)
  }else{
    stop("Please provide beta.a and se.a for the 1st gwas. Alternatively, please provide p.a, maf.a, and n.a for quantative traits and ncase.a more for case-control trait")}
  
  # Format the data structure for the 2nd gwas
  if("beta.b" %in% names(coloc.data) & "se.b" %in% names(coloc.data)){
    coloc.data.b = list(snp = coloc.data$rsid, beta = coloc.data$beta.b, varbeta = (coloc.data$se.b)^2, type = type.b)
  }else if(type.b == "quant" & "p.b" %in% names(coloc.data) & "maf.b" %in% names(coloc.data) & "n.b" %in% names(coloc.data)){
    coloc.data.b = list(snp = coloc.data$rsid, pvalues = coloc.data$p.b, MAF = coloc.data$maf.b, N = coloc.data$n.b, type = type.b)
  }else if(type.b == "cc" & "p.b" %in% names(coloc.data) & "maf.b" %in% names(coloc.data) & "n.b" %in% names(coloc.data) & "ncase.b" %in% names(coloc.data)){
    coloc.data.b = list(snp = coloc.data$rsid, pvalues = coloc.data$p.b, MAF = coloc.data$maf.b, N = coloc.data$n.b, s = coloc.data$ncase.b/coloc.data$n.b, type = type.b)
  }else{
    stop("Please provide beta and se in the 2nd gwas summary. Alternatively, please provide p, maf, and n for quantative traits and ncase more for case-control trait")}
  
  # Run colocalization
  coloc.result = coloc::coloc.abf(coloc.data.a, coloc.data.b)
  return(coloc.result)
}