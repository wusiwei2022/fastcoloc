#' To run a conventional colocalization
#' @export
fast.coloc = function(coloc.data, type.a = c("quant", "cc"), type.b = c("quant", "cc")){
  type.a = match.arg(type.a)
  type.b = match.arg(type.b)
  coloc.result = coloc::coloc.abf(list(snp = coloc.data$rsid, 
                                       beta = coloc.data$beta.a, 
                                       varbeta = (coloc.data$se.a)^2, 
                                       MAF = coloc.data$maf.a, 
                                       N = coloc.data$n.a, 
                                       type = type.a),
                                  list(snp = coloc.data$rsid, 
                                       beta = coloc.data$beta.b, 
                                       varbeta = (coloc.data$se.b)^2, 
                                       MAF = coloc.data$maf.b, 
                                       N = coloc.data$n.b, 
                                       type = type.b))
  return(coloc.result)
}

#' To run multi-trait colocalization
#' @export
fast.moloc = function(moloc.data, type.a = c("quant", "cc"), type.b = c("quant", "cc"), type.c = c("quant", "cc")){
  type.a = match.arg(type.a)
  type.b = match.arg(type.b)
  type.c = match.arg(type.c)
  moloc.result = moloc::moloc_test(list(trait.a = data.frame(SNP = coloc.data.abc$rsid, 
                                                             BETA = coloc.data.abc$beta.a, 
                                                             SE = coloc.data.abc$se.a, 
                                                             MAF = coloc.data.abc$maf.a, 
                                                             N = coloc.data.abc$n.a),
                                        trait.b = data.frame(SNP = coloc.data.abc$rsid, 
                                                             BETA = coloc.data.abc$beta.b, 
                                                             SE = coloc.data.abc$se.b, 
                                                             MAF = coloc.data.abc$maf.b, 
                                                             N = coloc.data.abc$n.b),
                                        trait.c = data.frame(SNP = coloc.data.abc$rsid, 
                                                             BETA = coloc.data.abc$beta.c, 
                                                             SE = coloc.data.abc$se.c, 
                                                             MAF = coloc.data.abc$maf.c, 
                                                             N = coloc.data.abc$n.c))
  )
  return(moloc.result)
}
