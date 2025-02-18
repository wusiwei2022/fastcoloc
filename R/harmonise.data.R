#' To harmonise 2 GWAS summary data
#' 
#' Harmonise 2 GWAS summary data primarily for conventional coloc analysis.
#' By default, the RSID, EA, and NEA will be referred to the first data set.
#' 
#' @export
harmonise.coloc.data = function(data.a, data.b){
  trait.a = unique(data.a$trait)
  trait.b = unique(data.b$trait)
  message(paste0("harmonise ", trait.a, " and ", trait.b))
  ## merge trait 1 and trait 2
  colnames(data.a)[2:length(colnames(data.a))] = paste0(colnames(data.a)[2:length(colnames(data.a))], ".a")
  colnames(data.b)[2:length(colnames(data.b))] = paste0(colnames(data.b)[2:length(colnames(data.b))], ".b")
  coloc.data = inner_join(data.a, data.b, by = c("rsid" = "rsid"))
  ## harmonise the beta in reference to the allele
  ### subset of data where both effect allele and non-effect allele in trait 1 and trait b match 
  coloc.data.1 = coloc.data %>% filter(ea.a == ea.b & nea.a == nea.b)
  coloc.data.1 = coloc.data.1 %>% mutate(ea = ea.a, nea = nea.a, .after = rsid) %>% select(-c("ea.a", "nea.a", "ea.b", "nea.b"))
  ### subset of data where effect allele in trait 1 matches non-effect allele in trait b and non-effect allele in trait 1 matches effect allele in trait 2  
  coloc.data.2 = coloc.data %>% filter(ea.a == nea.b & nea.a == ea.b)
  coloc.data.2 = coloc.data.2 %>% mutate(beta.b = - beta.b, eaf.b = 1-eaf.b)
  coloc.data.2 = coloc.data.2 %>% mutate(ea = ea.a, nea = nea.a, .after = rsid) %>% select(-c("ea.a", "nea.a", "ea.b", "nea.b"))
  ### combine harmonised data
  coloc.data.12 = rbind(coloc.data.1, coloc.data.2)
  ### subset of data where effect allele or non-effect allele are not matched
  coloc.data.3 = coloc.data %>% filter(!{rsid %in% coloc.data.12$rsid})
  ### combine harmonised data
  coloc.data.12 = rbind(coloc.data.1, coloc.data.2)
  ### subset of data where effect allele or non-effect allele are not matched
  coloc.data.3 = coloc.data %>% filter(!{rsid %in% coloc.data.12$rsid})
  if(dim(coloc.data.3)[1] > 0){message(paste0(dim(coloc.data.3)[1], "unharmonised variants are dropped"))}
  ### output
  return(coloc.data.12)
}

#' To harmonise across 3 GWAS summary data
#' 
#' Harmonise across 3 GWAS summary data primarily for multi-trait colocalization analysis.
#' By default, the RSID, EA, and NEA will be referred to the first data set.
#' 
#' @export
harmonise.moloc.data = function(data.a, data.b, data.c){
  trait.a = unique(data.a$trait)
  trait.b = unique(data.b$trait)
  trait.c = unique(data.c$trait)
  message(paste0("harmonise ", trait.a, " ", trait.b, ", and ", trait.c))
  ## harmonise trait.a and trait.b
  coloc.data.ab = harmonise.coloc.data(data.a, data.b)
  ## harmonise trait.a and trait.c
  coloc.data.ac = harmonise.coloc.data(data.a, data.c)
  colnames(coloc.data.ac) = gsub(".b", ".c", colnames(coloc.data.ac))
  ## merge data.ab with data.ac
  coloc.data.abc = inner_join(coloc.data.ab, coloc.data.ac)
  ## output
  return(coloc.data.abc)
}