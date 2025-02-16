#' Fast extract a subset of GWAS data
#' 
#' Fast extract a subset of the GWAS summary data set in reference to the ID provided.
#' The data set should be a .txt file separated by tab
#' 
#' @param ref.id A vector of RSID to match in the GWAS summary data
#' @param name Path to the GWAS summary to extract
#' @param data.id.index Column number of RSID in the GWAS summary data  
#' @returns A data frame that contains a subset of GWAS summary data
#' @export extract.data
extract.data = function(ref.id, data.path, data.id.index = 1){
  ### Check if all the IDs are RSIDs
  if(all(startsWith(ref.id, "rs"))){message("all the IDs are rsid")}else{warning("Not all the IDs are rsid")}
  ### Create a temporary file to save the IDs to match
  fn = tempfile()
  write.table(data.frame(ref.id), file=fn, row.names=FALSE, col.names=FALSE, quote=FALSE)
  ### Extract data matched to the RSID
  data = read.table(pipe(paste0("awk 'NR==FNR {AssoArray[$1]=$1; next} $",data.id.index, " in AssoArray {print $0}' ", fn, " ", data.path)))
  data.header = read.table(pipe(paste0("head -n1 ", data.path)), header = TRUE)
  colnames(data) = colnames(data.header)
  return(data)
}