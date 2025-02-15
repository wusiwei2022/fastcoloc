#' @export
# fast extract a subset of the dataset in reference to the ID provided
# the dataset to be extracted need to be a .txt file seperated by tab
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