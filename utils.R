# some util functions

name_search = function(sname, data=full_data){
  found = c()
  for(i in 1:length(data$data_name)){
    if(sname %in% names(data$data[[i]]))
      found = c(found, 
        paste(
          data$data_name[i], 
          " (", nrow(data$data[[i]]), ")", 
          sep=""
        )
      )
  }
  if(length(found) == 0) cat("Not found\n")
  else cat(paste(found, collapse=",  "), "\n")
}


name.idx = function(nm1, nm2){
  # which indexes of nm2 contains names in nm1
  idx = c()
  for(nm in nm1){
    idx = c(idx, which(nm == nm2))
  }
  return(idx)
}


nodupc = function(df, nms, by="subject_sp_id"){
  # return df without columns in `nms`
  # but `by` should be contained
  notin = c()
  for(nm in names(df)){
    if(nm == by || !(nm %in% nms)) notin = c(notin, nm)
  }
  df[notin]
}


## a merge function inner join a list of data frames
## where some columns may be duplicated across df
## return a merged df with no col duplication and a list of
## number indicating which columns are every individual df in 
mymerge = function(dflist, by="subject_sp_id"){
  dfnames = names(dflist)
  df = dflist[[dfnames[1]]]
  col.index = list()
  col.index[[dfnames[1]]] = 1:ncol(df)
  for(i in 2:length(dfnames)){
    nm = dfnames[i]
    df = merge(df, nodupc(dflist[[nm]], colnames(df), by), by=by)
    col.index[[nm]] = name.idx(colnames(dflist[[nm]]), colnames(df))
  }
  list(df=df, col.indx=col.index)
}