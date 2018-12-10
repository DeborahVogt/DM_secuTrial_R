


# construct names of metadata tables from study.options object
.constructmetaname <- function(x){
  paste0(study.options$meta_names[x],
         study.options$file.end,
         ".",
         study.options$extension)
}



# remove project name (mnpXYZ_) from whatever
.removeproj <- function(x){
  e <- grepl("^e", x)
  x <- gsub("mnp[[:alnum:]]{1,}_", "", x)
  x <- gsub("^_", "", x)
  x[e] <- gsub("^e", "e_", x[e])
  x
}

