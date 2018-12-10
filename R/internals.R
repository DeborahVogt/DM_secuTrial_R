


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



# load specified metadata table
.load.meta.table <- function(metatable){
  if(.available(metatable)){
    if(study.options$is.zip){
      con <- unz(study.options$data.dir,
                 .constructmetaname(metatable))
      tmp <- read.table(con,
                        sep = study.options$sep,
                        na.strings = study.options$na.strings,
                        header = TRUE)
      close(con)

    } else {
      tmp <- read.table(file.path(study.options$data.dir,
                                  .constructmetaname(metatable)),
                        sep = study.options$sep,
                        na.strings = study.options$na.strings,
                        header = TRUE)
    }
    return(tmp)
  } else {
    stop("metadata table unavailable")
  }
}



# query metadata availbility
.available <- function(x){
  unlist(study.options$meta_available[x])
}




