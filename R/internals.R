


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
      if(!isOpen(con)) open(con)
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




# create factors
.factorize <- function(x, ...) UseMethod(".factorize", x)

# data.frame method
.factorize.data.frame <- function(data){
  if(exists("cl", envir = .GlobalEnv)){
    warning("cl found in wd... ")
    if(all(names(cl) == c("column", "code", "value"))){
      warning("format appears to be suitable")
    } else {
      warning("format appears to be incorrect... reloading")
      cl <- .load.meta.table("cl")
    }
  } else {
    cl <- .load.meta.table("cl")
  }
  if(!is.character(cl$column)) cl$column <- as.character(cl$column)

  str <- strsplit(cl$column, ".", fixed = TRUE)
  str <- sapply(str, function(x) x[2])
  cl$var <- str

  for(i in names(data)[names(data) %in% cl$var]){
    lookup <- cl[which(cl$var == i), c("code", "value")]
    data[, paste0(i, ".factor")] <- .factorize(data[, i], lookup)
  }
  return(data)
}

# integer method
.factorize.integer <- function(data, lookup){
  factor(data, lookup$code, lookup$value)
}


