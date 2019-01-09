## Initilise study.options in GlobalEnv
##
## Upon package attachment these two lists are initialised in GlobalEnv.
# .onAttach <- function(libname, pkgname) {
#   load.study.options()
# }



#' Extended version of read.table.
#'
#' This function loads a database table and:
#' \itemize{
#' \item removes trailing empty columns
#' \item identifies and converts date columns (optionally sets "1900-01-01" to NA)
#' \item renames colnames (using \code{\link{new.names}})
#' }
#'
#' @param path location of the csv-file or unz(zip-file, table filename)
#' @param convert.dates flag for conversion of date columns
#' @param convert.unknown.date.to.na flag for conversion of unknown dates
#' @param rename.headers flag for renaming of headers
#' @param add.pat.id adding study ID pat.id to the first column
#' @param silent hide verbose output
#' @param ... other options passed to \code{read.table()}
#' @return Data frame of the table.
#' @export
#' @seealso load.study.options, new.names, convert.all.dates, unz
read.DB.table <- function(path, convert.dates=FALSE, convert.unknown.date.to.na=FALSE, rename.headers=FALSE, add.pat.id=TRUE, add.center=TRUE, silent=FALSE, ...) {
  study.options <- get("study.options") # declare variable since defined in dossier lib

  ## assert that "study.options" exist
  if(!exists("study.options")) stop("The list 'study.options' must be defined.")

  ## file (not in zip)
  if(is.character(path)) {
    if(!file.exists(path)) {
      warning(paste0(path, " does not exist. Skipped!", immediate.=TRUE))
      return(NULL)
    } else {
      tab <- read.table(path,
                        header=TRUE,
                        na.strings = study.options$na.strings, # fill all empty cells
                        sep=study.options$sep,
                        fill=TRUE) ## if values missing in last row
    }
  } else { # for zip connections
    tryCatch({
      tab <- read.table(path,
                        header=TRUE,
                        na.strings = study.options$na.strings, # fill all empty cells
                        sep=study.options$sep,
                        fill=TRUE)
    }, error = function(e) {
      print(paste0("File (", path, ") does not exist: ",e))
      return(NULL)
    }
    , finally = {
    }
    )
  }

  ## in earlier secuTrial exports there was a last/empty column "X" -> remove it
  if("X" %in% names(tab)) {
    tab <- tab[,-ncol(tab)]
  }
  if(rename.headers) names(tab) <- new.names(tab)
  if(add.pat.id & "mnppid" %in% names(tab)) {
     ## In order to be able to translate mnppid to mnpaid the casenode table is required
     ## This table is loaded first to enable the translations of the other table
     ## The casenode table or any other table that already has an mnpaid must not pass add.pat.id()
     ## (otehrwise this whould throw an error since the "patient"=casenode table is missing)
     if (!"mnpaid" %in% names(tab)) tab <- add.pat.id(tab)
     else {
       tab$pat.id <- tab$mnpaid
       tab <- move.column.after(tab, "pat.id", "first")
     }
  }
  if(add.center & "mnppid" %in% names(tab))  {
     if (!("mnpaid" %in% names(tab))) tab <- add.center(tab)
     else if ("mnpctrname" %in% names(tab))  {
       ## Since the introduction of the flag "Duplicate form meta data into all tables"
       ## The center-metadate-id is missing in some tables
       stopifnot("mnpctrname" %in% names(tab))
       tab$center <- as.factor(tab$mnpctrname)
       tab <- move.column.after(tab, "center", "pat.id")
       tab$center <- as.factor(unlist(lapply(tab$center, remove.center.tag)))
     }
  }
  if(convert.dates) {
    ## iterate of multiple date types
    for(date.format in study.options$date.format) {
    tab <- convert.all.dates(tab, date.format, convert.unknown.date.to.na, unknown.date.string=study.options$unknown.date.string, partial.date.handling=study.options$partial.date.handling, partial.date.string=study.options$partial.date.string, silent)
  }
  }
  return(tab)
}

# STUDY OPTIONS ----

#' List specifying the general properties of all tables in the export.
#'
#' The list \code{study.options} stores all relevant technical information
#' in order to be able to correctly load the tables of a given study.
#' The list is used in the function \code{read.DB.table}.
#'
#' @param data.dir location of the export directory (or zip file) containing the csv/xls files
#'
#' @details \code{partial.date.handling}: By default, potential data columns containing entries that cannot be converted are
#' skipped (option 'skip'). With 'force.conversion' fields incompatible with the date format are converted to NA.
#' 'fill.partial.dates' keeps the orginal column untouched and adds a new column to the data frame with the name <colname>.partial.dates.processed
#' in which partial are filled using \code{\link{fill.partial.date}} (e.g. Unknown.01.2013 -> 15.01.2013).
#' With 'fill.partial.dates.and.keep.original' partial dates are processed directly and the original data is copied to <colname>.original.
#' @value
#' TODO: add details of the returned object
#' @export
#' @seealso read.DB.table, convert.all.dates

load.study.options <- function(data.dir) {

  is.zip <- grepl(".zip$", data.dir)

  # shortnames
  if(is.zip){
    files <- unzip(data.dir, list=TRUE)
    w <- grepl("ExportOptions", files$Name)
    con <- unz(data.dir, files$Name[w])
    parsed.export <- readLines(con)
    close(con)
  } else {
    files <- data.frame(Names = list.files(data.dir))
    w <- grepl("ExportOptions", files$Name)
    parsed.export <- readLines(file.path(data.dir, files$Name[w]))
  }

  version <- parsed.export[max(grep("secuTrial", parsed.export))]
  version <- unlist(regmatches(version, gregexpr("[[:digit:]]\\.[[:digit:]]\\.[[:digit:]]\\.[[:digit:]]", version)))
  # short names
  shortnames <- any(grepl("Shorten", parsed.export))
  # TODO : German for shorten?
  # rectangular data
  rt <- any(grepl("[rR]ect", parsed.export))


  # metadata file names ----
  meta_names <- list()
  if(shortnames){
    meta_names$forms <- "fs"
    meta_names$casenodes <- "cn"
    meta_names$centres <- "ctr"
    meta_names$items <- "is"
    meta_names$questions <- "qs"
    meta_names$visitplan <- "vp"
    meta_names$visitplanforms <- "vpfs"
  } else {
    meta_names$forms <- "forms"
    meta_names$casenodes <- "casenodes"
    meta_names$centres <- "centres"
    meta_names$items <- "items"
    meta_names$questions <- "questions"
    meta_names$visitplan <- "visitplan"
    meta_names$visitplanforms <- "visitplanforms"
  }
  meta_names$cl <- "cl"

    # end of file name and extention
  end <- gsub("ExportOptions|.html", "", files$Name[w])
  if(rt & shortnames){
    Y <- paste("^", meta_names, collapse = "|", sep = "")
    X <- files$Name[grepl(Y, files$Name)][1]
    X <- gsub(".xls", "", X)
    X <- gsub(Y, "", X)
    end <- X
    rm(X, Y)
  }

  ext <- unique(sapply(strsplit(files$Name[-w], ".", fixed = TRUE), function(x) x[2]))
  ext <- ext[ext != "html"]

  # metadata availability ----
  .constructmetaname <- function(x){
    paste0(meta_names[x],
           end,
           ".",
           ext)
  }
  meta_available <- list()
  meta_available$forms <- .constructmetaname("forms") %in% files$Name
  meta_available$casenodes <- .constructmetaname("casenodes") %in% files$Name
  meta_available$centres <- .constructmetaname("centres") %in% files$Name
  meta_available$items <- .constructmetaname("items") %in% files$Name
  meta_available$questions <- .constructmetaname("questions") %in% files$Name
  meta_available$visitplan <- .constructmetaname("visitplan") %in% files$Name
  meta_available$visitplanforms <- .constructmetaname("visitplanforms") %in% files$Name
  meta_available$cl <- .constructmetaname("cl") %in% files$Name


  # sep ----
  if(is.zip){
    con <- unz(data.dir, files$Name[!grepl("html$", files$Name)][1])
    line1 <- readLines(con, 1)
    close(con)
  }
  if(!is.zip) line1 <- readLines(file.path(data.dir, files$Name[!grepl("html$", files$Name)][1]), 1)
  if (grepl(",", line1)) {
    sep <- ","
  } else if (grepl("'", line1)) {
    sep <- "'"
  } else if (grepl(";", line1)) {
    sep <- ";"
  } else if (grepl("\\t", line1)) {
    sep <- "\t"
  } else if (grepl("@", line1)) {
    sep <- "@"
  } else {
    stop("Unknown Field Separator")
    return(NULL)
  }

  # NA strings
  na.strings <- c("NA","")
  # TODO : custom formats? parsed from ExportOptions?

  # dates ----
  # date format
  date.format <- c("%Y%m%d", "%Y-%m-%d")
  # TODO : custom formats? parsed from ExportOptions?

  # unknown date strings
  unknown.date.string <- NULL
  # TODO : custom formats? parsed from ExportOptions?

  # partial dates
  partial.date.string <- ""
  partial.date.handling <- "fill.partial.dates.and.keep.original"
  # TODO : parsed from ExportOptions?

  # IDs
  # TODO : parsed from ExportOptions?

  # filenames
  datafiles <- files$Name[!grepl(".html$", files$Name)]
  datanames <- .removeproj(datafiles)
  datanames <- gsub(end, "", datanames)
  datanames <- gsub(paste0("\\.", ext), "", datanames)
  names(datanames) <- datafiles
  if ("ctr" %in% datanames) {
    w <- which(datanames == "ctr")
    datanames[w] <- "center"
  }
  if (any(c("cn", "casenodes") %in% datanames)) {
    w <- which(datanames %in% c("cn", "casenodes"))
    datanames[w] <- "patient"
  }

  # return object ----
  study.options <- list(sep=sep,
                        date.format = date.format,
                        na.strings = na.strings, # if blanks mean missing
                        unknown.date.string = unknown.date.string, # incomplete dates
                        partial.date.string = partial.date.string,
                        partial.date.handling = partial.date.handling,
                        shortnames = shortnames,
                        is.zip = is.zip,
                        is.rectangular = rt,
                        meta_names = meta_names,
                        meta_available = meta_available,
                        files = files$Name,
                        data.files = datafiles,
                        data.names = datanames,
                        file.end = end,
                        extension = ext,
                        data.dir = data.dir,
                        secuTrial.version = version)
  class(study.options) <- "secutrialoptions"
  assign("study.options", study.options, envir = .GlobalEnv)
  # return(NULL)
}

## -----------------------------------------------fill.partial.dates.and.keep.original-----------------------

#' Load tables from an export and distinguish between rectangular and non-rectangular input.
#'
#' If \code{is.rt} is TRUE then only the \code{decode.rt.visitlabels} and \code{silent} parameters are used.
#' If \code{is.rt} is FALSE then all other parameters including \code{silent} are interpreted.
#' The argument \code{tables} can handle four distinguished cases:
#' \itemize{
#' \item tables=NULL: there is variable called 'table.list' e.g. created from dossier-specific
#' package and all tables should be loaded. Definition:
#' table.list <- list(data.frame.name=list(filename=<filename in the export>,
#' tablename=<internal table name>), ...). The latter is only relevant for queries.
#' \item tables=c(tablename1,tablename2,...): there is a 'table.list', but only some
#' tables should be loaded.
#' \item tables=c(filename1,filename2,...): the user specifies the files that should be loaded.
#' \item tables="all" loads all files in the zip-archive or directory.
#' }
#' The optional pre-processing steps are: identification of date columns
#' and convertion to objects of the class 'Date', conversion of unknown dates to NA and
#' renaming of column names (remove '_' and capital letters, see \code{\link{new.names}}). The study specific date formats and unknown date strings
#' are specified in \code{load.study.options}.
#'
#' @param data.dir location of the export directory (or zip file) containing the csv/xls files
#' @param tables vector of tables to be loaded (if a \code{table.list} exists, the corresponing table names can be given)
#' @param convert.dates identify and convert date columns
#' @param convert.unknown.date.to.na convert unknown date strings (e.g 1900-01-01) to NA
#' @param rename.headers rename column names
#' @param add.pat.id adding study ID pat.id to the first column
#' @param add.center adding center information
#' @param silent hide output
#' @param is.rt specifies if export is in rectangular table format
#' @param decode.rt.visitlabels specifies whether to decode the visit labels in the rectangular table
#' @examples
#' ## non rectangular table
#' load.tables(data.dir=system.file("extdata", "s_export_CSV-xls_DEM00_20180912-125720.zip", package = "secuTrial"))
#' ## rectangular table
#' load.tables(system.file("extdata", "s_export_rt-CSV-xls_DEM00_20181016-151332.zip", package = "secuTrial"),
#'             is.rt = TRUE, decode.rt.visitlabels = TRUE)
#' @export
#' @seealso read.DB.table, load.table.list (used in dossier-specific packages), load.study.options
#' @references http://stackoverflow.com/questions/3640925/global-variable-in-r-function
#' @return (Pre-processed) tables in \code{tables} as data frames
load.tables <- function(data.dir,
                             tables="all",
                             convert.dates=FALSE,
                             convert.unknown.date.to.na=FALSE,
                             rename.headers=FALSE,
                             add.pat.id=TRUE,
                             add.center=FALSE,
                             silent=FALSE,
                             is.rt=FALSE,
                             decode.rt.visitlabels=TRUE) {
  ## first check that the file/path exists (may be empty if Sys.glob() was used)
  if(length(data.dir) == 0) {
        stop("Export location not specified.")
        return(NULL)
  }
  if(!file.exists(data.dir)) {
        stop(paste0("File '", data.dir,"' does not exist."))
        return(NULL)
  }

  load.study.options(data.dir)

  ## handle loading from zip
  is.zip <- study.options$is.zip

  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
  ## Check if neccessary items are included in export  ##
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

  ## Load ExportOptions.html
  path.or.zip <- Sys.glob(paste0(data.dir,"/ExportOptions*"))
  if (is.zip) {
      if (!any(grepl("ExportOptions", unzip(data.dir, list=TRUE)$Name))) {
          stop("ExportOptions html-file not found in secuTrial export!")
          return(NULL)
      } else {
          fn <- unzip(data.dir, list=TRUE)$Name[grepl("ExportOptions", unzip(data.dir, list=TRUE)$Name)]
          if(fn != "ExportOptions.html") warning("The export option 'shorten table names' has not been enables which results in large data.frame names.")
          path.or.zip <- unz(data.dir, fn)
      }
  } else {
      if(!file.exists(path.or.zip)) {
          stop("ExportOptions html-file not found in secuTrial export!")
          return(NULL)
      }
  }

  ## rectangular input ----
  if(study.options$is.rectangular) {
    close(path.or.zip)
<<<<<<< HEAD
=======

>>>>>>> 82063724c85820f2ccaea3d178d761f98030c64e
    files_in_zip <- study.options$data.files

    rtdata_con <- unz(data.dir, filename=files_in_zip[grep("data", files_in_zip)])
    if(!isOpen(rtdata_con)) open(rtdata_con)
    ## rtdata_internal
<<<<<<< HEAD
=======

>>>>>>> 82063724c85820f2ccaea3d178d761f98030c64e
    rtdata_internal <- read.csv(file=rtdata_con, header=T, sep="\t")
    close(rtdata_con)

    if (decode.rt.visitlabels) {
      # vp_con <- unz(data.dir, filename=files_in_zip$Name[grep("vp",files_in_zip$Name)])
      # vp <- read.csv(file=vp_con, header=T, sep="\t")
      vp <- .load.meta.table("visitplan")
<<<<<<< HEAD
=======

>>>>>>> 82063724c85820f2ccaea3d178d761f98030c64e
      ## clean out spaces and other common disturbing characters
      vp$mnpvislabel <- gsub("\\s+", "_", vp$mnpvislabel)
      vp$mnpvislabel <- gsub("\\.", "", vp$mnpvislabel)
      vp$mnpvislabel <- gsub("\\|", "", vp$mnpvislabel)
      vp$mnpvislabel <- gsub("#", "", vp$mnpvislabel)
      ## decoding
      for(i in 1:length(vp$mnpvislabel)) {
        names(rtdata_internal) <- gsub(x = names(rtdata_internal),
                                       pattern = vp$mnpvisid[i],
                                       replacement = vp$mnpvislabel[i])
      }
      ## remove leading v
      #names(rtdata_internal) <- gsub(x=names(rtdata_internal), pattern="^v", replacement="")
    }
    ## rtdata is a global variable
    if (!silent) {
      cat("--- rectangular data written into variable rtdata ---\n")
    }
    rtdata <<- rtdata_internal
  } else { ## non rectangular input

    parsed.export <- readLines(path.or.zip)
    if (is.zip) close(path.or.zip)

    ## Make sure that ExportOptions.html uses english
    ## TODO: Support German customer area!
    ##if(!silent) cat("** Checking Language of ExportOptions.html\n")
    ##if (length(grep("Created on",parsed.export))==0) {
    ##    warning("ExportOptions.html is written in other language than English. Automatic reading of csv separator might not be possible... In case you run into trouble, please contact a Data Manager to set the Customer area to English.\n", immediate.=TRUE)
    ##} else if (!silent) {
    ##   cat("** ExportOptions.html is in English\n")
    ##}

    ## Make sure that column names are included in Export!
    if(!silent) cat("** Checking for 'Column names' in ExportOptions.html'\n")
    if (length(grep("Column names",parsed.export))==0 &
        length(grep("Spaltennamen",parsed.export))==0) {
        stop("The secuTrial export does not include 'Column names'")
        return(NULL)
    } else if (!silent) {
        cat("** 'Column names' ('Spaltennamen') was found in ExportOptions.html\n")
    }

    ## Make sure that Add-ID/Zus-ID is included in export
    if (add.pat.id) {
        if(!silent) cat("** Checking for 'Add-ID' in ExportOptions.html\n")
        if (length(grep("Add-ID", parsed.export))==0 &
            length(grep("Zus-ID", parsed.export))==0 &
            length(grep("Patient-ID",parsed.export))==0)  {
            stop("The secuTrial export does not include 'Add-ID'")
            return(NULL)
        } else if (!silent) {
            cat("** 'Add-ID' ('Zus-ID', 'Patient-ID') was found in ExportOptions.html\n")
        }
    }



    ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
    ## If tables = NULL Load tables from table.list in dossier.lib  ----
    ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

    if(is.null(tables)) {
        if(!silent) cat("** Loading tables with 'table.list' (probably defined in dossier library\n")
        tables <- names(table.list)
        ## ensure that patient and center table are loaded first
        ## (needed to add pat.id and center to all tables)
        if(add.pat.id & add.center) {
            tables <- c("patient", "center", tables)
            tables <- tables[!duplicated(tables)]
        }
        if(add.pat.id & !add.center) {
            tables <- c("patient", tables)
            tables <- tables[!duplicated(tables)]
        }
        for(t in tables) {
            table.filename <- eval(parse(text = paste("table.list$", t , "$filename", sep="")))
            if(!silent) cat("--- table", table.filename, "loaded as",t,"---\n")
            path.or.zip <- file.path(data.dir, table.filename)
            if(is.zip) {
                path.or.zip <- unz(data.dir, table.filename)
            }
        assign(t, read.DB.table(path.or.zip,
                                convert.dates,
                                convert.unknown.date.to.na,
                                rename.headers,
                                add.pat.id,
                                add.center,
                                silent),
               envir = .GlobalEnv)
        }
    } else if (tables[1]=="all") {
        ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
        ## IF tables = TRUE Load all tables in data.dir  ----
        ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
        if(!silent) cat(paste0("** Loading all tables from ",data.dir,"\n"))
        ## Throw warning if table.list exists
        if(!silent) cat("** Ensuring that no 'table.list' was set by user\n")
        if (exists("table.list")) {
            warning("previously defined 'table.list' (possibly from dossier library) was written over!\n")
            remove("table.list", envir = .GlobalEnv)
                  if(!silent) cat("--- Deleting previous 'table.list'\n")
        } else {
            if(!silent) cat("** No 'table.list' found\n")
        }
        # Get the names of the table.list
        if(!silent) cat("** Building the 'table.list'\n")
        table.list <- study.options$data.files
        assign("table.list", table.list, envir=.GlobalEnv)
        if(!silent) cat(paste0("*** ",length(table.list)," tables were found\n"))
        if(!silent) cat("** Calling load.tables(data.dir, tables = table.list, ...)\n")
        load.tables(data.dir,
                    table.list,
                    convert.dates,
                    convert.unknown.date.to.na,
                    rename.headers,
                    add.pat.id,
                    add.center,
                    silent)
    } else {
        ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
        ## ELSE Load tables from input list 'tables'  ----
        ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
        if(!silent) cat("** Loading tables as defined in input tables = ... \n")
        ## ensure that patient and center table are loaded first
        ## (needed to add pat.id and center to all tables)
        ## Add xls or csv version of patient and center tables
        if(add.pat.id){
                tables <- c(.constructmetaname("casenodes"), tables)
        }
        if(add.center){
          tables <- c(.constructmetaname("centres"), tables)
        }
        if(!study.options$extension %in% c("csv", "xls")) {
            stop("ExportOptions.html does not include information on export Format (.xls or .csv)")
            return(NULL)
        }



      for(t in tables) {
            table.filename <- t
            ## For userfriendlieness, strip common endings like .xls or .csv
            # t <- gsub(paste0("\\.", study.options$extension), "", t)
            ## Backwards compatibility: If a list item is not a file name
            ## but a name of an exisiting table.list,
            ## then load the corresponding table.filename as table
            if(exists("table.list") && (table.filename %in% names(table.list))) table.filename <- eval(parse(text=paste("table.list$",t,"$filename",sep="")))
            path.or.zip <- file.path(data.dir,table.filename)
            if (is.zip) {
                if (!table.filename %in% unzip(data.dir, list=TRUE)$Name) {
                    warning(paste0("--- table ",table.filename," not found in ",data.dir))
                    next
                } else {
                    path.or.zip <- unz(data.dir, table.filename)
                }
            }
            ## Make sure that 'ctr' and 'cn' are loaded as 'center' and 'patient'
            # t <- gsub(study.options$file.end, "", t) # shorten the names
            # t <- .removeproj(t) # shorten the names
            # if (t=="ctr") {
            #     t2 <- "center"
            # } else if (t %in% c("cn", "casenodes")) {
            #     t2 <- "patient"
            # } else {
            #     t2 <- t
            # }
            t2 <- study.options$data.names[t]

            ## Finally load the table
            if(!silent) cat("--- table",table.filename,"loaded as",t2,"---\n")
        assign(t2, read.DB.table(path.or.zip,
                                 convert.dates,
                                 convert.unknown.date.to.na,
                                 rename.headers,
                                 add.pat.id,
                                 add.center,
                                 silent),
               envir = .GlobalEnv)
      }
    }
  }
}






#' Load labels from an export .
#'
#' Get a named vector of variable labels.
#' Uses results of \code{load.study.options} directly - must be run after \code{load.tables} or \code{load.study.options}
#' @examples
#' ## non rectangular table
#' load.study.options(data.dir=system.file("extdata", "s_export_CSV-xls_DEM00_20180912-125720.zip", package = "secuTrial"))
#' labs <- load.labels()
#' labs[1]
#' @export
#' @seealso read.DB.table, load.table.list (used in dossier-specific packages), load.study.options
#' @references http://stackoverflow.com/questions/3640925/global-variable-in-r-function
#' @return (Pre-processed) tables in \code{tables} as data frames

load.labels <- function(){
  if(!exists("study.options")) stop("'study.options' not found \nrun load.study.options(...) or load.tables(...)")
  if(options()$stringsAsFactors) warning("stringsAsFactors is TRUE. Recommend setting to FALSE")

  if(study.options$is.zip){
    con <- unz(study.options$data.dir,
               secuTrial:::.constructmetaname("items"))
    tmp <- read.table(con,
                      sep = study.options$sep,
                      na.strings = study.options$na.strings,
                      header = TRUE)
    myIsOpen <- function(con) tryCatch(isOpen(con), error=function(e) FALSE)
    if(myIsOpen(con)) close(con)

  } else {
    tmp <- read.table(file.path(study.options$data.dir,
                                secuTrial:::.constructmetaname("items")),
                      sep = study.options$sep,
                      na.strings = study.options$na.strings,
                      header = TRUE)
  }
  # remove layout dummies
  tmp <- tmp[!is.na(tmp$ffcolname), ]
  # unique variables
  tmp <- unique(tmp[, c("ffcolname", "fflabel")])
  # duplicates (different labels) - retain longest
  if(length(unique(tmp$ffcolname)) < nrow(tmp)){
    warning("duplicate variable names - retaining longest label")
    nc <- nchar(tmp$ffcolname)
    tmp <- tmp[order(tmp$ffcolname, nc),]
    tmp$n <- unlist(tapply(tmp$ffcolname,
                           tmp$ffcolname,
                           function(x) 1:length(x)))
    tmp <- tmp[tmp$n == 1, ]
    tmp <- tmp[, 1:2]
  }
  rownames(tmp) <- tmp$ffcolname
  tmp2 <- tmp$fflabel
  names(tmp2) <- tmp$ffcolname
  return(tmp2)
}



#' add visit names to a table.
#'
#' Add the visit name to a table (as opposed to just the visit id - secuTrial uses many visit ids to refer to a single visit).
#' Uses results of \code{load.study.options} directly - must be run after \code{load.tables} or \code{load.study.options}
#' @param data data.frame to add visit name to
#' @examples
#' # TODO!!
#' @export
#' @seealso load.table, load.study.options
#' @references http://stackoverflow.com/questions/3640925/global-variable-in-r-function
#' @return data.frame with additional mnpvislabel variable for visit label

add.visit.name <- function(data){
  if(!exists("study.options")) stop("'study.options' not found \nrun load.study.options(...) or load.tables(...)")
  if(!.available("items")) stop("'items' metadata not available \n    - suggest exporting 'Project setup' with data")
  if(!"mnpvisid" %in% names(data)) stop("Visit ID not in this form")
  # load meta data table
  visits <- .load.meta.table("visitplan")
  # only need two columns
  visits <- visits[, c("mnpvisid", "mnpvislabel")]
  # rename one to be more obvious
  names(visits)[2] <- "visit.name"
  # merge with data
  tmp <- merge(data, visits)
  tmp <- move.column.after(tmp, c("visit.name", "mnpvisid"), "mnppid")
  return(tmp)
}

