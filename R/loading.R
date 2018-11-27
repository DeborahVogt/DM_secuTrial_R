## Initilise study.options in GlobalEnv
##
## Upon package attachment these two lists are initialised in GlobalEnv.
.onAttach <- function(libname, pkgname) {
  load.study.options()
}



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
      print(paste0("File does not exist: ",e))
      return(NULL)
    }
    , finally = {
    }
    )
  }
  ## in eralier secuTrial exports there was a last/empty column "X" -> remove it
  if("X" %in% names(tab)) {
    tab <- tab[,-ncol(tab)]
  }
  if(rename.headers==TRUE) names(tab) <- new.names(tab)
  if(add.pat.id==TRUE & "mnppid" %in% names(tab)) {
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
  if(add.center==TRUE & "mnppid" %in% names(tab))  {
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
  if(convert.dates==TRUE) {
    ## iterate of multiple date types
    for(date.format in study.options$date.format) {
    tab <- convert.all.dates(tab, date.format, convert.unknown.date.to.na, unknown.date.string=study.options$unknown.date.string, partial.date.handling=study.options$partial.date.handling, partial.date.string=study.options$partial.date.string, silent)
  }
  }
  return(tab)
}

## ----------------------------------------------------------------------

#' List specifying the general properties of all tables in the export.
#'
#' The list \code{study.options} stores all relevant technical information
#' in order to be able to correctly load the tables of a given study.
#' The list is used in the function \code{read.DB.table}.
#'
#' @details \code{partial.date.handling}: By default, potential data columns containing entries that cannot be converted are
#' skipped (option 'skip'). With 'force.conversion' fields incompatible with the date format are converted to NA.
#' 'fill.partial.dates' keeps the orginal column untouched and adds a new column to the data frame with the name <colname>.partial.dates.processed
#' in which partial are filled using \code{\link{fill.partial.date}} (e.g. Unknown.01.2013 -> 15.01.2013).
#' With 'fill.partial.dates.and.keep.original' partial dates are processed directly and the original data is copied to <colname>.original.
#' @export
#' @seealso read.DB.table, convert.all.dates
load.study.options <- function() {
  study.options <- list(sep='\t',
                        date.format=c("%Y%m%d", "%Y-%m-%d"),
                        na.strings = c("NA",""), # if blanks mean missing
                        unknown.date.string = NULL, # incomplete dates
                        partial.date.string = "",
                        partial.date.handling = "fill.partial.dates.and.keep.original")
  assign("study.options", study.options, envir = .GlobalEnv)
  return(NULL)
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
#'             is.rt = T, decode.rt.visitlabels = T)
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

  ## handle loading from zip
  is.zip <- FALSE
  if(substr(data.dir, nchar(data.dir)-2, nchar(data.dir)) == "zip") is.zip <- TRUE

  #######################################################
  ## Check if neccessary items are included in export  ##
  #######################################################

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


  if(is.rt) { ## rectangular input
    close(path.or.zip)
    files_in_zip <- unzip(data.dir, list=T)

    rtdata_con <- unz(data.dir, filename=files_in_zip$Name[grep("data",files_in_zip$Name)])
    ## rtdata_internal
    rtdata_internal <- read.csv(file=rtdata_con, header=T, sep="\t")

    if (decode.rt.visitlabels) {
      vp_con <- unz(data.dir, filename=files_in_zip$Name[grep("vp",files_in_zip$Name)])
      vp <- read.csv(file=vp_con, header=T, sep="\t")
      ## clean out spaces and other common disturbing characters
      vp$mnpvislabel <- gsub("\\s+", "_", vp$mnpvislabel)
      vp$mnpvislabel <- gsub("\\.", "", vp$mnpvislabel)
      vp$mnpvislabel <- gsub("\\|", "", vp$mnpvislabel)
      vp$mnpvislabel <- gsub("#", "", vp$mnpvislabel)
      ## decoding
      for(i in 1:length(vp$mnpvislabel)) {
        names(rtdata_internal) <- gsub(x = names(rtdata_internal), pattern = vp$mnpvisid[i], replacement = vp$mnpvislabel[i])
      }
      ## remove leading v
      #names(rtdata_internal) <- gsub(x=names(rtdata_internal), pattern="^v", replacement="")
    }
    ## rtdata is a global variable
    if (silent==FALSE) {
      cat("--- rectangular data written into variable rtdata ---\n")
    }
    rtdata <<- rtdata_internal
  } else { ## non rectangular input

    parsed.export <- readLines(path.or.zip)
    if (is.zip) close(path.or.zip)

    ## Make sure that ExportOptions.html uses english
    ## TODO: Support German customer area!
    ##if(silent==FALSE) cat("** Checking Language of ExportOptions.html\n")
    ##if (length(grep("Created on",parsed.export))==0) {
    ##    warning("ExportOptions.html is written in other language than English. Automatic reading of csv separator might not be possible... In case you run into trouble, please contact a Data Manager to set the Customer area to English.\n", immediate.=TRUE)
    ##} else if (silent==FALSE) {
    ##   cat("** ExportOptions.html is in English\n")
    ##}

    ## Make sure that column names are included in Export!
    if(silent==FALSE) cat("** Checking for 'Column names' in ExportOptions.html'\n")
    if (length(grep("Column names",parsed.export))==0 & length(grep("Spaltennamen",parsed.export))==0) {
        stop("The secuTrial export does not include 'Column names'")
        return(NULL)
    } else if (silent==FALSE) {
        cat("** 'Column names' ('Spaltennamen') was found in ExportOptions.html\n")
    }

    ## Make sure that Add-ID/Zus-ID is included in export
    if (add.pat.id == TRUE) {
        if(silent==FALSE) cat("** Checking for 'Add-ID' in ExportOptions.html\n")
        if (length(grep("Add-ID",parsed.export))==0 & length(grep("Zus-ID",parsed.export))==0 & length(grep("Patient-ID",parsed.export))==0)  {
            stop("The secuTrial export does not include 'Add-ID'")
            return(NULL)
        } else if (silent==FALSE) {
            cat("** 'Add-ID' ('Zus-ID', 'Patient-ID') was found in ExportOptions.html\n")
        }
    }



    ##################################################################
    ## If tables = NULL Load tables from table.list in dossier.lib  ##
    ##################################################################

    if(is.null(tables)) {
        if(silent==FALSE) cat("** Loading tables with 'table.list' (probably defined in dossier library\n")
        tables <- names(table.list)
        ## ensure that patient and center table are loaded first
        ## (needed to add pat.id and center to all tables)
        if(add.pat.id == TRUE & add.center == TRUE) {
            tables <- c("patient", "center", tables)
            tables <- tables[!duplicated(tables)]
        }
        if(add.pat.id == TRUE & add.center == FALSE) {
            tables <- c("patient", tables)
            tables <- tables[!duplicated(tables)]
        }
        for(t in tables) {
            table.filename <- eval(parse(text=paste("table.list$",t,"$filename",sep="")))
            if(silent==FALSE) cat("--- table",table.filename,"loaded as",t,"---\n")
            path.or.zip <- file.path(data.dir,table.filename)
            if(is.zip) {
                path.or.zip <- unz(data.dir, table.filename)
            }
        assign(t, read.DB.table(path.or.zip, convert.dates, convert.unknown.date.to.na, rename.headers, add.pat.id, add.center, silent), envir = .GlobalEnv)
        }
    } else if (tables[1]=="all") {
        ###################################################
        ## IF tables = TRUE Load all tables in data.dir  ##
        ###################################################
        if(silent==FALSE) cat(paste0("** Loading all tables from ",data.dir,"\n"))
        ## Throw warning if table.list exists
        if(silent==FALSE) cat("** Ensuring that no 'table.list' was set by user\n")
        if (exists("table.list")) {
            warning("previously defined 'table.list' (possibly from dossier library) was written over!\n")
            remove("table.list", envir = .GlobalEnv)
                  if(silent==FALSE) cat("--- Deleting previous 'table.list'\n")
        } else {
            if(silent==FALSE) cat("** No 'table.list' found\n")
        }
        # Get the names of the table.list
        if(silent==FALSE) cat("** Building the 'table.list'\n")
        if(is.zip) {
          table.list <- unzip(data.dir, list=TRUE)$Name
        } else {
          table.list <- list.files(data.dir)
        }
        # ExportOptions.html are not a dataframe
        table.list <- table.list[which(table.list!="ExportOptions.html")]
        assign("table.list", table.list, envir=.GlobalEnv)
        if(silent==FALSE) cat(paste0("*** ",length(table.list)," tables were found\n"))
        if(silent==FALSE) cat("** Calling load.tables(data.dir, tables = table.list, ...)\n")
        load.tables(data.dir,
                    table.list,
                    convert.dates,
                    convert.unknown.date.to.na,
                    rename.headers,
                    add.pat.id,
                    add.center,
                    silent)
    } else {
        ################################################
        ## ELSE Load tables from input list 'tables'  ##
        ################################################
        if(silent==FALSE) cat("** Loading tables as defined in input tables = ... \n")
        ## ensure that patient and center table are loaded first
        ## (needed to add pat.id and center to all tables)
        ## Add xls or csv version of patient and center tables
        if ((length(grep("CSV format",parsed.export))!=0 | length(grep("CSV-Format",parsed.export))!=0 ) & length(grep("MS Excel",parsed.export))!=0) {
              if(add.pat.id == TRUE & add.center == TRUE) {
                  tables <- c("ctr.xls", "cn.xls", tables)
                  tables <- tables[!duplicated(tables)]
                  if(silent==FALSE) cat("*** Added ctr.xls and cn.xls to tables\n")
              }
              if(add.pat.id == TRUE & add.center == FALSE) {
                  tables <- c("cn.xls", tables)
                  tables <- tables[!duplicated(tables)]
                  if(silent==FALSE) cat("*** Added cn.xls to tables\n")
              }
        } else if ((length(grep("CSV format",parsed.export))!=0 | length(grep("CSV-Format",parsed.export))!=0)  & length(grep("MS Excel",parsed.export))==0) {
              if(add.pat.id == TRUE & add.center == TRUE) {
                  tables <- c("ctr.csv", "cn.csv", tables)
                  tables <- tables[!duplicated(tables)]
                  if(silent==FALSE) cat("*** Added ctr.csv and cn.csv to tables\n")
              }
              if(add.pat.id == TRUE & add.center == FALSE) {
                  tables <- c("cn.csv", tables)
                  tables <- tables[!duplicated(tables)]
                  if(silent==FALSE) cat("*** Added cn.csv to tables\n")
              }
              ## Get info on Field separator and edit study.options
              if(silent==FALSE) cat("*** Parsing ExportOptions.html for csv separator\n")
              parsed.sep <- parsed.export[grep("Field separated with",parsed.export)||grep("Feld getrennt mit",parsed.export)]
              if (length(grep("Komma",parsed.sep))!=0) {
                  study.options$sep <- ","
              } else if (length(grep("'",parsed.sep))!=0) {
                  study.options$sep <- "'"
              } else if (length(grep("Semikolon",parsed.sep))!=0) {
                  study.options$sep <- ";"
              } else if (length(grep("Tabulator",parsed.sep))!=0) {
                  study.options$sep <- "\t"
              } else if (length(grep("@",parsed.sep))!=0) {
                  study.options$sep <- "@"
              } else {
                  stop("Unknown Field Separator in ExportOptions.html")
                  return(NULL)
              }
              if(silent==FALSE) cat(paste0("*** CSV separator identified: '",study.options$sep,"'\n"))
              assign("study.options",study.options, envir = .GlobalEnv)
        } else {
            stop("ExportOptions.html does not include information on export Format (.xls or .csv)")
            return(NULL)
        }
        for(t in tables) {
            table.filename <- t
            ## For userfriendlieness, strip common endings like .xls or .csv
            if(substr(t,nchar(t)-2,nchar(t))=="xls"||substr(t,nchar(t)-2,nchar(t))=="csv") t <- substr(t,1,nchar(t)-4)
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
            if (t=="ctr") {
                t2 <- "center"
            } else if (t=="cn") {
                t2 <- "patient"
            } else {
                t2 <- t
            }
            ## Finally load the table
            if(silent==FALSE) cat("--- table",table.filename,"loaded as",t2,"---\n")
        assign(t2, read.DB.table(path.or.zip, convert.dates, convert.unknown.date.to.na, rename.headers,
                                 add.pat.id, add.center, silent), envir = .GlobalEnv)
      }
    }
  }
}

