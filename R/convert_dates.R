## ----------------------------------------------------------------------
## FUNCTIONS OPERATING ON DATES
## ----------------------------------------------------------------------

## TODO:
## - re-write as generic to both allow df and individual columns
##   name it: convert.date
## - possibly extract "convert.unknown.date.to.na" as individual function

#' Convert all date columns of a data.frame to Date vectors.
#'
#' This function identifies all date columns in a data frame and converts them
#' to vectors of Date objects. Optionally, unknown dates (e.g. coded as '1900-01-01')
#' can be converted to \code{NA}.
#'
#' @param dat data frame to convert dates
#' @param .format format of the dates (default: '\%Y-\%m-\%d')
#' @param convert.unknown.date.to.na flag for conversion of unknown dates
#' @param unknown.date.string string specifying the coding of unknown dates
#' @param partial.date.handling how to handle columns containing entries that cannot be converted (default: skip; see Details)
#' @param partial.date.string string used for unknown data components (e.g. 'Unknown' or '--')
#' @param silent hide verbose output
#' @return Date frame with date columns converted.
#' @details \code{partial.date.handling}: By default, potential data columns containing entries that cannot be converted are skipped (option 'skip'). With 'force.conversion' fields incompatible with the date format are converted to NA. 'fill.partial.dates' keeps the orginal column untouched and adds a new column to the data frame with the name <colname>.partial.dates.processed in which partial are filled using \code{\link{fill.partial.date}} (e.g. Unknown.01.2013 -> 15.01.2013). With 'fill.partial.dates.and.keep.original' partial dates are processed directly and the original data is copied to <colname>.original.
#' @export
#' @seealso fill.partial.date
#' @examples
#' convert.all.dates(data.frame(date=c("2014-01-Unknown", "1900-01-01")),
#' partial.date.handling="fill.partial.dates", convert.unknown.date.to.na = TRUE,
#' unknown.date.string="1900-01-01")
#' ##              date date.partial.dates.processed
#' ##1  2014-01-Unknown                   2014-01-15
#' ##2       1900-01-01                         <NA>
#' @author Pascal Benkert
convert.all.dates <- function(dat, .format="%Y-%m-%d", convert.unknown.date.to.na = FALSE, unknown.date.string=NULL, partial.date.handling="skip", partial.date.string="Unknown", silent=FALSE) {
  for(i in names(dat)) {
    ## since multiple date formats may be check, skip if already converted to Date
    if (inherits(dat[[i]], "Date")) next
    ## handle error, e.g. Error in strptime(x, format, tz = "GMT"): input string is too long
    possibleError <- tryCatch(x <- as.Date(as.character(dat[[i]]), format=.format), error = function(e) {warning(paste0("Column '", i, "': ", e, " Skipped..."))})
    if(inherits(possibleError, "error")) next
      ##x <- as.Date(as.character(dat[[i]]), format=.format)
    text.length <- as.numeric(sapply(as.character(dat[[i]])[!is.na(dat[[i]])],FUN=nchar))
    ## non-date columns have only NA entries after conversion (others are date fields)
    ## FIXME: "Unknown." is used in the in the MS cohort as a placeholder which is a 7 digits string:
    ## To avoid that comment columns with a by change date entry are converted which check the
    ## overall string length. alternatively we may convert prior to all other steps "Unknown" (i.e. unknown.date.string to a generic generic placeholder "00" or "--"
    if(!all(is.na(x)==TRUE) & (all(text.length<=10 & text.length>=4) | length(grep("Unknown", dat[[i]], fixed=T)) != 0)) {
      ## identify 3-component-fields
      is.three.component.field <- FALSE
      date.string.length <- 8 + (nchar(.format) - 6)
      ##
      if(nchar(partial.date.string) > 2 & length(grep(partial.date.string, dat[[i]])) > 0) is.three.component.field <- TRUE
      ## handle dates in freetext fields (e.g. comments: "23.05.2013: positive")
      ## none of the entries is allowed to be longer than the format string
      ## calculate date string length: len("%Y%m%d")=8, len("%Y.%m.%d")=10
      if(is.three.component.field == FALSE & TRUE %in% (as.numeric(sapply(as.character(dat[[i]]),FUN=nchar)) > date.string.length)) {
        if(silent==FALSE) cat(" *", i,"identified as potential date column, but contains \nstrings longer than date format\n")
        next
      }
      if(silent==FALSE) cat(" *", i,"identified as potential date column\n")
      converted <- TRUE
      col.original <- dat[[i]]
      ## handle missing dates coded as string (unknown.date.string)
      if(!is.null(unknown.date.string)) {
        nr.missing.dates <- length(dat[[i]][!is.na(dat[[i]]) & dat[[i]] == unknown.date.string])
        if(nr.missing.dates > 0) {
          if(convert.unknown.date.to.na == TRUE) {
            dat[[i]][!is.na(dat[[i]]) & dat[[i]] == unknown.date.string] <- NA
            if(silent==FALSE) cat("   *", nr.missing.dates,"date(s) coded as", unknown.date.string, "set to NA\n")
          }
          else {
            if(silent==FALSE) cat("   *", nr.missing.dates,"date(s) coded as", unknown.date.string, "identified\n")
          }
        }
      }
      ## fill up trimed dates
      text.length <- as.numeric(sapply(as.character(dat[[i]])[!is.na(dat[[i]])],FUN=nchar))
      if ((partial.date.string == "" | partial.date.string == "  ") & nchar(.format) == 6 & length(text.length) > 0 & all(text.length<=8 & text.length>=4)) {
        ## %Y%m%d: fill right
        if (.format == "%Y%m%d") dat[[i]] <- sprintf("%-8s", dat[[i]])
        ## %d%m%Y: fill right
        else if (.format == "%d%m%Y") dat[[i]] <- sprintf("%8s", dat[[i]])
        else {
          warning(paste("   *", "incomplete dates of the format", .format, "and no placeholder cannot be handeled."), immediate.=TRUE)
          next
        }
        ## restore NA and ""
        dat[[i]][trim(dat[[i]]) == "NA" | trim(dat[[i]]) == ""] <- NA
        partial.date.string = "  "
      }
      ## convert dates
      col.before.conversion <- dat[[i]] # unknown.date.string already processed
      dat[[i]] <- as.Date(as.character(dat[[i]]), format=.format)
      ## handle entries that cannot be converted
      ## is NA after conversion but was not NA before
      corrupt.entires <- (is.na(dat[[i]]) & (!is.na(col.before.conversion)))
      if(TRUE %in% corrupt.entires) {
        if(partial.date.handling == "skip") {
          dat[[i]] <- col.before.conversion
          warning(paste("   *", length(which(corrupt.entires)),"could not be converted - conversion skipped."),immediate.=TRUE)
          cat("   *", length(which(corrupt.entires)),"could not be converted - conversion skipped.\n")
          converted <- FALSE
        }
        else if(partial.date.handling == "fill.partial.dates") {
          ## restore original
          dat[[i]] <- col.original
          tryCatch({
          new.col.name <- paste(i, ".partial.dates.processed", sep="")
          dat[[new.col.name]] <- as.Date(as.character(unlist(lapply(col.before.conversion, FUN= function(x,y,z) fill.partial.date(x, partial.date.string, .format) ))), format=.format)
          dat <- move.column.after(dat, new.col.name, i)
          cat("   * ", length(which(corrupt.entires))," partial dates identified - new column", new.col.name, "\n")
          converted <- TRUE
          }, error = function(err) {
          ##stop()
           warning(paste("   *", length(which(corrupt.entires)),"could not be converted - conversion skipped."),immediate.=TRUE)
          cat("   *", length(which(corrupt.entires)),"could not be converted - conversion skipped.\n")
          converted <- FALSE
          })
        }
        else if(partial.date.handling == "fill.partial.dates.and.keep.original") {
          ## restore original
          tryCatch({
          new.col.name <- paste(i, ".original", sep="")
          dat[[new.col.name]] <- col.original
          dat[[i]] <- as.Date(as.character(unlist(lapply(col.before.conversion, FUN= function(x,y,z) fill.partial.date(x, partial.date.string, .format) ))), format=.format)
          dat <- move.column.after(dat, new.col.name, i)
          cat("   * ", length(which(corrupt.entires))," partial dates identified - dates converted and original data stored in column", new.col.name, "\n")
          converted <- TRUE
          }, error = function(err) {
           ##stop()
           warning(paste("   *", length(which(corrupt.entires)),"could not be converted - conversion skipped."),immediate.=TRUE)
          cat("   *", length(which(corrupt.entires)),"could not be converted - conversion skipped.\n")
          converted <- FALSE
          })
        }
        else {
          warning(paste("   *", length(which(corrupt.entires)),"could not be converted. Fields converted to NA."), immediate.=TRUE)
           cat("   *", length(which(corrupt.entires)),"could not be converted. Fields converted to NA.\n")
        }
      }
    cat("   *", "column", ifelse(converted, "","NOT"), "converted to date\n")
    }
  }
  return(dat)
}

## ----------------------------------------------------------------------

#' Fill partial dates.
#'
#' This function convert a partial date of the form Unknown.mm.YYYY or
#' Unknown.Unknown.YYYY to the typical placeholders 15.mm.YYYY and 01.07.YYYY,
#' respectively. Other \code{partial.date.string} can be specified.
#' Alternatively, the 'exact' midpoint of the month/year can be used, i.e.
#' \code{floor(ndays/2)} and 02.07.YYYY.
#'
#' @param date.string string containing a partial date
#' @param partial.date.string string used for unknown data components (e.g. 'Unknown' or '--')
#' @param .format format of the dates (default: '\%Y-\%m-\%d')
#' @param exact use the exact modpoints \code{floor(ndays/2)} and 02.07.YYYY
#' @return Date string, converted if applicable.
#' @export
#' @examples
#' fill.partial.date("Unknown.02.2014", "Unknown", .format="%d.%m.%Y", exact=TRUE) # 14.02.2014
#' fill.partial.date("2014/--/--", "--", .format="%Y/%m/%d") # 2014/07/01
#' @author Pascal Benkert
fill.partial.date <- function(date.string, partial.date.string="Unknown", .format="%Y-%m-%d", exact=FALSE) {
  ## skip NA's
  if(is.na(date.string)) return(NA)
  ####################
  ## german format: ##
  ####################
  ## any separator or none allowed
  if(length(grep("%d?.%m?.%Y", .format)) != 0) {
    separator <- ""
    ## check whether same separator used or ne separator
    if(substr(.format,3,3) == substr(.format,6,6)) {
      separator <- substr(.format,3,3)
    }
    else {
      ## e.g. %d.%m/%Y
      if(nchar(.format) != 6) stop(paste0("Wrong date format '",.format,"'."))
    }
    if(date.string == paste(partial.date.string, partial.date.string, partial.date.string, sep=separator)) return(NA)
    ## handle Unknown day+month
    unk.day.month.placeholder <- paste("01",separator,"07",separator,sep="")
    if(exact == TRUE) unk.day.month.placeholder <- paste("02",separator,"07",separator,sep="")
    date.string <- sub(paste(partial.date.string, separator, partial.date.string, separator, sep=""), unk.day.month.placeholder , date.string)
    ## handle Unknown day
    if(regexpr(partial.date.string, date.string)[1] == 1) {
      tryCatch(month.days <- days.of.month(as.Date(gsub(partial.date.string,"01",date.string), format=.format)), error = function(e) stop(paste0("Date '",date.string,"' does not match format '", .format, "'.")))
      if(exact == TRUE) date.string <- gsub(partial.date.string,floor(month.days/2),date.string) else date.string <- gsub(partial.date.string,"15",date.string)
    }
    ## final check that output is correct date
    if(is.na(as.Date(date.string, format=.format))) stop(paste0("Date '",date.string,"' cannot be processed using format '", .format, "'."))
    return(date.string)
  }
  ####################
  ## english format:##
  ####################
  ## any separator or none allowed
  if(length(grep("%Y?.%m?.%d", .format)) != 0) {
    separator <- ""
    ## check whether same separator used or ne separator
    if(substr(.format,3,3) == substr(.format,6,6)) separator <- substr(.format,3,3)
    else {
      ## e.g. %d.%m/%Y
      if(nchar(.format) != 6) stop(paste0("Wrong date format '",.format,"'."))
    }
    if(date.string == paste(partial.date.string, partial.date.string, partial.date.string, sep=separator)) return(NA)
    ## handle Unknown day+month
    unk.day.month.placeholder <- paste(separator, "07", separator, "01", sep="")
    if(exact == TRUE) unk.day.month.placeholder <- paste(separator, "07", separator, "02", sep="")
    date.string <- sub(paste(separator, partial.date.string, separator, partial.date.string, sep=""), unk.day.month.placeholder , date.string)
    ## handle Unknown day
    if(regexpr(partial.date.string, date.string)[1] == (nchar(date.string) - nchar(partial.date.string) + 1)) {
      tryCatch(month.days <- days.of.month(as.Date(gsub(partial.date.string,"01",date.string), format=.format)), error = function(e) stop(paste0("Date '",date.string,"' does not match format '", .format, "'.")))
      if(exact == TRUE) date.string <- gsub(partial.date.string,floor(month.days/2),date.string) else date.string <- gsub(partial.date.string,"15",date.string)
    }
    ## final check that output is correct date
    if(is.na(as.Date(date.string, format=.format))) stop(paste0("Date '",date.string,"' cannot be processed using format '", .format, "'."))
    return(date.string)
  }
  else stop(paste0("Wrong date format '",.format,"'."))
}


## ----------------------------------------------------------------------

#' Number of days of given month.
#'
#' This function returns the number of days for a given month as specified by
#' as Date object generated by \code{\link{as.Date}}.
#'
#' @param date Date object
#' @return Number of days.
#'
#' @examples
#' days.of.month(as.Date("01.02.2013", "%d.%m.%Y")) # 28
#' @export
#' @author Pascal Benkert
days.of.month <- function(date) {
  assertthat::assert_that(inherits(date, "Date") & !is.na(date))
  m <- format(date, format="%m")
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format="%d")))
}
