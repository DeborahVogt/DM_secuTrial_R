#' Convert internal id to patient id
#'
#' This function converts the internal id \code{mnppid} used in most tables
#' to the real patient id (\code{mnpaid}).
#' It requires you to have a loaded secuTrial export in your active workspace.
#'
#' @param mnppid internal id (backend)
#' @return patient id
#' @export
#' @examples
#' \dontrun{
#' mnppid2mnpaid(978) # 10001
#' }
#' @seealso mnpaid2mnppid, load.tables
mnppid2mnpaid <- function(mnppid) {
  patient <- get("patient") # declare table
  patient$mnpaid[match(mnppid, patient$mnppid)]
}

#' Convert patient id to internal id
#'
#' This function converts the patient id \code{mnpaid}
#' to the internal id (\code{mnppid}) used in most tables.
#' It requires you to have a loaded secuTrial export in your active workspace.
#'
#' @param mnpaid patient id (frontend)
#' @return internal id
#' @export
#' @examples
#' \dontrun{
#' mnpaid2mnppid(10001) # 978
#' }
#' @seealso mnppid2mnpaid, load.tables
mnpaid2mnppid <- function(mnpaid) {
  patient <- get("patient") # declare table
  patient$mnppid[match(mnpaid, patient$mnpaid)]
}

#' This function trims whitespaces from a string.
#'
#' Taken from the lava package
#'
#' @param all will remove all whitespaces from string if set to TRUE. If FALSE only leading and trailing whitespaces will be removed.
#' @return string
#' @export
#' @examples
#' trim("  test string trim  ")
#' # "test string trim"
#' trim("  test string trim  ", all=TRUE)
#' # "teststringtrim"
trim <- function (x, all = FALSE, ...)
{
    if (!all)
        return(gsub("^\\s+|\\s+$", "", x))
    return(gsub("\\s", "", x))
}

#' Helper function to strip away trailing center tag in parentheses
#'
#' @examples
#' remove.center.tag("Universitätsspital Basel (SWISS-AF)")
#' # "Universitätsspital Basel"
#' remove.center.tag("HUG Genève (SSR)")
#' # "HUG Genève"
#' @export
#' @param x center string
#' @return cleaned center string
#' @seealso mnppid2center
remove.center.tag <- function(x) {
  x <- as.character(x)
  pos <- which(strsplit(x, "")[[1]]=="(")
  if(length(pos) == 1) {
    x <- trim(substr(x, 1, pos-1))
  }
  return(x)
}


#' Convert center id to center
#'
#' This function returns the center for a given internal id \code{mnppid}.
#' It requires you to have a loaded secuTrial export in your active workspace.
#'
#' @param mnppid internal id
#' @param remove.ctag 1 for yes (default) 0 for no
#' @return Center name as string.
#' @export
mnppid2center <- function(mnppid, remove.ctag=1) {
  center <- get("center") # declare table
  patient <- get("patient") # declare table
  ## first convert mnppid to mnpctrid
  mnpctrid <- patient$mnpctrid[match(mnppid, patient$mnppid)]
  if(remove.ctag==1) {
    ## then convert center id to center and remove center tag
    as.factor(unlist(lapply(center$mnpctrname[match(mnpctrid, center$mnpctrid)], remove.center.tag)))
  } else {
    as.factor(unlist(center$mnpctrname[match(mnpctrid, center$mnpctrid)]))
  }
}


#' Add patient id
#'
#' This function adds the patient id (i.e. \code{mnpaid})
#' to the specified table. The column is added at the front.
#'
#' @param tab table to which pat.id column should be added to
#' @param id column name (default: pat.id)
#' @return Table with pat.id column at the front.
#' @export
#' @seealso mnppid2mnpaid
add.pat.id <- function(tab, id="pat.id") {
  tab[[id]] <- mnppid2mnpaid(tab$mnppid)
  tab <- move.column.after(tab, id, "first")
  return(tab)
}

#' Add center
#'
#' This function adds the center information to the specified table.
#' The column is added after \code{pat.id} if present and after
#' \code{mnppid} otherwise.
#'
#' @param tab table to which center column should be added to
#' @param id column name (default: center)
#' @return Table with additional center column.
#' @export
#' @seealso mnppid2center
add.center <- function(tab, id="center") {
  tab[[id]] <- mnppid2center(tab$mnppid)
  if("pat.id" %in% names(tab)) tab <- move.column.after(tab, id, "pat.id")
  else tab <- move.column.after(tab, id, "mnppid")
  return(tab)
}
