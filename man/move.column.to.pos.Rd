% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moving.R
\name{move.column.to.pos}
\alias{move.column.to.pos}
\title{Move a column in a data frame by index.}
\usage{
move.column.to.pos(df, col.idx, new.col.idx)
}
\arguments{
\item{df}{data frame}

\item{col.idx}{old column index}

\item{new.col.idx}{new column index}
}
\value{
Data frame with rearranged columns.
}
\description{
This function moves a given column (defined by the column index)
to a specific position in the data frame.
}
\examples{
## create a dataframe
a_dataframe <- data.frame(a=c(1,0),b=c(2,0),c=c(3,0),d=c(4,0))
a_dataframe
##   a b c d
## 1 1 2 3 4
## 2 0 0 0 0

## move column 1 to column 3
move.column.to.pos(df=a_dataframe,col.idx=1,new.col.idx=3)
##   b c a d
## 1 2 3 1 4
## 2 0 0 0 0

}
\seealso{
move.column.after
}
\author{
Pascal Benkert
}
