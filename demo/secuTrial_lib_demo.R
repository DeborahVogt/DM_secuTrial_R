
### (1)
##  prepare dataset for import into secuTrial

## calcium dataset
library("lava") # v.1.6.3
## mapvalues()
library("plyr") # v.1.8.4

## todate
## makes a date of the input
todate <- function(x) {
  return(as.Date(x, origin = "1899-12-30"))
}

## round according to commonly accepted rounding
## i.e. 0.5 -> 1 instead of 0
expected_round <- function(num, digits=1) {
  return(round((num+0.00000000000001), digits = digits))
}

## add data
data(calcium)
calcium_st <- calcium

## alternatively you can load the dataset from calcium_dataset.csv

## visit dates
calcium_st$ctime <- todate(calcium_st$ctime)
calcium_st$ctime <- format(calcium_st$ctime, "%d.%m.%Y")

## adjust col names
names(calcium_st) <- c("bmd.bmd", "bmd.grouping", "patid", "visit", "bmd.age", "visitdate")

## centre
calcium_st$centre <- "Hospital (BMD)"

## add entry date (today)
calcium_st$entrydate <- Sys.Date()
calcium_st$entrydate <- format(calcium_st$entrydate, "%d.%m.%Y")

## round age
calcium_st$bmd.age <- expected_round(calcium_st$bmd.age, digits=5)

## spell grouping out
calcium_st$bmd.grouping <- mapvalues(calcium_st$bmd.grouping, c("C","P"), c("Calcium","Placebo"))

## reorder
calcium_st <- calcium_st[c("patid","centre","entrydate","visitdate","bmd.bmd","bmd.grouping","bmd.age")]

## write data
write.table(calcium_st, "calcium_secuTrial.csv", row.names=F, quote=F, sep=";")

## import data into secuTrial


### (2)
##  export the same data from secuTrial

##
### secuTrial export options:
##

## Visit:                                             all visits
## Patients:                                          All patients
## Format:                                            CSV format for MS Excel
## Rectangular table:                                 No
## Duplicate form meta data into all tables:          No
## Column names:                                      Yes
## Store reference values:                            same table
## number format:                                     English number format (decimal sign=".")
## Shorten table name:                                Yes
## Character encoding:                                UTF-8
## Patient-ID:                                        Yes
## Lab-ID:                                            No
## Centre information:                                Yes
## Project setup:                                     Yes
## Queries and Comments:                              Yes
## Audit Trail:                                       Yes
## Form items:                                        all items
## Form meta data:                                    Patient status, Structure, Document editing, Form status
## Data handling: 	unselected checkbox as:           "NULL" value
## date/time values as:                               string (with spaces)
## Filter data records:                               no check (all)

## lib
library(secuTrial)

## data
load.tables(data.dir=system.file("extdata", "s_export_CSV-xls_DEM00_20180912-125720.zip", package = "secuTrial"))

### (3)
##  compare source data (calcium_st) and secuTrial export (bmd)
bmd_compare <- bmd[,c("pat.id","mnpvispdt","age","grouping","bmd")]
calcium_st_compare <- calcium_st[,c("patid","visitdate","bmd.age","bmd.grouping","bmd.bmd")]

## check dimensions
## no differences should lead to the same dimensions
dim(bmd_compare)
# [1] 501   5
dim(calcium_st_compare)
# [1] 501   5

## check for differences in numeric values by subtracting the vecotrs from each other
## no differences should lead to a sum of zero
sum( (calcium_st_compare$bmd.age - bmd_compare$age) +  # age
     (calcium_st_compare$patid - bmd_compare$pat.id) + # patid
     (calcium_st_compare$bmd.bmd - bmd_compare$bmd)    # bone mineral density
   )
# [1] 0

## check for date differences
## no differences should lead to all individual dates being equal (i.e. all TRUE)
table(calcium_st_compare$visitdate==format(as.Date(bmd_compare$mnpvispdt), "%d.%m.%Y"))
# TRUE
# 501
## reverse first table to test what happens if inequality exists (i.e. most values should be FALSE)
table(rev(calcium_st_compare$visitdate)==format(as.Date(bmd_compare$mnpvispdt), "%d.%m.%Y"))
# FALSE  TRUE
# 496     5

## check for grouping differences
## no differences should lead to all individual grouping values being equal (i.e. all TRUE)
table(calcium_st_compare$bmd.grouping==bmd_compare$grouping)
# TRUE
# 501
## reverse first table to test what happens if inequality exists (i.e. most values should be FALSE)
table(rev(calcium_st_compare$bmd.grouping)==bmd_compare$grouping)
# FALSE  TRUE
# 268   233

### (4)
## moving columns
names(calcium_st_compare)
# "patid"        "visitdate"    "bmd.age"      "bmd.grouping" "bmd.bmd"

## position "bmd.grouping" and "bmd.bmd" behind "visitdate" with "move.column.after"
calcium_st_compare_mv1 <- move.column.after(df=calcium_st_compare,col.name=c("bmd.grouping","bmd.bmd"),"visitdate")
names(calcium_st_compare_mv1)
# "patid"        "visitdate"    "bmd.grouping" "bmd.bmd"      "bmd.age"

## position "bmd.age" back to position 3 with "move.column.to.pos"
calcium_st_compare_mv2 <- move.column.to.pos(df=calcium_st_compare_mv1,col.idx=5,new.col.idx=3)
names(calcium_st_compare_mv2)
# "patid"        "visitdate"    "bmd.age"      "bmd.grouping" "bmd.bmd"


### (5)
## translating ids
mnppid2mnpaid(1512)
# 104
mnpaid2mnppid(104)
# 1512

## stripping center tags
remove.center.tag("Universitätsspital Basel (SWISS-AF)")
# "Universitätsspital Basel"
remove.center.tag("HUG Genève (SSR)")
# "HUG Genève"

## get center from mnppid
mnppid2center(1509)
# [1] Hospital
# Levels: Hospital
mnppid2center(1509, remove.ctag = 0)
# [1] Hospital (BMD)
# Levels: Hospital (BMD)

## if pat.id is not present in tables it can be added
load.tables(system.file("extdata", "s_export_CSV-xls_DEM00_20180912-125720.zip", package = "secuTrial"), add.pat.id = F)
## original
head(names(bmd), n=3)
# "mnppid"      "mnpdocid"    "mnplastedit"
## with pat.id column
head(names(add.pat.id(bmd)), n=4)
# "pat.id"      "mnppid"      "mnpdocid"    "mnplastedit"
dim(bmd)
# 501  24
dim(add.pat.id(bmd))
# 501  25

## add center to tables
## original
head(names(bmd), n=3)
# "mnppid"      "mnpdocid"    "mnplastedit"
## with center column second
head(names(add.center(bmd)), n=4)
# "mnppid"      "center"      "mnpdocid"    "mnplastedit"
dim(bmd)
# 501  24
dim(add.center(bmd))
# 501  25

### (6)
## loading rectangular tables (loads intp global variable rtdata)
load.tables(system.file("extdata", "s_export_rt-CSV-xls_DEM00_20181016-151332.zip", package = "secuTrial"), is.rt = T, decode.rt.visitlabels = T)
## names(rtdata)


