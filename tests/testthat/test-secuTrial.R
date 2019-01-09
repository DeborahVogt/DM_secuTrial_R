context("secuTrial-testing")

# load data
load.tables(data.dir=system.file("extdata", "s_export_CSV-xls_DEM00_20180912-125720.zip", package = "secuTrial"))
calcium_st <- read.table(system.file("extdata", "calcium_secuTrial.csv", package = "secuTrial"), sep=";", header=TRUE)

# test dimensions
test_that("Bone mineral density dataset (non rectangular) has the correct dimensions", {
  expect_equal(dim(bmd),c(501, 25))
})

# retain relevant columns
bmd_compare <- bmd[,c("pat.id","mnpvispdt","age","grouping","bmd")]
calcium_st_compare <- calcium_st[,c("patid","visitdate","bmd.age","bmd.grouping","bmd.bmd")]

# test for data (age, patid, done mineral density) equality
test_that("Test import and export data for equality", {
   expect_equal(sum( (calcium_st_compare$bmd.age - bmd_compare$age) +  # age
                     (calcium_st_compare$patid - bmd_compare$pat.id) + # patid
                     (calcium_st_compare$bmd.bmd - bmd_compare$bmd)    # bone mineral density
                   ),
                0)
})

# test for visitdate equality
test_that("Test for visitdate equality", {
   expect_equal(calcium_st_compare$visitdate==format(as.Date(bmd_compare$mnpvispdt), "%d.%m.%Y"),
                rep(TRUE, 501))
})

# test for grouping equality
test_that("Test for grouping equality", {
   expect_equal(calcium_st_compare$bmd.grouping==bmd_compare$grouping,
                rep(TRUE, 501))
})

# test column moving
test_that("Test column moving", {
   expect_equal(names(move.column.after(df=calcium_st_compare,col.name=c("bmd.grouping","bmd.bmd"),"visitdate")),
                c("patid", "visitdate", "bmd.grouping", "bmd.bmd", "bmd.age"))
   expect_equal(names(move.column.to.pos(df=calcium_st_compare,col.idx=5,new.col.idx=3)),
                c("patid", "visitdate", "bmd.bmd", "bmd.age", "bmd.grouping"))
})

# test id translation
test_that("Test id translation", {
   expect_equal(mnppid2mnpaid(1512), 104)
   expect_equal(mnpaid2mnppid(104), 1512)
})

# test tag stripping
test_that("Test tag stripping", {
   expect_equal(remove.center.tag("Universitätsspital Basel (SWISS-AF)"), "Universitätsspital Basel")
   expect_equal(remove.center.tag("HUG Genève (SSR)"), "HUG Genève")
})

# test center retrieval
test_that("Test center retrieval", {
   expect_equal(as.character(mnppid2center(1509)), "Hospital")
   expect_equal(as.character(mnppid2center(1509, remove.ctag = 0)), "Hospital (BMD)")
})

# test rectangular table loading
load.tables(data.dir=system.file("extdata", "s_export_rt-CSV-xls_DEM00_20181016-151332.zip", package = "secuTrial"), is.rt = T, decode.rt.visitlabels = T)

# test dimensions
 test_that("Bone mineral density dataset (rectangular) has the correct dimensions", {
   expect_equal(dim(rtdata),c(112, 128))
 })

# test load.labels
load.study.options(data.dir=system.file("extdata", "s_export_CSV-xls_DEM00_20180912-125720.zip", package = "secuTrial"))
labs <- load.labels()
test_that("First label is age", {
  expect_equal(unname(labs["age"]),"Age")
})
