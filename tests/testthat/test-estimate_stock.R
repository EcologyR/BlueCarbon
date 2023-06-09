

test_that("Carbon stock correctelly estimated", {

  df<-data.frame(CoreID="A", DMin=c(0,20,40,60,80), DMax=c(20,40,60,80,100), DBD=1, fOC=2)


  expect_equal(estimate_stock(df)[1,2]==1.6, TRUE)


})

test_that("Carbon stock estimated for 4 cores", {
df<- exampledata2
expect_equal(estimate_stock(df)[1,2]==3.210010, TRUE)})
