

test_that("Carbon stock correctelly estimated", {

  df<-data.frame(Core.ID="A", Min.D=c(0,20,40,60,80), Max.D=c(20,40,60,80,100), DBD=1, POC=2)


  expect_equal(BCS[1,2]==2, TRUE)
})
