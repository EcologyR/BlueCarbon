
test_that("Carbon flux correctelly estimated", {

  df<-data.frame(Core.ID="A", Min.D=c(0,20,40,60,80), Max.D=c(20,40,60,80,100), DBD=1, POC=2, Age=c(20,40,60,80,110))


  expect_equal(estimate_flux(df)[1,4]==0.01533333, TRUE)
})
