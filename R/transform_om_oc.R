#' Organic carbon estimation
#'
#' @description Model linear relation between organic matter and organic carbon and estimate organic carbon values from organic matter data
#'
#' @param df A tibble or data.frame with, at least, columns CoreID, Ecosystem, Species, SiteID, OM, and OC.
#' #### FRS: An alternative is to let the user specify the column names for each of these things
#' @param num_sample
#' @param Rsq
#' @param Pval
#'
#' @return the initial data.frame + one column with organic carbon values (fOC = final organic carbon)
#' @export
#'
#' @examples transform_om_oc(A, r_squared = 0.8, p_value = 0.05)
#' @examples transform_om_oc(A)

transform_om_oc <- function(df = NULL, num_sample = 10, r_squared = 0.5, p_value = 0.05) {

  #### Estimate df linear model to predict OC from OM for each ecosystem, species and station ###
  #skip those models with R2<0.5 or P value>0.05
  #### FRS: Why?

  # check if the class of the parameters objects, the names and class of the columns of df are correct

  # Note df could be a tibble too
  if (!inherits(df, "data.frame")) {
    stop("The data provided must be a tibble or data.frame")
  }

  # name of the columns
  if (!"SiteID" %in% names(df)) {stop("There must be a column named 'SiteID'")}
  if (!"CoreID" %in% names(df)) {stop("There must be a column named 'CoreID'")}
  if (!"Ecosystem" %in% names(df)) {stop("There must be a column named 'Ecosystem'")}
  if (!"Species" %in% names(df)) {stop("There must be a column named 'Species'")}
  if (!"OM" %in% names(df)) {stop("There must be a column named 'OM'")}
  if ("OC" %in% names(df)) {stop("There must be a column named 'OC'")}

  # class of the columns
  if (!is.numeric(df$OM)) {stop("Organic matter data is not class numeric, please check")}
  if (!is.numeric(df$OC)) {stop("Organic carbon data is not class numeric, please check")}


  #create list of dataframes with data from each ecosystem, species, and station (site)
  X <- split(df, df$Ecosystem)
  X2 <- split(df, df$Specie)
  X3 <- split(df, df$SiteID)
  X <- c(X, X2, X3)


  #create empty table to log model data
  OCEst <- data.frame(ID = character(),
                      R2 = numeric(),
                      P = numeric(),
                      f = numeric(),
                      int = numeric(),
                      slope = numeric()
                      )

  for (i in seq_along(X)) {
    OCEst[i,1] <- names(X[i])
    Data <- as.data.frame(X[i])
    names(Data) <- names(df)


    #we only model those ecosystem, species, and station with more than 10 samples where OC and LOI were measured
    if((nrow(Data |> dplyr::filter_at(dplyr::vars(OM,OC),dplyr::all_vars(!is.na(.)))))< num_sample) next


    else{

      model<-lm(OC ~ OM, data=Data)

      if(summary(model)$r.squared<r_squared | broom::glance(model)$p.value>p_value ) next

      else{

        OCEst[i,2]<-summary(model)$r.squared
        OCEst[i,3]<-broom::glance(model)$p.value
        OCEst[i,4]<-summary(model)$fstatistic[1]
        OCEst[i,5]<-model$coefficients[1]
        OCEst[i,6]<-model$coefficients[2]

      }}
  }

  rownames(OCEst)<-OCEst$ID

  # write.csv(OCEst,file.path(Folder,"OM-OC_lm.csv"),sep=";", dec=",")

  ## Use the models estimated to predict OC from OM content for those samples with no OC data
  #If there is OC data for that sample, keep original OC data,
  #else use function estimated for that SiteID
  #else function for specie
  #else function for ecosystem

  df$fOC <- NA

  for(i in 1:nrow(df)) {

    if (is.na(df[i,which( colnames(df)=="OC" )])==FALSE)
    {df[i,which( colnames(df)=="fOC" )]<-df[i,which( colnames(df)=="OC" )]}

    else { if (is.na(OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="SiteID" )])),which(colnames(OCEst)=="int")])==FALSE)

    {df[i,which( colnames(df)=="fOC" )]<-
      OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="SiteID" )])),which(colnames(OCEst)=="int" )]+
      (OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="SiteID" )])),which(colnames(OCEst)=="slope" )])*
      df[i,which( colnames(df)=="OM" )] }

      else{ if (is.na(OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Specie" )])),which(colnames(OCEst)=="int")])==FALSE)

      {df[i,which( colnames(df)=="fOC" )]<-
        OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Specie" )])),which(colnames(OCEst)=="int" )]+
        (OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Specie" )])),which(colnames(OCEst)=="slope" )])*
        df[i,which( colnames(df)=="OM" )]}

        else {df[i,which( colnames(df)=="fOC" )]<-
          OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Ecosystem" )])),which(colnames(OCEst)=="int" )]+
          (OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Ecosystem" )])),which(colnames(OCEst)=="slope" )])*
          df[i,which( colnames(df)=="OM" )]}}}

  }

  ## when OM very low, the estimation can give negative values of OC. We change negative values for 0.

  df$fOC[df$fOC < 0] <- 0


  if (sum(is.na(df$fOC) & !is.na(df$OM))>=1) {
  message(
    paste("Howard et al (2014) applied to",
          sum(is.na(df$fOC) & !is.na(df$OM) ), "observations")
  )}

  df <- df |>
    dplyr::mutate(
      fOC = dplyr::case_when(
        is.na(fOC) & OM <= 0.2 & Ecosystem == "Seagrass" ~
          0.4 * OM - 0.21,
        is.na(fOC) & OM > 0.2 & Ecosystem == "Seagrass" ~
          0.43 * OM - 0.33,
        is.na(fOC) & Ecosystem == "Salt Marsh" ~
          0.47 * OM + 0.0008 * OM^2,
        is.na(fOC) & Ecosystem == "Mangrove" ~
          0.415 * OM - 2.89,
        T ~ fOC
      )
    )

  return(df)
  #return(OCEst)

}
