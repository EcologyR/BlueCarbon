#' Organic carbon estimation
#'
#' @description Model linear relation between organic matter and organic carbon and estimate organic carbon values from organic matter data
#'
#' @param df A [data.frame] with, at least, columns Core.ID, Ecosystem, Specie, Site.ID, OM, and OC.
#'
#' @return the initial [data.frame] + one column with organic carbon values
#' @export
#'
#' @examples

transform_om_oc <- function(df = NULL) {

  #### Estimate df linear model to predict OC from OM for each ecosystem, specie and station ###
  #skip those models with R2<0.5 or P value>0.05

  # check if the class of the parameters objects, the names and class of the columns of df are correct

  # class of the dataframe
  if (is.data.frame(df)==FALSE) {stop("The data provided is not class data.frame, please chaeck data and transforme")}

  # name of the columns
  if ("Site.ID" %in% colnames(df)==FALSE) {stop("There is not column named Site.ID. Please, check necesary columns in functions documentation")}
  if ("Core.ID" %in% colnames(df)==FALSE) {stop("There is not column named Core.ID. Please, check necesary columns in functions documentation")}
  if ("Ecosystem" %in% colnames(df)==FALSE) {stop("There is not column named Ecosystem. Please, check necesary columns in functions documentation")}
  if ("Specie" %in% colnames(df)==FALSE) {stop("There is not column named Specie. Please, check necesary columns in functions documentation")}
  if ("OM" %in% colnames(df)==FALSE) {stop("There is not column named OM. Please, check necesary columns in functions documentation")}
  if ("OC" %in% colnames(df)==FALSE) {stop("There is not column named OC. Please, check necesary columns in functions documentation")}

  # class of the columns
  if (is.numeric(df$OM)==FALSE) {stop("Organic matter data is not class numeric, please chaeck")}
  if (is.numeric(df$OC)==FALSE) {stop("Organic carbon data is not class numeric, please chaeck")}


  #create df list of dataframes with data from each ecosystem, specie, and station (site)
  table(df$Ecosystem)
  X<-split(df, df$Ecosystem)
  X2<-split(df, df$Specie)
  X3<-split(df, df$Site.ID)
  X<-c(X,X2,X3)
  length(X)

  #create empty table to log model data
  OCEst <- data.frame(ID=character(),
                      R2=numeric(),
                      P=numeric(),
                      f=numeric(),
                      int=numeric(),
                      slope=numeric()
  )

  for(i in 1:length(X)) {
    OCEst[i,1]<-names(X[i])
    Data<-as.data.frame(X[i])
    colnames(Data)<-colnames(df)


    #we only model those ecosystem, specie, and station with more than 5 samples were OC and LOI were mwasured
    if((nrow(Data %>% filter_at(vars(OM,OC),all_vars(!is.na(.)))))<5) next


    else{

      model<-lm(OC ~ OM, data=Data)

      if(summary(model)$r.squared<0.5 | glance(model)$p.value>0.05 ) next

      else{

        OCEst[i,2]<-summary(model)$r.squared
        OCEst[i,3]<-glance(model)$p.value
        OCEst[i,4]<-summary(model)$fstatistic[1]
        OCEst[i,5]<-model$coefficients[1]
        OCEst[i,6]<-model$coefficients[2]

      }}
  }

  rownames(OCEst)<-OCEst$ID

  # write.csv(OCEst,file.path(Folder,"OM-OC_lm.csv"),sep=";", dec=",")

  ## Use the models estimated to predict OC from OM content for those samples with no OC data
  #If there is OC data for that sample, keep original OC data,
  #else use function estimated for that Site.ID
  #else function for specie
  #else function for ecosystem

  df$POC <- NA

  for(i in 1:nrow(df)) {

    if (is.na(df[i,which( colnames(df)=="OC" )])==FALSE)
    {df[i,which( colnames(df)=="POC" )]<-df[i,which( colnames(df)=="OC" )]}

    else { if (is.na(OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Site.ID" )])),which(colnames(OCEst)=="int")])==FALSE)

    {df[i,which( colnames(df)=="POC" )]<-
      OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Site.ID" )])),which(colnames(OCEst)=="int" )]+
      (OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Site.ID" )])),which(colnames(OCEst)=="slope" )])*
      df[i,which( colnames(df)=="OM" )] }

      else{ if (is.na(OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Specie" )])),which(colnames(OCEst)=="int")])==FALSE)

      {df[i,which( colnames(df)=="POC" )]<-
        OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Specie" )])),which(colnames(OCEst)=="int" )]+
        (OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Specie" )])),which(colnames(OCEst)=="slope" )])*
        df[i,which( colnames(df)=="OM" )]}

        else {df[i,which( colnames(df)=="POC" )]<-
          OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Ecosystem" )])),which(colnames(OCEst)=="int" )]+
          (OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Ecosystem" )])),which(colnames(OCEst)=="slope" )])*
          df[i,which( colnames(df)=="OM" )]}}}

  }

  ## when OM very low, the estimation can give negative values of OC. We change negative values for 0.

  df$POC[df$POC < 0] <- 0

  message(
    paste("Howard et al (2014) applied to",
          sum(is.na(df$POC)), "observations")
  )

  df <- df |>
    mutate(
      POC = case_when(
        is.na(POC) & OM <= 0.2 & Ecosystem == "Seagrass" ~
          0.4 * OM - 0.21,
        is.na(POC) & OM > 0.2 & Ecosystem == "Seagrass" ~
          0.43 * OM - 0.33,
        is.na(POC) & Ecosystem == "Salt Marsh" ~
          0.47 * OM + 0.0008 * OM^2,
        T ~ POC
      )
    )

  return(df)

}
