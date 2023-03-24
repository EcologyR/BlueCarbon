#' Estimate the average organic carbon flux
#'
#' @description estimate the average organic carbon flux to the soil in a indicated time frame (by default last 100 years) from the organic carbon concentration and ages obtained from a age-depth or age-accumulated mass model
#'
#' @param df A data frame with columns: Core.ID, Min.D (minimum depth of the sample), Max.D (maximum depth of the sample), DBD (dry bulk density), POC (organic carbon %), Age (age of the sample obtained from a age-depth or age-accumulated mass model)
#' @param TimeFrame standardization time frame, by default 100 years
#'
#' @return data frame with columns Core.id, F.WC (organic carbon fluxes at the whole core), A.Max (maximum age of the core), and Flux (average organic carbon flux at the indicated time frame)
#' @export
#'
#' @examples


estimate_flux<- function(df=NULL,TimeFrame=100) {


  df <-df %>% mutate (Center = Min.D+((Max.D-Min.D)/2))

  #select those cores with chronological models
  AD = filter(df, !is.na(Age))

  length(unique(AD$Core.ID))# number of cores with age estimations

  X<-split(AD, AD$Core.ID)

  BCF <- data.frame(Core.ID=character(),
                    F.WC=numeric(),
                    A.Max=numeric(),
                    Flux=numeric())


  for(i in 1:length(X)) {
    BCF[i,1]<-names(X[i])
    Data<-as.data.frame(X[i])
    colnames(Data)<-colnames(AD)


    Data = filter(Data, !is.na(POC))

    if(nrow(Data)<3) next

    else{

      Data$h<-NA

      #estimation of the thickness of the sample (h)
      for (j in 2:(nrow(Data)-1)) {

        Data[j,which(colnames(Data)=="h")]<-
          ((Data[j,which(colnames(Data)=="Center")]-Data[(j-1),which(colnames(Data)=="Center")])/2)+
          ((Data[(j+1),which(colnames(Data)=="Center")]-Data[j,which(colnames(Data)=="Center")])/2)
      }

      #estimation of carbon g cm2 per sample, OCgcm2= carbon density (g cm3) by thickness (h)
      Data[1,which(colnames(Data)=="h")]<-Data[1,which(colnames(Data)=="Center")]
      Data[nrow(Data),which(colnames(Data)=="h")]<-
        Data[nrow(Data),which(colnames(Data)=="Max.D")]-Data[nrow(Data),which(colnames(Data)=="Center")]

      Data <-Data %>% mutate (OCgcm2 = DBD*(POC/100)*h)

      #estimation of the average carbon flux for the whole core (OC stock/Max Age)
      BCF[i,2]<-(sum(Data[,which(colnames(Data)=="OCgcm2")]))/max(Data$Age)
      BCF[i,3]<-max(Data$Age)

      #estimation of the average carbon flux for the selected TimeFrame (OC stock last 100 yrs/TimeFrame)
      Data<-Data[c(1:(length(which(Data$Age <=TimeFrame))+1)),]

      BCF[i,4]<-((((sum(Data[c(1:(nrow(Data)-1)),which(colnames(Data)=="OCgcm2")]))+
                     (Data[nrow(Data),which(colnames(Data)=="OCgcm2")]/((max(Data$Age)-Data[(nrow(Data)-1),which(colnames(Data)=="Age")]))
                      *(TimeFrame-Data[(nrow(Data)-1),which(colnames(Data)=="Age")]))))/TimeFrame)

    }}
  return(BCF)
}
