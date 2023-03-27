#' Test differences between observed and extrapolated stocks
#'
#' @description subset those cores that reach the standardization depth and estimates the stock (observed stock), estimate the stock from the linear relation of organic carbon accumulated mass and depth using the 90, 75, 50 and 25% top length of the indicated standardization depth. Compares the observed stock with the estimated stocks by extrapolation.
#'
#' @param df A [data.frame] with, at least, columns: Core.ID, Min.D (minimum depth of the sample), Max.D (maximum depth of the sample), DBD (dry bulk density), POC (organic carbon %)
#' @param Depth standardization soil depth, by default 100 cm.
#'
#' @return A [data.frame] with the observed and extrapolated stocks. A plot with comparisons.
#' @export
#'
#' @examples

test_extrapolation <- function(df = NULL, Depth = 100) {
  # class of the dataframe
  if (is.data.frame(df) == FALSE) {
    stop("The data provided is not class data.frame, please chaeck data and transforme")
  }
  if (is.numeric(Depth) == FALSE) {
    stop("The Depth provided is not class numeric, please chaeck data and transforme")
  }

  # name of the columns
  if ("CoreID" %in% colnames(df) == FALSE) {
    stop(
      "There is not column named CoreID. Please, check necesary columns in functions documentation"
    )
  }
  if ("Min.D" %in% colnames(df) == FALSE) {
    stop(
      "There is not column named Min.D. Please, check necesary columns in functions documentation"
    )
  }
  if ("Max.D" %in% colnames(df) == FALSE) {
    stop(
      "There is not column named Max.D. Please, check necesary columns in functions documentation"
    )
  }
  if ("DBD" %in% colnames(df) == FALSE) {
    stop(
      "There is not column named DBD. Please, check necesary columns in functions documentation"
    )
  }
  if ("POC" %in% colnames(df) == FALSE) {
    stop(
      "There is not column named POC. Please, check necesary columns in functions documentation"
    )
  }

  # class of the columns
  if (is.numeric(df$Min.D) == FALSE) {
    stop("Minimum depth data is not class numeric, please chaeck")
  }
  if (is.numeric(df$Max.D) == FALSE) {
    stop("Maximum depth data is not class numeric, please chaeck")
  }
  if (is.numeric(df$DBD) == FALSE) {
    stop("Dry Bulk Density data is not class numeric, please chaeck")
  }
  if (is.numeric(df$POC) == FALSE) {
    stop("Organic carbon data is not class numeric, please chaeck")
  }


  df <- df |> mutate (Center = Min.D + ((Max.D - Min.D) / 2))
  X <- split(df, df$CoreID)

  ExtS <- data.frame(
    CoreID = character(),
    S.1m = numeric(),
    EXT.90 = numeric(),
    EXT.75 = numeric(),
    EXT.50 = numeric(),
    EXT.25 = numeric()
  )



  for (i in 1:length(X)) {
    ExtS[i, 1] <- names(X[i])
    Data <- as.data.frame(X[i])
    colnames(Data) <- colnames(df)

    Data = filter(Data,!is.na(POC))

    if (nrow(Data) < 3)
      next


    else {
      Data$h <- NA

      #estimation of the thickness of the sample (h)
      for (j in 2:(nrow(Data) - 1)) {
        Data[j, which(colnames(Data) == "h")] <-
          ((Data[j, which(colnames(Data) == "Center")] - Data[(j - 1), which(colnames(Data) ==
                                                                               "Center")]) / 2) +
          ((Data[(j + 1), which(colnames(Data) == "Center")] - Data[j, which(colnames(Data) ==
                                                                               "Center")]) / 2)

      }

      #estimation of carbon g cm2 per sample, OCgcm2= carbon density (g cm3) by thickness (h)
      Data[1, which(colnames(Data) == "h")] <-
        Data[1, which(colnames(Data) == "Center")]
      Data[nrow(Data), which(colnames(Data) == "h")] <-
        Data[nrow(Data), which(colnames(Data) == "Max.D")] - Data[nrow(Data), which(colnames(Data) ==
                                                                                      "Center")]
      Data <- Data |> mutate (OCgcm2 = DBD * (POC / 100) * h)


      #For those cores longer than the extrapolation depth we estimate stock the observed stock at that depth and from linear models
      # using data from the 90, 75, 50 and 25 % of the extrapolation depth
      if (max(Data$Max.D) <= Depth) next


        if (max(Data$Max.D) == Depth) {
          Data <-
            Data
        } else {
          Data <- Data[c(1:(length(which(
            Data$Max.D <= Depth
          )) + 1)), ]
        }


        ExtS[i, 2] <-
          (((sum(Data[c(1:(nrow(Data) - 1)), which(colnames(Data) == "OCgcm2")])) +
              (Data[nrow(Data), which(colnames(Data) == "OCgcm2")] /
                 ((max(Data$Max.D) - Data[(nrow(Data) - 1), which(colnames(Data) == "Max.D")])
                 )
               * (
                 Depth - Data[(nrow(Data) - 1), which(colnames(Data) == "Max.D")]
               ))))

        Data <- Data |> mutate (OCM = cumsum(OCgcm2))

        ninety <- Depth * 0.9 #90% of the extrapolation length
        seventy <- Depth * 0.75 #90% of the extrapolation length
        fifhty <- Depth * 0.50 #90% of the extrapolation length
        twentififty <- Depth * 0.25 #90% of the extrapolation length


        Data <- subset(Data, Data$Max.D <= ninety)

        if (nrow(Data) > 3) {
          model90 <- lm(OCM ~ Max.D, data = Data)
          ExtS[i, 3] <- coef(model90)[1] + 100 * coef(model90)[2]
        }

        Data <- subset(Data, Data$Max.D <= seventy)
        if (nrow(Data) > 3) {
          model75 <- lm(OCM ~ Max.D, data = Data)
          ExtS[i, 4] <- coef(model75)[1] + 100 * coef(model75)[2]
        }

        Data <- subset(Data, Data$Max.D <= fifhty)
        if (nrow(Data) > 3) {
          model50 <- lm(OCM ~ Max.D, data = Data)
          ExtS[i, 5] <- coef(model50)[1] + 100 * coef(model50)[2]
        }

        Data <- subset(Data, Data$Max.D <= twentififty)
        if (nrow(Data) > 3) {
          model25 <- lm(OCM ~ Max.D, data = Data)
          ExtS[i, 6] <- coef(model25)[1] + 100 * coef(model25)[2]
        }
    }
  }

  #############
  # we test the correlation between the stock at 1m estimated from real data and the models
  # and the error of the models
  ##############


  CorMat <- cor(na.omit(ExtS[, c(2:6)]), method = "pearson")


  corrplot::corrplot(
    CorMat,
    type = "upper",
    order = "hclust",
    tl.col = "black",
    tl.srt = 45
  )

  #write.csv(CorMat,file.path(Folder,"Corr.Extrapolation.csv"),sep=";", dec=",")


  ExtS <- ExtS |> mutate (Error.90 = (abs(S.1m - EXT.90) * 100) / S.1m)
  ExtS <- ExtS |> mutate (Error.75 = (abs(S.1m - EXT.75) * 100) / S.1m)
  ExtS <- ExtS |> mutate (Error.50 = (abs(S.1m - EXT.50) * 100) / S.1m)
  ExtS <- ExtS |> mutate (Error.25 = (abs(S.1m - EXT.25) * 100) / S.1m)

  summary(ExtS)


  #Global Error
  m.ExtS <- ExtS[, c(1, 7:10)]
  m.ExtS <- reshape::melt(m.ExtS, id = c("CoreID"))

  P1<-ggplot2::ggplot(m.ExtS, aes(variable, value)) + ylab("% of deviation from observed value") + xlab("% of standar depth") +
    geom_boxplot() +
    geom_jitter() +
    scale_x_discrete(labels=c("Error.90" = "90%", "Error.75" = "75%", "Error.50" = "50%", "Error.25" = "25%"))+
    theme(
      #axis.title.x = element_blank(),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

  P2<-ggplot2::ggplot(ExtS, aes(ExtS[, 2], ExtS[, 3])) + xlab("Observed Stock") + ylab("Extrapolated stock") +
    geom_point(aes(ExtS[, 2], ExtS[, 3], color = "90%"), size = 2) +
    geom_point(aes(ExtS[, 2], ExtS[, 4], color = "75%"), size = 2) +
    geom_point(aes(ExtS[, 2], ExtS[, 5], color = "50%"), size = 2) +
    geom_point(aes(ExtS[, 2], ExtS[, 6], color = "25%"), size = 2) +
    theme(text = element_text(size = 15)) +
    #geom_text_repel(aes(label=A[,1]), size=4)+
    xlim(0, 5) + ylim(0, 5) +
    geom_abline()

Extrapolation_plot<-gridExtra::grid.arrange(P1,P2, ncol=2)

return(Extrapolation_plot)

}
