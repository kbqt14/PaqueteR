#' Reparacion del set de datos de clorofila
#'
#' @param chla data.frame
#'
#' @return data.frame arreglado
#' @export
#'
#' @examples
#'
#' fixChlorophyllData(badChlorophyllDataFrame)
#'
fixChlorophyllData <- function(chla){


  actualYear <- chla$Year[1]

  chla$Date <- ymd("1900-01-01")

  for(i in 1:nrow(chla)){

    if(is.na(chla$Year[i])) {

      chla$Year[i] <- actualYear

    } else {

      actualYear <- chla$Year[i]
    }

    fechaTmp <- ymd( paste(chla$Year[i], chla$Month[i], 1 ) )

    if(is.na(fechaTmp)) {

      if(chla$Month[i]=="Mar" ) {

        fechaTmp <- ymd( paste(chla$Year[i], 3, 1 ) )

      } else if(chla$Month[i]=="Ago" ) {

        fechaTmp <- ymd( paste(chla$Year[i], 8, 1 ) )

      } else {

        fechaTmp <- dmy(chla$Month[i])
      }
    }

    chla$Date[i] <- fechaTmp

  }


  chla$IntegE1 <- abs(chla$IntegE1)

  chla$IntegE2 <- abs(as.numeric(chla$IntegE2))


  return(chla)
}
