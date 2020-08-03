Depuracion <-
  function(sf_datos,
           VarADep,
           mDepuration = c("Outliers", "Inliers"),
           SacoBordes = TRUE,
           Buffer = 20,
           ylimitmax = NA,
           ylimitmin = 0,
           DEOut = 3,
           VecinosMin = 7,
           VecinosMax = 25) {
    library(sp)
    library(raster)
    library(spdep)
# browser()
  moran.plot1 <-function (x, listw, zero.policy = NULL, spChk = NULL, labels = NULL,
                          xlab = NULL, ylab = NULL, quiet = NULL, ...){
    # browser()
    if (!inherits(listw, "listw"))
      stop(paste(deparse(substitute(listw)), "is not a listw object"))
    if (is.null(quiet))
      quiet <- !get("verbose", envir = .spdepOptions)
    stopifnot(is.vector(x))
    stopifnot(is.logical(quiet))
    if (is.null(zero.policy))
      zero.policy <- get("zeroPolicy", envir = .spdepOptions)
    stopifnot(is.logical(zero.policy))
    xname <- deparse(substitute(x))
    if (!is.numeric(x))
      stop(paste(xname, "is not a numeric vector"))
    if (any(is.na(x)))
      stop("NA in X")
    n <- length(listw$neighbours)
    if (n != length(x))
      stop("objects of different length")
    if (is.null(spChk))
      spChk <- get.spChkOption()
    if (spChk && !chkIDs(x, listw))
      stop("Check of data and weights ID integrity failed")
    labs <- TRUE
    if (is.logical(labels) && !labels)
      labs <- FALSE
    if (is.null(labels) || length(labels) != n)
      labels <- as.character(attr(listw, "region.id"))
    wx <- lag.listw(listw, x, zero.policy = zero.policy)
    if (is.null(xlab))
      xlab <- xname
    if (is.null(ylab))
      ylab <- paste("spatially lagged", xname)
    if (zero.policy) {
      n0 <- wx == 0
      if (any(n0)) {
        symbols(x[n0], wx[n0], inches = FALSE, circles = rep(diff(range(x))/50,
                                                             length(which(n0))), bg = "grey", add = TRUE)
      }
    }
    xwx.lm <- lm(wx ~ x)
    infl.xwx <- influence.measures(xwx.lm)
    is.inf <- which(apply(infl.xwx$is.inf, 1, any))
    if (!quiet)
      summary(infl.xwx)
    invisible(infl.xwx)
  }
  

  MyFile <- sf_datos  
  
  DatosCrudos <- data.frame(st_coordinates(MyFile), MyFile[[VarADep]])
  
  # DatosCrudos <- MyFile[,c(input$xmapa, input$ymapa, input$rto)]
  MyFile <- DatosCrudos[complete.cases(MyFile[[VarADep]]),]

  colnames(MyFile)[1] <- paste("X")
  colnames(MyFile)[2] <- paste("Y")
  colnames(MyFile)[3] <- paste("Z")

  mapa <- MyFile[,c(1:3)]
  mapa$Filas <- 1:nrow(mapa)

  DatosOrig <- mapa

  Duplicados <- duplicated(mapa[,c(1:3)])
  condicion <- data.frame("Filas" = mapa$Filas, "Duplicated" = Duplicados)
  mapa <- mapa[!Duplicados,]

  Logi0 <- TRUE %in% c(mDepuration %in% c("Outliers", "Inliers"))
  if (as.logical(Logi0))  {
    if (SacoBordes) {
      borde <- mapa[chull(mapa[,1:2]),1:2]
      pol <- Polygons(list(Polygon(borde)),as.character(dim(borde)[1]))
      sr <- SpatialPolygons(list(pol))
      bufferValue <- -abs(as.numeric(as.character(Buffer)))
      buff <- buffer(sr, width = bufferValue, dissolve = T)

      Border <- SpatialPoints(mapa[,1:2]) %over% buff
      datos <- mapa[complete.cases(Border),]

      Border <- data.frame("Filas" = names(Border), Border)
      Border[,2] <- addNA(Border[,2])
      levels(Border[,2]) <- c("FALSE", "TRUE")
      condicion <- merge(condicion,Border, all.x = TRUE)

      condicion <- unique(condicion)


    } else {datos <- mapa}



    Logi <- TRUE %in% c(mDepuration == "Outliers")
    if (as.logical(Logi)) {
      if (is.na(ylimitmax) | is.na(ylimitmin)) {
        if (is.na(ylimitmax)) {
          GlobalMin <- mapa$Z <= ylimitmin
          Limitado <- data.frame("Filas" = mapa$Filas, GlobalMin)
          datos <- subset(datos, datos$Z > ylimitmin)
        }
        if (is.na(ylimitmin)) {
          GlobalMax <- mapa$Z >= ylimitmax
          Limitado <- data.frame("Filas" = mapa$Filas, GlobalMax)
          datos <- subset(datos, mapa$Z < ylimitmax)
        }
      } else {
        GlobalMin <- mapa$Z <= ylimitmin
        GlobalMax <- mapa$Z >= ylimitmax
        Limitado <- data.frame("Filas" = mapa$Filas, GlobalMin, GlobalMax)
        datos <- subset(datos, datos$Z > ylimitmin &  datos$Z < ylimitmax)
      }


      LI <- mean(datos$Z, na.rm = TRUE) - DEOut * sqrt(var(datos$Z, na.rm = TRUE))
      LS <- mean(datos$Z, na.rm = TRUE) + DEOut * sqrt(var(datos$Z, na.rm = TRUE))
      Outlier <- data.frame("Filas" = datos$Filas, "Outlier" = datos$Z <= LI | datos$Z >= LS)

      datos <- subset(datos, datos$Z > LI & datos$Z < LS)


      Raritos <- data.frame(tryCatch(merge(Limitado,Outlier, all = TRUE), error = function(e) {Outlier}))
      condicion <- merge(condicion, Raritos, all.x = TRUE)
      condicion <- unique(condicion)

    }


    Logi2 <- TRUE %in% c(mDepuration %in% c("Inliers"))
    if (as.logical(Logi2)) {

      cord <- coordinates(datos[,1:2])


      gri <- dnearneigh(cord, VecinosMin, VecinosMax)
      lw <- try(nb2listw(gri, style = "W"))



      umin <- VecinosMax
      # if (TRUE %in% agrepl(lw[1],"Error in nb2listw(gri, style = W) : Empty neighbour sets found")) {
      if (any(class(lw) == "try-error")) {
        repeat { umin = umin + 10; gri <- dnearneigh(cord, VecinosMin, umin); lw <- try(nb2listw(gri, style = "W"));
        if (all(class(lw) != "try-error")) break}}

      LM <- localmoran(datos$Z,lw,p.adjust.method = "bonferroni",alternative = "less")

      datos_LM <- data.frame(datos,LM)

      MP <- moran.plot1(datos_LM$Z, lw,quiet = TRUE,labels = FALSE,
                        col = 3, xlab = "Variable", ylab = "Variable Spatially Lagged", zero.policy = FALSE)
      Influ <- MP$is.inf #Influyentes por MoranPlot
      Influ <- data.frame("IdFila" = rownames(Influ),Influ)


      MyYdepIn <- data.frame(datos_LM,Influ) #Inlier por Moran Local e Influyentes por Moran Plot


      SpMP <- !(MyYdepIn$dfb.1_ == FALSE & MyYdepIn$dfb.x == FALSE & MyYdepIn$dffit == FALSE
                & MyYdepIn$cov.r == FALSE & MyYdepIn$cook.d  == FALSE & MyYdepIn$hat == FALSE)

      datos <- subset(MyYdepIn,MyYdepIn$Ii > 0 | MyYdepIn$Pr.z...0. > 0.05 | !SpMP)[, c("X","Y","Z")]
      # datos <- datos[,c(1:3)]
      Inliers <- data.frame("Filas" = MyYdepIn$Filas,
                            "SpatialOutlier" = (MyYdepIn$Ii <= 0 | MyYdepIn$Pr.z...0. <= 0.05),
                            "SpatialOutlier_MoranPlot" = SpMP)
      condicion <- merge(condicion, Inliers, all.x = TRUE)
      condicion <- unique(condicion)
    }
  } else {datos <- mapa}


  condicion <- unique(condicion)

  MisVerd <- which(condicion[,-1, drop=FALSE] == TRUE, arr.ind = TRUE)
  # nrow(MisVerd)
  MisVerd <- MisVerd[!duplicated(MisVerd[,"row"]), ]

  CondicFinal <- tryCatch({
    MiCondic <- apply(MisVerd,1,function(x) {
      NombreCol <- colnames(condicion[,-1])
      data.frame("Filas" = x[1],"Condition" = NombreCol[x[2]])
    })

    CondicFinal <- do.call(rbind,MiCondic)
    CondicFinal <- unique(merge(DatosOrig,CondicFinal, all.x = TRUE))[,-1]
    colnames(CondicFinal)[3] <- VarADep
    CondicFinal

  }, error = function(e) {

    CondicFinal <- data.frame("X" = NA,"Y" = NA,"Z" = NA, CondFinal = NA)
    colnames(CondicFinal)[3] <- VarADep
    CondicFinal

  })

  DatosProcedimientoOriginales <- DatosOrig
  # colnames(DatosProcedimientoOriginales)

  DatosUtilizadosProcedSinOutliers_Inlierns <- CondicFinal[is.na(CondicFinal[4]),]
  # colnames(DatosUtilizadosProcedSinOutliers_Inlierns) <- colnames(data()[,c(input$xmapa, input$ymapa, input$rto)])


  if(all(is.na(DatosUtilizadosProcedSinOutliers_Inlierns))) {
    DatosProcedimiento <- DatosProcedimientoOriginales
    
  } else {
    DatosProcedimiento <- DatosUtilizadosProcedSinOutliers_Inlierns
  }
  
  DatosProcedimiento_sf <- st_as_sf(DatosProcedimiento[,-ncol(DatosProcedimiento)], coords = c("X", "Y"), crs = st_crs(sf_datos))
  DatosProcedimiento_sf <- st_join(DatosProcedimiento_sf,sf_datos, suffix = c(".Dep" ))
  
  CondicFinal_sf <- st_as_sf(CondicFinal, coords = c("X", "Y"), crs = st_crs(sf_datos))
  CondicFinal_sf <- st_join(CondicFinal_sf,sf_datos, suffix = c(".Dep" ))
  
  return(list("Datos" = DatosProcedimiento_sf, #DatosProcedimiento, #DatosUtilizadosProcedSinOutliers_Inlierns,#DatosProcedimientoOriginales,
              "CondicionDeDepuracion" = CondicFinal_sf #CondicFinal,
              # "UtilizadosDep" = DatosUtilizadosProcedSinOutliers_Inlierns,
              # "DatosOriginales"= DatosCrudos   
              ))

  
}
