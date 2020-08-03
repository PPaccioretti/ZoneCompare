makeMeanComparisson <-
  function(VariableEstudiada,
           Clasif,
           numCluster,
           EstDescr = EstDesc ,
           alpha = 0.05,
           retDMS = FALSE) {
    
    # browser()
    
    
    Medias <-
      Clasif %>%
      group_by(!!rlang::sym(numCluster)) %>%
      summarize_at(VariableEstudiada, mean)
    ## Test ----
    
    MisMedias <- Medias[, c(numCluster, VariableEstudiada)] %>%
      arrange(!!rlang::sym(VariableEstudiada)) #%>%
    # mutate(DifSign = do.call(rbind, Krig)[ VariableEstudiada, 2] * 1.96 * 2)
    
    
    
    comb <- utils::combn(nrow(MisMedias), 2)
    nn <- ncol(comb)
    dif <- rep(0, nn)
    pvalue <- dif
    sdtdif <- dif
    sig <- rep(" ", nn)
    DMS <- EstDescr[VariableEstudiada, 2] * qnorm(1 - alpha / 2) * 2
    
    for (k in 1:nn) {
      i <- comb[1, k]
      j <- comb[2, k]
      dif[k] <- st_drop_geometry(MisMedias[i, 2]) - st_drop_geometry(MisMedias[j, 2])
      sdtdif[k] <- EstDescr[VariableEstudiada, 2]
      pvalue[k] <- abs(dif[k][[1]]) <= DMS
    }

    MisLetras <-
      CalcLetras(MisMedias = MisMedias,
                 pvalue = pvalue,
                 alpha = alpha)
    if (retDMS) {
      return(list("TablaMedias" = MisLetras, "DMS" = DMS))
    }
    MisLetras
  }

VarKrigDescr <-
  function(ClustersFM,
           datosAValid,
           crs) {
    # browser()
    # datos_predsf <- st_as_sf(ClustersFM, coords = 1:2, crs = crs)
    datos_predsf <- ClustersFM
    Krig <-
      sapply(datosAValid, function(columna) {
        library(gstat)
        # browser()
        if (columna == "geometry") {
          return(NULL)
        }
        
        formulaKr <- as.formula(paste(columna, "~ 1"))
        
        # Ajuste de semivariograma empírico
        semiv_emp <- variogram(formulaKr, datos_predsf)
        
        # Ajuste de semivariograma teóricos sin valores iniciales de los parámetros
        modelos <-
          fit.variogram(semiv_emp, vgm(c("Exp", "Sph", "Gau")))
        kriging_cv <-
          krige.cv(
            formulaKr,
            datos_predsf,
            nfold = 10,
            nmin = 7,
            nmax = 25,
            model = modelos
          )
        
        media <- mean(datos_predsf[[columna]], na.rm = T)
        Cv <-
          sd(datos_predsf[[columna]], na.rm = T) / media * 100
        tamPunt <- sum(!is.na(datos_predsf[[columna]]))
        data.frame(
          "Variable" = as.character(columna),
          "MedianaK" = median(sqrt(kriging_cv$var1.var), na.rm = T),
          "MediaVar" = media,
          "CVVar" = Cv,
          "n" = tamPunt
        )
        
      }, simplify = FALSE)
    
    #### Est Descriptiva  ----
    # AreaP <- datos_predsf %>%
    #   summarise(geometry = st_combine(geometry)) %>%
    #   st_convex_hull() %>%
    #   st_area()
    # # browser()
    # AreaHa <- AreaP / 10000
    # browser()
    EstDesc <- do.call(rbind, Krig)
    EstDesc
  }


ValidVarKrig <-
  function(ClustersFM,
           datosAValid,
           numCluster,
           crs,
           EstDesc,
           ndeci = 2) {
    library(data.table)
    MedCV <-
      paste0(round(EstDesc$MediaVar, 2), " (", round(EstDesc$CVVar, 1), ")")
    MedCV <-
      data.table("Variable" = EstDesc$Variable,
                 MedCV,
                 "n" = EstDesc$n)
    
    EstDescTable <- dcast(data = MedCV,
                          n ~ Variable ,
                          value.var = "MedCV")
    
    ##### ComparacionMedias ----
    
    MeansComparissons <-
      sapply(
        datosAValid,
        makeMeanComparisson,
        Clasif = ClustersFM,
        numCluster = numCluster,
        EstDescr = EstDesc,
        retDMS = FALSE,
        simplify = FALSE
      )
    
    return(list(Diferencias = MeansComparissons,
                Descriptivo = EstDescTable))
  }


CalcLetras <- function(MisMedias, pvalue, alpha = 0.05) {
  #Adapted from agricolae package
  lastC <-
    function(x) {
      y<-sub(" +$", "",x)
      p1<-nchar(y)
      cc<-substr(y,p1,p1)
      return(cc)
    }

  Qm <- matrix(1, ncol = nrow(MisMedias), nrow = nrow(MisMedias))
  p <- pvalue
  k <- 0
  for (i in 1:(nrow(MisMedias) - 1)) {
    for (j in (i + 1):nrow(MisMedias)) {
      k <- k + 1
      Qm[i, j] <- p[k]
      Qm[j, i] <- p[k]
    }
  }
  
  
  
  n <- nrow(MisMedias)
  z <- MisMedias
  letras<-c(letters[1:26],LETTERS[1:26],1:9,c(".","+","-","*","/","#","$",
                                              "%","&","^","[","]",":","@",";","_","?","!","=","#",rep(" ",2000)))
  w <- z[order(z[[ 2]], decreasing = FALSE), ]
  M<-rep("",n)
  k<-1
  k1<-0
  j<-1
  i<-1
  cambio<-n
  cambio1<-0
  chequeo=0
  M[1]<-letras[k]
  q <- as.numeric(rownames(w)) #Check
  while(j<n) {
    chequeo<-chequeo+1
    if (chequeo > n) break
    for(i in j:n) {
      # browser()
      stest <- Qm[q[i],q[j]]>alpha
      if(stest) {
        if(lastC(M[i]) != letras[k]) {M[i]<-paste(M[i],letras[k],sep="")}
      }
      else {
        k<-k+1
        cambio<-i
        cambio1<-0
        ja<-j
        for(jj in cambio:n) M[jj]<-paste(M[jj],"",sep="") # El espacio
        M[cambio]<-paste(M[cambio],letras[k],sep="")
        for( v in ja:cambio) {
          if(Qm[q[v],q[cambio]]<=alpha) {j<-j+1
          cambio1<-1
          }
          else break
        }
        break
      }
    }
    if (cambio1 ==0 )j<-j+1
  }
  #-----------
  # browser()
  w<-data.frame(w,stat=M)
  if(k>81) 
    cat("\n",k,"groups are estimated.The number of groups exceeded the maximum of 81 labels. change to group=FALSE.\n")
  invisible(w)
}

