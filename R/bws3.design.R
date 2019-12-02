bws3.design<-function(
  bibd,
  ffd,
  attribute.levels)
{
  nALT    <- ncol(bibd)
  nQ      <- nrow(bibd)
  nA      <- ncol(ffd)
  AL      <- attribute.levels
  bibd.dm <- data.matrix(bibd)
  ffd.df  <- data.frame(data.matrix(ffd))

  for (i in 1:nA) {
    ffd.df[,i] <- factor(ffd.df[,i], labels = AL[[i]])
  }
  colnames(ffd.df) <- names(AL) 
  alts <- vector("list", nALT)
  for (i in 1:nALT) {
    alts[[i]] <-data.frame(BLOCK = 1,
                            QES = 1:nQ,
                            ALT = i,
                            ffd.df[bibd.dm[, i], ])
    names(alts)[i] <- paste("alt.", i, sep = "")
  }

  inf <- list(nalternatives = nALT,
              nblocks       = 1,
              nquestions    = nQ,
              nattributes   = nA)

  rtn <- list(alternatives       = alts,
              candidate          = NULL,
              design.information = inf)
  class(rtn) <- c("cedes", "list")

  rtn
}

