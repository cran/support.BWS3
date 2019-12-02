bws3.dataset <-
function(data,
         id = "id",
         response,
         choice.sets,
         categorical.attributes = NULL,
         continuous.attributes = NULL,
         common = NULL,
         optout = FALSE,
         asc = NULL,
         model = "maxdiff",
         detail = FALSE,
         ...) {
# set variables and validate arguments -----------------------------------------
  original.data <- data
  design <- choice.sets

  nresponses <- sapply(response, length)
  if (all(nresponses == nresponses[1])) {
    nresponses <- as.vector(nresponses[1])
  } else {
    stop(message = "Components of 'response' differ in length")
  }

  id.var        <- id
  nrespondents  <- nrow(data)
  nalternatives <- design$design.information$nalternatives
  if (!is.null(common)) nalternatives <- nalternatives + 1
  if (isTRUE(optout))   nalternatives <- nalternatives + 1
  nquestions    <- design$design.information$nquestions
  nattributes   <- design$design.information$nattributes
  nblocks       <- design$design.information$nblocks

  if (model == "rank" & nalternatives != nresponses) {
    stop(message = 
     "Rank ordered model needs a full BW ranking of all presented alternatives")
  }

  attribute.var.names <- 
    colnames(design$alternatives$alt.1[, 4:c(nattributes + 3)])
  attribute.names <- vector("list", nattributes)
  names(attribute.names) <- attribute.var.names
  for (i in attribute.var.names) {
    attribute.names[[i]] <- levels(design$alternatives$alt.1[, i])
  }

  if (model == "maxdiff") {
# create data for maxdiff model ------------------------------------------------

## delete response vars that are not used in maxdiff model
    delete.response.vars <- function(x) {
      x <- x[-c(3:nresponses)]
      x
    }
    if (nresponses > 2) {
      response.bw <- lapply(response, delete.response.vars)
    } else {
      response.bw <- response
    }

## change names of response variables
    response.vars <- unlist(response.bw)

    new.response.var.names <- rbind(
      paste("RESB", 1:nquestions, sep = "."),
      paste("RESW", 1:nquestions, sep = "."))
    new.response.var.names <- as.vector(new.response.var.names)

    rsp.data <- data[, c(id.var, "BLOCK", response.vars)]
    rsp.data.wo.block <- rsp.data[, !names(rsp.data) %in% c("BLOCK")] 
    colnames(rsp.data)[colnames(rsp.data) %in% response.vars] <- 
      new.response.var.names

## reshape respondent dataset
    long.rsp.data <- reshape(rsp.data,
                             idvar = id.var,
                             varying = new.response.var.names,
                             direction = "long")
    colnames(long.rsp.data)[colnames(long.rsp.data) == "time"] <- "QES"
    rownames(long.rsp.data) <- NULL
    tmp.colnames <- colnames(long.rsp.data)
    long.rsp.data <- 
      rep(1, times = nalternatives * (nalternatives - 1)) %x% 
        data.matrix(long.rsp.data)
    colnames(long.rsp.data) <- tmp.colnames
    PAIR <- rep(1:(nalternatives*(nalternatives - 1)), 
                each = nrespondents * nquestions)
    long.rsp.data <- data.frame(long.rsp.data, PAIR)
    long.rsp.data <- long.rsp.data[order(long.rsp.data[, id.var],
                                         long.rsp.data$QES,
                                         long.rsp.data$PAIR),]

## create design matrix
### create base design matrix
    X <- design.matrix.dcm(choice.sets = design,
                           categorical.attributes = categorical.attributes,
                           continuous.attributes  = continuous.attributes,
                           common = common, optout = optout,
                           asc = asc)
    original.X <- X
    SUBQES <- rep(1:nalternatives, each = nrow(X))
    var.names <- colnames(X)
    X <- rep(1, times = nalternatives) %x% data.matrix(X)
    colnames(X) <- var.names
    X  <- data.frame(cbind(X, SUBQES))
    XB <- X[order(X$BLOCK, X$QES, X$ALT),]
    XW <- X[order(X$BLOCK, X$QES, X$SUBQES),]

    XBp <- XB[, 1:3]
    colnames(XBp)[3] <- "BEST"
    XBp$WORST <- XW[, 3]

### create design matrix for paired model
    diffX <- XB - XW
    diffX <- cbind(XBp, diffX[, !names(diffX) %in% c("BLOCK", "QES", "ALT", "SUBQES")]) 
    diffX <- diffX[!c(diffX$BEST == diffX$WORST), ]
    diffX$PAIR <- rep(1:(nalternatives * (nalternatives - 1)),
                      times = nquestions * nblocks)

## create dataset for analysis
### merge respondent dataset with desing matrix
    rtn.data <- merge(long.rsp.data, diffX, 
                      by = c("BLOCK", "QES", "PAIR"))
### create RES and STR vars
    rtn.data$RES <- as.integer(
      (rtn.data$RESB == rtn.data$BEST) & (rtn.data$RESW == rtn.data$WORST))
    rtn.data$STR <- 1000 * rtn.data[, id.var] + 10 * rtn.data$QES

    attributes.X <- attributes(original.X)
    level.var.names <- attributes.X$level.variables
    ASC.names <- attributes.X$alternative.specific.constants

    if (is.null(asc)) {
      rtn.data <- rtn.data[, c(id.var, "BLOCK", "QES", "PAIR", "BEST", "WORST",
                               "RESB", "RESW", "RES", level.var.names, "STR")]
    } else {
      rtn.data <- rtn.data[, c(id.var, "BLOCK", "QES", "PAIR", "BEST", "WORST",
                               "RESB", "RESW", "RES", ASC.names, 
                               level.var.names, "STR")]
    }
### merge rtn.data with covariates
    response.vars.original <- unlist(response)
    tmp <- 
      colnames(data)[!(colnames(data) %in% c("BLOCK", response.vars.original))]
    if (length(tmp) > 1){
      covariate.data <- data[, tmp]
      covariate.names <- tmp[-1]
      rtn <- merge(x = rtn.data, y = covariate.data, by = id.var)
    } else {
      rtn <- rtn.data
      covariate.names <- NULL
    }
    if (isTRUE(detail)) {
      rtn <- merge(x = rtn, y = rsp.data.wo.block, by = id.var)
    }
    rtn <- rtn[order(rtn[, id.var], rtn$BLOCK, rtn$QES, rtn$PAIR),]
    colnames(rtn)[colnames(rtn) %in% c("RESB", "RESW")] <- c("RES.B", "RES.W")

  } else if (model == "sequential") {
# create data for sequential model ---------------------------------------------

## change names of response variables
    response.vars <- unlist(response)
    new.res.vars <- paste("RES_ALT", 1:length(response.vars), sep =".") 
    dat1      <- data[, c(!colnames(data) %in% unlist(response))]
    dat1rsp   <- data[, response.vars]
    dat1idrsp <- data[, c(id.var, response.vars)]
    colnames(dat1rsp) <- new.res.vars
    dat1      <- cbind(dat1, dat1rsp)

## reshape respondent dataset
    dat2 <- reshape(dat1,
                    idvar = id.var,
                    varying = new.res.vars,
                    direction = "long")

## create QES, SET, and BW variables
    dat2$QES <- rep(as.integer(1:nquestions),
                    each = nrespondents * nresponses)
    dat2$SET <- rep(as.integer(1:nresponses),
                    each = nrespondents,
                    time = nquestions)
    dat2$BW  <- ifelse((dat2$SET %% 2 == 1), 1L, -1L)

## reshape reshaped-respondent dataset
### create temporary id variable for the reshaping
    dat2$tmpID <- 1:nrow(dat2)
### create alternative variables for the reshaping 
    tmpDF <- data.frame(matrix(1:nalternatives, ncol = nalternatives))
    alt.vars <- paste("ALT", 1:nalternatives, sep = ".")
    colnames(tmpDF) <- alt.vars
    dat3 <- cbind(dat2, tmpDF)
### reshape
    dat3 <- reshape(dat3,
                    idvar = "tmpID",
                    varying = alt.vars,
                    direction = "long")
    dat3 <- dat3[order(dat3[, id.var], dat3$QES, dat3$SET, dat3$ALT), ]

## create RES and STR variables
    dat3$RES <- as.integer(dat3$RES_ALT == dat3$ALT)
    dat3$STR <- 1000 * dat3[, id.var] + 10 * dat3$QES + dat3$SET

## delete selected alternatives in previous responses
    RES_ALT <- as.integer(dat3$RES_ALT)
    matALT <- matrix(1L:nalternatives,
                     nrow = nalternatives * nresponses,
                     ncol = nquestions * nrespondents)
    matRTN <- matrix(0L,
                     nrow = nalternatives * nresponses,
                     ncol = nquestions * nrespondents)

    for (i in 1:(nalternatives - 1)) {
      B <- c(rep(0L, times = i * nalternatives), RES_ALT)
      B <- B[1:length(RES_ALT)]
      matB <- matrix(B,
                     nrow = nalternatives * nresponses,
                     ncol = nquestions * nrespondents)
      matB[1:(i * nalternatives), ] <- 0L
      matTRUE <- matB == matALT
      matRTN <- matRTN + matTRUE
    }

    DELETE <- as.vector(matRTN)
    dat3$DELETE <- DELETE
    rtn.dat <- dat3[c(dat3$DELETE == 0), ]

## delete temporary variables
    rtn.dat <- rtn.dat[, !names(rtn.dat) %in% c("time", "tmpID", "DELETE")] 

## merge respondent dataset with design matrix
    tmp <- colnames(data)[!(colnames(data) %in% c("BLOCK", response.vars))]
    if (length(tmp) > 1){
      covariate.names <- tmp[-1]
    } else {
      covariate.names <- NULL
    }

    X <- design.matrix.dcm(choice.sets = design,
                           categorical.attributes = categorical.attributes,
                           continuous.attributes  = continuous.attributes,
                           common = common, optout = optout,
                           asc = asc)
    original.X <- X
    rtn <- merge(x = rtn.dat, y = X, by = c("BLOCK", "QES", "ALT"))
    tmp.var.names <- colnames(rtn)[!(colnames(rtn) %in% covariate.names)]
    tmp.var.names <- tmp.var.names[!(tmp.var.names %in% id.var)]
    rtn <- rtn[, c(id.var, tmp.var.names, covariate.names)]
    if (isTRUE(detail)) {
      rtn <- merge(x = rtn, y = dat1idrsp, by = id.var)
    }
    rtn <- rtn[order(rtn[, id.var], rtn$QES, rtn$SET, rtn$ALT, -rtn$BW), ]

## reverse the sign of ASCs and level variables corresponding to the worst
    attributes.X <- attributes(original.X)
    tmp <- colnames(rtn)
    level.var.names <- attributes.X$level.variables
    rtn[, level.var.names] <- rtn$BW * rtn[, level.var.names]
    ASC.names <- attributes.X$alternative.specific.constants
    rtn[, ASC.names] <- rtn$BW * rtn[, ASC.names]

## delete choice sets containing only one option 
    rtn <- rtn[rtn$SET != nresponses, ]

  } else {
# create data for rank ordered model -------------------------------------------
## create RES.ALT var
    response.rank <- response
    nvars <- length(response.rank[[1]])
    ranking <- c(seq(from = 1, to = nvars, by = 2),      # best
                 rev(seq(from = 2, to = nvars, by = 2))) # worst
    for (i in 1:nquestions) {
      response.rank[[i]] <- response.rank[[i]][ranking]
    }
    response.vars <- unlist(response.rank)
    new.res.vars  <- paste("RES_ALT", 1:length(response.vars), sep =".") 
    dat1      <- data[, c(!colnames(data) %in% unlist(response))]
    dat1rsp   <- data[, response.vars]
    dat1idrsp <- data[, c(id.var, response.vars)]
    colnames(dat1rsp) <- new.res.vars
    dat1 <- cbind(dat1, dat1rsp)

## reshape respondent dataset
    dat2 <- reshape(dat1,
                    idvar = id.var,
                    varying = new.res.vars,
                    direction = "long")

## create QES, SET, and BW variables
    dat2$QES <- rep(as.integer(1:nquestions),
                    each = nrespondents * nresponses)
    dat2$SET <- rep(as.integer(1:nresponses),
                    each = nrespondents,
                    time = nquestions)

## reshape reshaped-respondent dataset
### create temporary id variable for the reshaping
    dat2$tmpID <- 1:nrow(dat2)
### create alternative variables for the reshaping 
    tmpDF <- data.frame(matrix(1:nalternatives, ncol = nalternatives))
    alt.vars <- paste("ALT", 1:nalternatives, sep = ".")
    colnames(tmpDF) <- alt.vars
    dat3 <- cbind(dat2, tmpDF)
### reshape
    dat3 <- reshape(dat3,
                    idvar = "tmpID",
                    varying = alt.vars,
                    direction = "long")
    dat3 <- dat3[order(dat3[, id.var], dat3$QES, dat3$SET, dat3$ALT), ]

## create RES and STR variables
    dat3$RES <- as.integer(dat3$RES_ALT == dat3$ALT)
    dat3$STR <- 1000 * dat3[, id.var] + 10 * dat3$QES + dat3$SET

## delete selected alternatives in previous responses
    RES_ALT <- as.integer(dat3$RES_ALT)
    matALT <- matrix(1L:nalternatives,
                     nrow = nalternatives * nresponses,
                     ncol = nquestions * nrespondents)
    matRTN <- matrix(0L,
                     nrow = nalternatives * nresponses,
                     ncol = nquestions * nrespondents)

    for (i in 1:(nalternatives - 1)) {
      B <- c(rep(0L, times = i * nalternatives), RES_ALT)
      B <- B[1:length(RES_ALT)]
      matB <- matrix(B,
                     nrow = nalternatives * nresponses,
                     ncol = nquestions * nrespondents)
      matB[1:(i * nalternatives), ] <- 0L
      matTRUE <- matB == matALT
      matRTN <- matRTN + matTRUE
    }

    DELETE <- as.vector(matRTN)
    dat3$DELETE <- DELETE
    rtn.dat <- dat3[c(dat3$DELETE == 0), ]

## delete temporary variables
    rtn.dat <- rtn.dat[, !names(rtn.dat) %in% c("time", "tmpID", "DELETE")]

## merge respondent dataset with design matrix
    tmp <- colnames(data)[!(colnames(data) %in% c("BLOCK", response.vars))]
    if (length(tmp) > 1){
      covariate.names <- tmp[-1]
    } else {
      covariate.names <- NULL
    }

    X <- design.matrix.dcm(choice.sets = design,
                           categorical.attributes = categorical.attributes,
                           continuous.attributes  = continuous.attributes,
                           common = common, optout = optout,
                           asc = asc)
    original.X <- X
    rtn <- merge(x = rtn.dat, y = X, by = c("BLOCK", "QES", "ALT"))
    tmp.var.names <- colnames(rtn)[!(colnames(rtn) %in% covariate.names)]
    tmp.var.names <- tmp.var.names[!(tmp.var.names %in% id.var)]
    rtn <- rtn[, c(id.var, tmp.var.names, covariate.names)]
    if (isTRUE(detail)) {
      rtn <- merge(x = rtn, y = dat1idrsp, by = id.var)
    }
    rtn <- rtn[order(rtn[, id.var], rtn$QES, rtn$SET, rtn$ALT), ]

## delete choice sets containing only one option (the lowest rank option) 
    rtn <- rtn[rtn$SET != nresponses, ]

##
    attributes.X <- attributes(original.X)
    level.var.names <- attributes.X$level.variables
    ASC.names <- attributes.X$alternative.specific.constants
  }

# format and return output ----------------------------------------------------
  rownames(rtn) <- NULL

  attributes(rtn)$data                   <- original.data 
  attributes(rtn)$id                     <- id.var
  attributes(rtn)$response               <- response
  attributes(rtn)$model                  <- model
  attributes(rtn)$nalternatives          <- nalternatives
  attributes(rtn)$nresponses             <- nresponses
  attributes(rtn)$nquestions             <- nquestions
  attributes(rtn)$nchoicesets            <- nblocks * nquestions 
  attributes(rtn)$design                 <- choice.sets
  attributes(rtn)$design.matrix          <- original.X
  attributes(rtn)$covariates             <- covariate.names
  attributes(rtn)$attribute.names        <- attribute.names
  attributes(rtn)$categorical.attributes <- categorical.attributes 
  attributes(rtn)$level.variables        <- level.var.names 
  attributes(rtn)$continuous.attributes  <- continuous.attributes
  attributes(rtn)$common                 <- common 
  attributes(rtn)$asc                    <- asc 
  attributes(rtn)$asc.names              <- ASC.names

  class(rtn) <- c("bws3dataset", "data.frame")
  rtn
}
