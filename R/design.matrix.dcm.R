design.matrix.dcm <-
function(choice.sets,
         categorical.attributes = NULL,
         continuous.attributes = NULL,
         common = NULL,
         optout = FALSE,
         asc = NULL) {
# set variables ----------------------------------------------------------------
## effect-coded variables
  categorical.attributes.original <- categorical.attributes
  if (is.list(categorical.attributes)){
    effectcoding <- TRUE
    reference.levels <- unlist(categorical.attributes)
    categorical.attributes <- names(categorical.attributes)
  } else {
    effectcoding <- FALSE
  }
## design information
  nblocks       <- choice.sets$design.information$nblocks
  nquestions    <- choice.sets$design.information$nquestions
  nalternatives <- choice.sets$design.information$nalternatives
  nattributes   <- choice.sets$design.information$nattributes
## choice sets
  ced <- choice.sets$alternatives
  if (!is.null(common)) {
    nalternatives <- nalternatives + 1
    base.alt <- ced[[1]]
    base.alt[, 3] <- nalternatives
    for (i in categorical.attributes) {
      base.alt[i] <- factor(common[[i]], levels = levels(base.alt[[i]]))
    }
    for (i in continuous.attributes) {
      base.alt[i] <- as.numeric(common[[i]])
    }
    ced <- c(ced, alt.common = list(base.alt))
  }
  variable.names <- NULL
  conv.ced <- vector("list", nalternatives)
## check asc
  if (!is.null(asc)) {
    if (isTRUE(optout)) {
      nAlts <- nalternatives + 1
    } else {
      nAlts <- nalternatives
    }
    if (!isTRUE(length(asc) == nAlts)) {
      stop(message = 
        "length of asc differs from the number of alternatives per set")
    }
  }

# create attribute variables ---------------------------------------------------
  for (i in 1:nalternatives) {
    ## categorical attribute variables
    if (!is.null(categorical.attributes)) {
      for (j in 1:length(categorical.attributes)) {
        k <- which(names(ced[[i]]) == categorical.attributes[j])
        m <- nlevels(ced[[i]][, k])
        if (effectcoding == TRUE) {
        ### effect-coded variables
          tmp.var.names <- levels(ced[[i]][, k])[1:m]
          ref <- which(tmp.var.names == reference.levels[j])
          variable.names <- c(variable.names, tmp.var.names[-ref])
          tmp.model.matrix <- model.matrix(~ ced[[i]][, k] - 1)[, 1:m]
          tmp.model.matrix <- tmp.model.matrix[, -ref, drop = FALSE]
          rows2ref <- which(ced[[i]][, k] == reference.levels[j])
          tmp.model.matrix[rows2ref, ] <- -1
          conv.ced[[i]] <- cbind(conv.ced[[i]], tmp.model.matrix)
        } else {
        ### dummy-coded variables
          variable.names <- c(variable.names, 
                              levels(ced[[i]][, k])[1:m])
          conv.ced[[i]] <- cbind(conv.ced[[i]],
                                 model.matrix(~ ced[[i]][, k] - 1)[, 1:m])
        }
      }
    }
    ## continuous attribute variables
    if (is.null(continuous.attributes) == FALSE) {
      for (j in 1:length(continuous.attributes)) {
        k <- which(names(ced[[i]]) == continuous.attributes[j]) 
        variable.names <- c(variable.names, names(ced[[i]])[k])
        conv.ced[[i]] <- cbind(conv.ced[[i]],
                               as.numeric(as.character(ced[[i]][, k])))
      }
    }
  }

# create design matrix ---------------------------------------------------------
## create desing matrix with attribute variabls
  nvariables <- length(variable.names) / nalternatives
  my.design  <- conv.ced[[1]]
  for (i in 2:nalternatives) {
    my.design <- rbind(my.design, conv.ced[[i]])
  }
  colnames(my.design) <- level.vars <- variable.names[1: nvariables]
## create BLOCK, QES, and ALT variables
  BQS <- ced[[1]][, 1:3]
  for (i in 2:nalternatives) {
    BQS <- rbind(BQS, ced[[i]][, 1:3])
  }
## add rows corresponding to optout to BQS and design matrix
  if (isTRUE(optout)) {
    nQnB <- nquestions * nblocks
    optout.vars <- rep(x = 0, times = ncol(my.design))
    optout.vars <- matrix(optout.vars, nrow = 1)
    optout.design <- rep(x = 1, times = nQnB) %x% optout.vars
    colnames(optout.design) <- level.vars
    optout.BQS <- as.data.frame(
      matrix(c(rep(x = c(1:nblocks), each = nquestions),   # BLOCK
               rep(x = c(1:nquestions), times = nblocks),  # QES
               rep(x = (nalternatives + 1), times = nQnB)),# ALT
             ncol = 3))
    colnames(optout.BQS) <- colnames(BQS)
    BQS <- rbind(BQS, optout.BQS)
    my.design <- rbind(my.design, optout.design)
  }
## create ASC
  if (!is.null(asc)) {
    nASCs <- length(asc) 
    matASC <- diag(asc)
    matASC <- matASC[BQS$ALT,]
    if (!is.null(names(asc))) {
      colnames(matASC) <- names(asc)
    } else {
      colnames(matASC) <- paste("ASC", 1:nASCs, sep = "")
    }
    ASCnames <- colnames(matASC)
  } else {
    ASCnames <- NULL
  }
## finalize design matrix
  if (!is.null(asc)) {
    rtn <- data.frame(BQS, matASC, my.design) 
  } else {
    rtn <- data.frame(BQS, my.design)
  }
  rtn <- rtn[order(rtn$BLOCK, rtn$QES, rtn$ALT), ]
  row.names(rtn) <- 1:nrow(rtn)

# format and return output -----------------------------------------------------
  attributes(rtn)$design                 <- choice.sets
  attributes(rtn)$categorical.attributes <- categorical.attributes.original
  attributes(rtn)$continuous.attributes  <- continuous.attributes
  attributes(rtn)$level.variables        <- level.vars
  attributes(rtn)$alternative.specific.constants <- ASCnames
  attributes(rtn)$nquestions             <- nquestions
  attributes(rtn)$nattributes            <- nattributes
  attributes(rtn)$nblocks                <- nblocks
  attributes(rtn)$nalternatives          <- nrow(rtn) / (nquestions * nblocks)

  return(rtn)
}
