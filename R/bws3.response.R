bws3.response <- function(
  design,
  categorical.attributes = NULL,
  continuous.attributes = NULL,
  asc = NULL,
  common = NULL,
  optout = FALSE,
  b,
  n,
  detail = FALSE,
  seed = NULL)
{
# Set variables
  nR   <- n
  nALT <- design$design.information$nalternatives
  nB   <- design$design.information$nblocks
  nQ   <- design$design.information$nquestions
  if (optout == TRUE) {
    nALT <- nALT + 1
  }
  if (!is.null(common)) {
    nALT <- nALT + 1
  }
  nPP <- nALT * (nALT - 1)

# Check arguments
  if (nR < nB) {
    stop("number of respondents must be larger than number of blocks")
  }
  if ((nR %% nB) != 0) {
    stop("number of respondents must be divisible by numbe of blocks") 
  }

# Create design matrix for paired model
  D <- design.matrix.dcm(
         choice.sets = design,
         categorical.attributes = categorical.attributes,
         continuous.attributes = continuous.attributes,
         common = common,
         optout = optout,
         asc = asc)
  original.D <- D
  SUBQES <- rep(x = 1:nALT, each = nrow(D))
  var.names <- colnames(D)
  D <- rep(x = 1, times = nALT) %x% data.matrix(D)
  colnames(D) <- var.names
  D  <- data.frame(cbind(D, SUBQES))
  DB <- D[order(D$BLOCK, D$QES, D$ALT),]
  DW <- D[order(D$BLOCK, D$QES, D$SUBQES),]

  DBp <- DB[, 1:3]
  colnames(DBp)[3] <- "BEST"
  DBp$WORST <- DW[, 3]

  diffD <- DB - DW
  deleteRows <- c(-1, -2, -3, -ncol(diffD))
  subset.diffD <- diffD[, deleteRows]
  diffD <- cbind(DBp, subset.diffD)
  diffD <- diffD[!c(diffD$BEST == diffD$WORST), ]
  diffD$PAIR <- rep(x = 1:(nALT * (nALT - 1)), times = nQ * nB)

# Calculate all respondents' utilities for alternatives
  id <- rep(x = 1:nR, each = nrow(diffD)/nB)
  rownames(diffD) <- NULL
  X  <- data.frame(id, diffD)
  Xb <- sweep(x = X[, 6:(ncol(X) - 1)], MARGIN = 2, STATS = b, FUN = "*")
  V  <- rowSums(Xb)
  if (!is.null(seed)) {
    set.seed(seed)
  }
  e <- -log(-log(runif(n = length(V))))
  U <- V + e

# Search best and worst alternatives according to U
  Umat <- matrix(data = U, ncol = nPP, byrow = TRUE) 
  columns.max <- max.col(Umat)
  BWelement <- cbind(R = 1:nrow(Umat), C = columns.max)
  RESmat <- matrix(data = 0L, nrow = nrow(Umat), ncol = ncol(Umat))
  RESmat[BWelement] <- 1L
  RES <- as.vector(t(RESmat))

# Construct and return detailed dataset
  STR <- 100 * X$id + X$QES
  dataset <- data.frame(X, RES, STR)
  if (detail == TRUE) {
    return(dataset)
  }

# Construct and return simple dataset (in alternative number format)
  simple.dataset <- dataset[dataset$RES == 1,
                            c("id", "BLOCK", "QES", "BEST", "WORST")]
  colnames(simple.dataset)[c(4, 5)] <- c("B", "W")
  rtn <- reshape(simple.dataset, v.names = c("B", "W"), idvar = "id",
                 timevar = "QES", sep = "", direction = "wide")
  rownames(rtn) <- NULL
  return(data.frame(rtn))
}

