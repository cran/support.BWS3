\name{milk}
\alias{milk}
\docType{data}
\title{
Consumer valuation of milk
}
\description{
This dataset contains artificial responses to Case 3 BWS questions about consumer valuation of milk.
}
\usage{
data(milk)
}
\format{
  A data frame with 150 rows (respondents) and 20 columns (variables).
  \describe{
    \item{\code{id}}{Identification number of respondents.}
    \item{\code{BLOCK}}{Serial number of blocks.}
    \item{\code{B1}}{Alternative selected as the best in question 1.}
    \item{\code{W1}}{Alternative selected as the worst in question 1.}
    \item{\code{B2}}{Alternative selected as the best in question 2.}
    \item{\code{W2}}{Alternative selected as the worst in question 2.}
    \item{\code{B3}}{Alternative selected as the best in question 3.}
    \item{\code{W3}}{Alternative selected as the worst in question 3.}
    \item{\code{B4}}{Alternative selected as the best in question 4.}
    \item{\code{W4}}{Alternative selected as the worst in question 4.}
    \item{\code{B5}}{Alternative selected as the best in question 5.}
    \item{\code{W5}}{Alternative selected as the worst in question 5.}
    \item{\code{B6}}{Alternative selected as the best in question 6.}
    \item{\code{W6}}{Alternative selected as the worst in question 6.}
    \item{\code{B7}}{Alternative selected as the best in question 7.}
    \item{\code{W7}}{Alternative selected as the worst in question 7.}
    \item{\code{B8}}{Alternative selected as the best in question 8.}
    \item{\code{W8}}{Alternative selected as the worst in question 8.}
    \item{\code{f}}{Dummy variable taking 1 if respondent is female, 0 otherwise.}
    \item{\code{age}}{Respondent's age: 2 = 20s; 3 = 30s; 4 = 40s; and 5 = 50s.}
  }
}
\source{
Responses were generated artificially as follows.

1) A Case 3 BWS question required respondents to select their most and least preferred ones from among two cartons of milk (Milk 1 and Milk 2) and an opt-out option. Each response variable (\code{B} and \code{W}) indicates an alternative selected: 1 = Milk 1; 2 = Milk 2; and 3 = the opt-out option.

2) Milk has six attributes: safety, worker, ghg, cattle, biodiversity, and price. The former five attributes refer to sustainable dairy practices involved in the production process of raw milk.

3) A total of 24 questions were created and then divided into 3 sets of 8 questions.

4) Conditions for generating responses were set considering Aizaki et al. (2013) and Aizaki and Takeshita (2023).

5) Respondents' characteristics (variables \code{f} and \code{age}) were generated randomly.
}
\references{
Aizaki H, Takeshita H. (2023) Comparing consumer preferences for sustainable dairy activities among countries. \emph{Behaviormetrika} \bold{50}: 653--677. (Open access paper)

Aizaki H, Nanseki T, Zhou H. (2013) Japanese consumer preferences for milk certified as good agricultural practice. \emph{Animal Science Journal} \bold{84}: 82--89.
}
\author{
Hideo Aizaki
}
\seealso{
 \code{\link{bws3.dataset}}, \code{\link[support.CEs]{rotation.design}}, \code{\link[survival]{clogit}}
}
\examples{
\dontrun{
# Load packages
library(survival)
library(support.CEs)
# Define attributes and levels
## Each practice attribute has two levels: "wo.x" and "x".
## "wo" is an abbreviation of "without".
## "wo.x" means a milk carton without a label regarding practice x.
## "x" means a milk carton with a label regarding practice x.
attrs <- list(
 safety       = c("wo.safety", "safety"),
 worker       = c("wo.worker", "worker"),
 ghg          = c("wo.ghg", "ghg"),
 cattle       = c("wo.cattle", "cattle"),
 biodiversity = c("wo.biodiversity", "biodiversity"),
 price        = c(148, 158, 168, 178, 188, 198))
# Create choice sets
BWS3design <- rotation.design(
 attribute.names = attrs,
 nalternatives = 2,
 nblocks = 3,
 randomize = TRUE,
 seed = 987)
# Load a dataset "milk"
data(milk)
# Create a dataset
bws3dat <- bws3.dataset(
 data = milk,
 response = list(
  c("B1", "W1"), c("B2", "W2"), c("B3", "W3"), c("B4", "W4"),
  c("B5", "W5"), c("B6", "W6"), c("B7", "W7"), c("B8", "W8")),
 choice.sets = BWS3design,
 categorical.attributes = 
  c("safety", "worker", "ghg", "cattle", "biodiversity"),
 continuous.attributes  = c("price"),
 optout = TRUE,
 asc = c(0,0,1),
 model = "maxdiff")
# Fit a model
bws3mf <- RES ~ ASC3 + safety + worker + ghg + cattle +
                biodiversity + price + strata(STR)
bws3md.cl <- clogit(formula = bws3mf, data = bws3dat)
bws3md.cl
gofm(bws3md.cl)
mwtp(bws3md.cl, monetary.variables = "price")
}
}
\keyword{datasets}
