\name{bws3.response}
\alias{bws3.response}

\title{
Generating artificial responses to Case 3 best-worst scaling questions
}

\description{
This function synthesizes responses to Case 3 best-worst scaling (BWS) questions on the basis of a maximum difference model.
}

\usage{
bws3.response(design, categorical.attributes = NULL, 
  continuous.attributes = NULL, asc = NULL, common = NULL,
  optout = FALSE, b, n, detail = FALSE, seed = NULL)
}

\arguments{
\item{design}{
An object of the S3 class \code{"cedes"}.
}
\item{categorical.attributes}{
A vector containing the names of the attributes that are treated as dummy-coded variables in the model. If there are no categorical variables, it is set as \code{NULL} (default).
}
\item{continuous.attributes}{
A vector containing the names of the attributes that are treated as continuous variables in the model. If there are no continuous variables, it is set as \code{NULL} (default).
}
\item{asc}{
A vector containing binary values, which takes the value of 1 when an alternative specific constant (ASC) is included in the utility function of an alternative and 0 otherwise. The \eqn{i}-th element in the vector corresponds to the \eqn{i}-th alternative. If there are no ASCs, it is set as \code{NULL} (default).
}
\item{common}{
A named vector containing a fixed combination of levels corresponding to a common alternative (base/reference) in the choice sets. If the common alternative is an opt-out (no choice) option, use the argument \code{optout} instead. If there is no common option, it is set as \code{NULL} (default).
}
\item{optout}{
A logical variable describing whether the opt-out option is included in the BWS questions. If \code{TRUE} (default), the opt-out option is included; if \code{FALSE}, it is not.
}
\item{b}{
A vector containing parameters of independent variables in the model. The vector is used to calculate utilities for alternatives.
}
\item{n}{An integer value showing the number of respondents in the resultant dataset.
}
\item{detail}{
A logical variable: if \code{TRUE}, the dataset is returned in a detailed format; and if \code{FALSE} (default), the dataset is returned in a simple format.
}
\item{seed}{
Seed for a random number generator.
}
}

\details{
This function synthesizes responses to Case 3 BWS questions on the basis of a maximum difference model. The model assumes that an alternative consists of \eqn{k} attribute-levels, and that \eqn{m} alternatives exist in a choice set (a question). The number of possible pairs where alternative \eqn{i} is selected as the best and alternative \eqn{j} is selected as the worst (\eqn{i \neq j}) from \eqn{m} alternatives is given by \eqn{m \times (m - 1)}. The model also assumes that the respondents select alternative \eqn{i} as the best and alternative \eqn{j} as the worst because the difference in utility between alternatives \eqn{i} and \eqn{j} is the highest among all of the \eqn{m \times (m - 1)} differences in utility. The systematic component of the utility is assumed to be a linear additive function of the attribute and/or level variables. If the error component of the utility is assumed to be an independently, identically distributed type I extreme value, the probability of selecting alternative \eqn{i} as the best and alternative \eqn{j} as the worst is expressed as a conditional logit model.

Given the parameter values assigned to the argument \code{b}, the choice sets assigned to the argument \code{design}, and the classifications of attributes (categorical or continuous) assigned to the arguments \code{categorical.attributes} and/or \code{continuous.attributes}, the function \code{bws3.response} calculates the utility for the alternatives. The parameter values assigned to the argument \code{b} are set as a numerical vector where the elements correspond to the parameters of attribute and/or level variables. As an example, assume that three attributes A, B, and C are the first, second, and third continuous attribute, respectively, in alternatives, and their corresponding attribute variables \eqn{X_{A}}, \eqn{X_{B}}, and \eqn{X_{C}} have parameter values of \eqn{0.1}, \eqn{0.4}, and \eqn{-0.5}, respectively. A vector assigned to the argument \code{b} is \code{c(0.1, 0.4, -0.5)}. As another example, assume that the three attributes A, B, and C are categorical attributes with three levels each (e.g., levels A1, A2, and A3 for attribute A) and that the first level for each attribute (i.e., A1, B1, and C1) is treated as the base (reference) level. If the parameters for the dummy-coded level variables corresponding to levels A2, A3, B2, B3, C2, and C3 are \eqn{0.2}, \eqn{0.4}, \eqn{0.4}, \eqn{0.8}, \eqn{-0.7}, and \eqn{-1.4}, respectively, a vector assigned to the argument \code{b} is given as \code{c(0, 0.2, 0.4, 0, 0.4, 0.8, 0, -0.7, -1.4)}, where the first, fourth, and seventh elements correspond to the reference level for each attribute, and thus have a value of 0. After calculating the utility values (by adding the calculated values of the systematic component of the utility and random numbers generated from a type I extreme value distribution), the function \code{bws3.response} finds the pair with the highest difference in utility from the \eqn{m \times (m - 1)} differences in utility.

If the systematic component of the utility includes ASCs, the argument \code{asc} is set: the \eqn{i}-th element of a binary vector assigned to the argument \code{asc} takes a value of 1 if the \eqn{i}-th alternative has an ASC, and takes a value of 0 otherwise. As an example, if a choice set consists of four alternatives and only the fourth alternative has an ASC, a vector \code{c(0, 0, 0, 1)} is assigned to the argument \code{asc}. Although this creates four ASCs (i.e., \code{ASC1}, \code{ASC2}, \code{ASC3}, and \code{ASC4}) corresponding to the four alternatives in the resultant dataset, only the fourth ASC (\code{ASC4}) is used in the analysis (the remaining \code{ASC1}, \code{ASC2}, and \code{ASC3} are ignored). As another example, if a choice set consists of three alternatives and the first and second alternatives have ASCs, a vector \code{c(1, 1, 0)} is assigned to the argument \code{asc}: this results in three ASCs in the resultant dataset; only the first and second ASCs (\code{ASC1} and \code{ASC2}) are used in the analysis.

If BWS questions include an opt-out option, the argument \code{optout} is set as \code{TRUE}; otherwise it is set as \code{FALSE}. The function \code{bws3.response} assumes that the opt-out option is the last alternative in each choice set and that all of the attribute and/or level variables associated with the opt-out option have a value of 0.
}

\value{
This function returns a data frame that contains synthesized responses to Case 3 BWS questions, in either a detailed or a simple format. The detailed format dataset contains the following variables, as well as independent variables according to the arguments \code{categorical.attributes} and/or \code{continuous.attributes}.

\item{id}{
An identification number of artificial respondents.
}
\item{BLOCK}{
A serial number of blocks in the choice sets.
}
\item{QES}{
A serial number of questions.
}
\item{BEST}{
An alternative number treated as the best in the possible pairs of the best and worst alternatives.
}
\item{WORST}{
An alternative number treated as the worst in the possible pairs of the best and worst alternatives.
}
\item{PAIR}{
A serial number of possible pairs of the best and worst alternatives for each question.
}
\item{RES}{
Responses to BWS questions, taking the value of 1 if a possible pair of the best and worst alternative is selected by the synthesized respondents and 0 otherwise.
}
\item{STR}{
A stratification variable used to identify each combination of respondent and question.
}

The simple format dataset contains the following variables.

\item{id}{
An identification number of artificial respondents.
}
\item{BLOCK}{
A serial number of blocks in the choice sets.
}
\item{Bi}{
A variable describing the alternative number that is selected as the best in the \eqn{i}-th BWS question. The serial number of questions is appended to the tail of the variable name (e.g., \code{B1} for the first question, \code{B2} for the second question, and \code{B3} for the third question).
}
\item{Wi}{
A variable describing the alternative number that is selected as the worst in the \eqn{i}-th BWS question. The serial number of questions is appended to the tail of the variable name (e.g., \code{W1} for the first question, \code{W2} for the second question, and \code{W3} for the third question).
}

The detailed format dataset includes a dependent variable and independent variables for the analysis, and thus is available for discrete choice analysis functions such as the function \code{clogit} in the \bold{survival} package. On the other hand, the simple format dataset only contains variables that correspond to responses to BWS questions, as well as \code{id} and \code{BLOCK} variables. It must be converted using the function \code{bws3.dataset} in the package for the analysis. For details, see the Examples section.
}

\seealso{
\code{\link[support.BWS3]{support.BWS3-package}}, \code{\link[support.CEs]{rotation.design}}, \code{\link[survival]{clogit}}, \code{\link[DoE.base]{oa.design}}, \code{\link[crossdes]{find.BIB}}
}

\examples{
# The following lines of code synthesize responses to Case 3 BWS questions,
# return them in detailed and simple format, and then fit the models using
# the function clogit in the survival package. The choice sets include 
# three alternatives that are expressed by three attributes (A, B, and C)
# with three levels each. The function bws3.design creates the choice sets
# from a three-level fractional factorial design with 3 factors that is
# generated using the function oa.design in the DoE.base package and a BIBD
# with 9 treatments, 12 blocks, and size 3 that is generated using
# the function find.BIB in the crossdes package. The systematic component
# of the utility for alternatives where all of the three attributes are
# treated as categorical attributes is the same as that explained in 
# the Details section.

\dontrun{
# Load packages
library(DoE.base)
library(survival)
library(crossdes)

# Create design matrix
set.seed(987)
bibd <- find.BIB(trt = 9, b = 12, k = 3)
isGYD(bibd)
oa <- oa.design(nl = c(3, 3, 3), randomize = FALSE)
atr <- list(
  A = c("A1", "A2", "A3"), B = c("B1", "B2", "B3"),
  C = c("C1", "C2", "C3"))
dsgn <- bws3.design(bibd = bibd, ffd = oa, attribute.levels = atr)

# Synthesize responses to Case 3 BWS questions 
param <- c(0, 0.2, 0.4, 0, 0.4, 0.8, 0, -0.7, -1.4)
atr.names <- c("A", "B", "C")
## dataset in detailed format
dat.detail <- bws3.response(
  design = dsgn, b = param, n = 100,
  categorical.attributes = atr.names,
  detail = TRUE, seed = 987)
str(dat.detail)
## dataset in simple format
dat.simple <- bws3.response(
  design = dsgn, b = param, n = 100,
  categorical.attributes = atr.names,
  detail = FALSE, seed = 987)
str(dat.simple)

# Convert dat.simple into dataset for the analysis
dat.simple.md <- bws3.dataset(
  data = dat.simple,
  response = list(
    c("B1", "W1"), c("B2", "W2"), c("B3", "W3"),
    c("B4", "W4"), c("B5", "W5"), c("B6", "W6"),
    c("B7", "W7"), c("B8", "W8"), c("B9", "W9"),
    c("B10", "W10"), c("B11", "W11"), c("B12", "W12")),
  choice.sets = dsgn,
  categorical.attributes = atr.names,
  model = "maxdiff")

# Fit conditional logit models
mf <- RES ~ A2 + A3 + B2 + B3 + C2 + C3  + strata(STR)
out.detail <- clogit(mf, dat.detail)
out.simple <- clogit(mf, dat.simple.md)
out.simple
all.equal(coef(out.detail), coef(out.simple))
}
}

\keyword{ datagen }
