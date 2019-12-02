\name{bws3.dataset}
\alias{bws3.dataset}

\title{
Creating a dataset suitable for Case 3 best-worst scaling analysis
}

\description{
This function creates a dataset suitable for Case 3 best-worst scaling analysis using a modeling approach.
}

\usage{
bws3.dataset(data, id = "id", response, choice.sets, categorical.attributes = NULL,
 continuous.attributes = NULL, common = NULL, optout = FALSE, asc = NULL,
 model = "maxdiff", detail = FALSE, ...)
}

\arguments{
  \item{data}{
A data frame containing a respondent dataset.
}
  \item{id}{
A character representing the respondent identification number variable in the respondent dataset.
}
  \item{response}{
A named list containing the names of response variables in the respondent dataset.
}
  \item{choice.sets}{
An object of the S3 class \code{"cedes"}.
}
  \item{categorical.attributes}{
A vector containing the names of attributes treated as dummy-coded variables in the analysis, or a named vector containing the reference levels of attributes treated as effect-coded variables. If there are no categorical variables, it is set as \code{NULL} (default).
}
  \item{continuous.attributes}{
A vector containing the names of attributes treated as continuous variables in the analysis. If there are no continuous variables, it is set as \code{NULL} (default).

}
  \item{common}{
A named vector containing a fixed combination of levels corresponding to a common alternative (base/reference) in the choice sets. If the common alternative is an opt-out (no choice) option, use the argument \code{optout} instead. If there is no common option, it is set as \code{NULL} (default).
}
  \item{optout}{
A logical variable describing whether the opt-out option is included in the BWS questions. If \code{TRUE} (default), the opt-out option is included; otherwise it is not.
}
  \item{asc}{
A vector containing binary values, which takes the value of \code{1} when an alternative specific constant (ASC) is included in the utility function of an alternative and \code{0} otherwise: the \eqn{i}-th element in the vector corresponds to the \eqn{i}-th alternative. If there are no ASCs, it is set as \code{NULL} (default).
}
  \item{model}{
A character showing a type of dataset created by the function: \code{"maxdiff"} for a maximum difference model; \code{"sequential"} for a sequential model; and \code{"rank"} for a rank-ordered model.
}
  \item{detail}{
A logical variable describing whether the response variables that are assigned to the argument \code{response} are also stored in the dataset created by the function. 
}
  \item{\dots}{
Optional arguments; currently not in use.
}
}

\details{
This function creates a dataset for Case 3 BWS analysis using the modeling approach, which uses discrete choice models. When using the function, a model type must be selected from among the three standards models: maximum difference (maxdiff/paired), sequential, and rank-ordered. The argument \code{model} is set according to the selected model. See Lancsar et al. (2013) and Marley and Pihlens (2012) for details on the three models, and Hensher et al. (2015, Appendix 6B) for details on the dataset structure for Case 3 BWS.

The respondent dataset, in which each row corresponds to a respondent, is assigned to the argument \code{data}. The dataset must include the respondent's identification number (id) variable, the block variable (\code{BLOCK}), and the response variables, each indicating the alternatives (profiles) that were selected as the best and worst for each question. Other variables in the respondent dataset are treated as the respondents' characteristics such as gender and age. The variables relating to respondents' characteristics are also stored in the resultant dataset created by the function. The names of the id and response variables are left to the discretion of the user. The id variable is assigned to the argument \code{id} (the default is \code{"id"}). The response variables are assigned to the argument \code{response} in list format: each element of the list, corresponding to a Case 3 BWS question, is a named vector that contains alternative numbers selected as the best and worst in the Case 3 BWS question (see the Examples section below for details). 

An object containing the choice sets, which follows the S3 class \code{"cedes"} (see the \code{\link[support.CEs]{rotation.design}} function in the \pkg{support.CEs} package), is assigned to the argument \code{choice.sets}. Only unlabeled (generic) designs are acceptable in the current version of the function. Attribute variables in \code{choice.sets} are assigned either to the argument \code{categorical.attributes} or the argument \code{continuous.attributes}, according to the model you use. If categorical attributes are treated as dummy-coded variables in the analysis, a vector containing the names of the categorical attributes is assigned to \code{categorical.attributes}. If categorical attributes are treated as effect-coded variables in the analysis, a named vector containing the names of reference levels is assigned to \code{categorical.attributes}. For example, suppose a profile consists of three 3-level attributes: attribute A with levels A1, A2, and A3; attribute B with B1, B2, and B3 levels; and attribute C with C1, C2, and C3 levels. If the three attributes are treated as dummy-coded variables in the analysis, a vector \code{c("A", "B", "C")} is assigned to \code{categorical.attributes}. If the three attributes are treated as effect-coded variables in the analysis and the first level in each attribute is a reference level, a named vector \code{c(A = "A1", B = "B1", C = "C1")} is assigned to it. If each BWS question contains a common alternative, such as status quo, and it is not included in the choice sets assigned to the argument \code{choice.sets}, a fixed combination of levels corresponding to the common alternative in vector format (e.g., \code{c("A1", "B3", "C2")}) is assigned to the argument \code{common}. If each BWS question includes an opt-out (no choice) option, the argument \code{option} is set as \code{TRUE}; otherwise it is set as \code{FALSE} (default). Accordingly, the attribute variables in the dataset created by the function take the value of \code{0} in the row corresponding to the opt-out options. The order of alternatives in \code{choice.sets} must be the same as the alternative numbers used in the response variables. Furthermore, the order of questions in the respondent dataset must be the same as that in \code{choice.sets}.

If alternative specific constants (ASCs) are needed for the model you choose, a vector with binary values is set according to your model specification and then assigned to the argument \code{asc}. The \eqn{i}-th element in the vector corresponds to the \eqn{i}-th alternative, and the value of \code{1} indicates the presence of ASC in the corresponding alternative and the value of \code{0} indicates its absence. For example, suppose that each BWS question has four alternatives and the 1st, 2nd, and 3rd alternatives in the model have an ASC each. In this case, a vector \code{c(1, 1, 1, 0)} is assigned to \code{asc}. Consequently, the resultant dataset includes four ASCs: \code{ASC1} taking the value of \code{1} for the 1st alternative and \code{0} otherwise, \code{ASC2} taking the value of \code{1} for the 2nd alternative and \code{0} otherwise, \code{ASC3} taking the value of \code{1} for the 3rd alternative and \code{0} otherwise, and \code{ASC4} taking the value of \code{0} for all the alternatives (\code{ASC4} is not needed in the analysis).
}

\value{
This function returns a dataset for analysis, which is an object of S3 class \code{"bws3dataset"} and inherits from data frame format. The dataset for the maxdiff (paired) model contains the following variables as well as attribute variables and respondents' characteristic variables: 

\item{id}{
A respondent's identification number; the actual name of this variable is set according to the id variable in the respondent dataset if the argument \code{id} is set by users.}

\item{BLOCK}{
A serial number of blocks.}

\item{QES}{
A serial number of Case 3 BWS questions.}

\item{PAIR}{
A serial number for the possible pairs of the best and worst alternatives for each question.}

\item{BEST}{
An alternative number treated as the best in the possible pairs of the best and worst alternatives.}
 
\item{WORST}{
An alternative number treated as the worst in the possible pairs of the best and worst alternatives.}

\item{RES.B}{
An alternative number selected as the best by respondents.}
 
\item{RES.W}{
An alternative number selected as the worst by respondents.}
 
\item{RES}{
A binary variable that takes the value of \code{1} when a possible pair of the best and worst alternatives is selected by respondents and \code{0} otherwise. The variable is used as a dependent variable in the model formula of the function for discrete choice analysis.}
 
\item{STR}{
A stratification variable identifying each combination of respondent and question. The variable may be used in the model formula for discrete choice analysis.}

\item{ASC}{
Alternative specific constant(s). The serial number of the alternatives is appended to the tail of ASC (e.g., \code{ASC1}, \code{ASC2}, and \code{ASC3}).}


The dataset for the sequential model and the one for the rank-ordered model contains the variables \code{id}, \code{BLOCK}, \code{QES}, and \code{STR} mentioned above, in addition to the following variables: 

\item{ALT}{
A serial number of alternatives for each question.}
 
\item{RES.ALT}{
An alternative number selected as the best or worst by respondents.}

\item{SET}{
A serial number of implied choice sets for each question.}

\item{BW}{
A state variable taking the value of \code{1} for the possible best alternatives and \code{-1} for the possible worst alternatives. The variable is not included in the dataset for the rank-ordered model.}
 
\item{RES}{
A binary variable that takes the value of \code{1} when a possible best or worst alternative is selected by respondents and \code{0} otherwise.}

}

\references{
See the help page for \code{\link[support.BWS3]{support.BWS3-package}}.
}

\author{
Hideo Aizaki
}

\seealso{
\code{\link[support.BWS3]{support.BWS3-package}}, \code{\link[support.CEs]{rotation.design}}, \code{\link[survival]{clogit}}
}

\examples{
# The following lines of code construct choice sets for Case 3 BWS
# using rotation.design() in the package support.CEs; create a dataset
# for the analysis using bws3.dataset(); and then conduct a maxdiff model
# analysis on the basis of a conditional logit model specification
# using clogit() in the package survival.

# Load packages
library(support.CEs)
library(survival)
# Reproduce old results
if(getRversion() >= "3.6.0") RNGkind(sample.kind = "Rounding")
# Create choice sets
dsgn <- rotation.design(                
  attribute.names = list(
    A = c("A1", "A2", "A3"), B = c("B1", "B2", "B3"),
    C = c("C1", "C2", "C3"), D = c("D1", "D2", "D3")),
  nalternatives = 4, nblocks = 1,randomize = TRUE, seed = 9876)
# Prepare response variables
dat <- data.frame(          
  IDvar = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  BLOCK = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  q1b1  = c(2, 1, 1, 1, 1, 2, 1, 1, 1, 1),
  q1w1  = c(4, 2, 4, 3, 4, 4, 4, 3, 3, 4),
  q1b2  = c(1, 4, 2, 2, 2, 3, 3, 2, 2, 2),
  q1w2  = c(3, 3, 3, 4, 3, 1, 2, 4, 4, 3),
  q2b1  = c(1, 4, 2, 1, 1, 4, 4, 4, 1, 2),
  q2w1  = c(3, 3, 3, 3, 3, 1, 3, 2, 2, 3),
  q2b2  = c(4, 1, 4, 4, 4, 3, 1, 1, 4, 4),
  q2w2  = c(2, 2, 1, 2, 2, 2, 2, 3, 3, 1),
  q3b1  = c(4, 2, 1, 4, 2, 2, 4, 4, 3, 4),
  q3w1  = c(1, 3, 3, 1, 1, 1, 1, 1, 4, 1),
  q3b2  = c(2, 1, 2, 3, 4, 3, 2, 3, 1, 3),
  q3w2  = c(3, 4, 4, 2, 3, 4, 3, 2, 2, 2),
  q4b1  = c(2, 2, 2, 1, 2, 3, 4, 4, 2, 1),
  q4w1  = c(3, 4, 3, 3, 4, 4, 3, 2, 4, 2),
  q4b2  = c(1, 1, 4, 2, 1, 1, 1, 1, 1, 3),
  q4w2  = c(4, 3, 1, 4, 3, 2, 2, 3, 3, 4),
  q5b1  = c(1, 1, 1, 3, 3, 2, 3, 3, 3, 1),
  q5w1  = c(2, 2, 2, 2, 4, 4, 2, 2, 2, 2),
  q5b2  = c(3, 3, 3, 1, 1, 1, 1, 1, 4, 3),
  q5w2  = c(4, 4, 4, 4, 2, 3, 4, 4, 1, 4),
  q6b1  = c(1, 3, 3, 1, 2, 4, 3, 3, 3, 3),
  q6w1  = c(4, 2, 1, 3, 1, 1, 1, 4, 1, 1),
  q6b2  = c(3, 1, 2, 4, 4, 3, 2, 1, 2, 4),
  q6w2  = c(2, 4, 4, 2, 3, 2, 4, 2, 4, 2),
  q7b1  = c(4, 4, 2, 2, 3, 4, 4, 4, 2, 1),
  q7w1  = c(3, 2, 4, 1, 1, 3, 3, 3, 1, 3),
  q7b2  = c(2, 3, 3, 4, 4, 2, 2, 2, 4, 4),
  q7w2  = c(1, 1, 1, 3, 2, 1, 1, 1, 3, 2),
  q8b1  = c(3, 2, 2, 2, 1, 3, 2, 2, 3, 3),
  q8w1  = c(4, 4, 1, 4, 4, 4, 3, 1, 2, 2),
  q8b2  = c(2, 1, 3, 1, 2, 2, 4, 3, 1, 4),
  q8w2  = c(1, 3, 4, 3, 3, 1, 1, 4, 4, 1),
  q9b1  = c(4, 1, 3, 4, 3, 3, 3, 1, 4, 4),
  q9w1  = c(2, 4, 1, 3, 4, 1, 1, 2, 3, 1),
  q9b2  = c(1, 2, 2, 2, 2, 4, 2, 3, 2, 2),
  q9w2  = c(3, 3, 4, 1, 1, 2, 4, 4, 1, 3))
# Store names of response variables
rsp.vars <- list(
  q1 = c("q1b1", "q1w1", "q1b2", "q1w2"),
  q2 = c("q2b1", "q2w1", "q2b2", "q2w2"),
  q3 = c("q3b1", "q3w1", "q3b2", "q3w2"),
  q4 = c("q4b1", "q4w1", "q4b2", "q4w2"),
  q5 = c("q5b1", "q5w1", "q5b2", "q5w2"),
  q6 = c("q6b1", "q6w1", "q6b2", "q6w2"),
  q7 = c("q7b1", "q7w1", "q7b2", "q7w2"),
  q8 = c("q8b1", "q8w1", "q8b2", "q8w2"),
  q9 = c("q9b1", "q9w1", "q9b2", "q9w2"))
# Store names of attributes
attributes <- c("A", "B", "C", "D")
# Create a dataset
bws3dat <- bws3.dataset(
  data = dat, id = "IDvar", response = rsp.vars,
  choice.sets = dsgn, categorical = attributes, model = "maxdiff")
# Fit the model
clogit(RES ~ A2 + A3 + B2 + B3 + C2 + C3 + D2 + D3 + strata(STR), bws3dat)
}

\keyword{manip}
