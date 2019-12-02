\name{bws3.design}
\alias{bws3.design}

\title{
Designing choice sets for Case 3 best-worst scaling
}

\description{
This function creates a design for Case 3 best-worst scaling (BWS) from a full/fractional factorial design (FFD) and a balanced incomplete block design (BIBD).
}

\usage{
bws3.design(bibd, ffd, attribute.levels)
}

\arguments{
\item{bibd}{
A data frame or matrix containing a BIBD.
}
\item{ffd}{
A data frame or matrix containing a FFD.
}
\item{attribute.levels}{
A list containing the names of the attributes and their levels.
}
}

\details{
This function creates the choice sets for Case 3 BWS according to Louviere et al. (2015): candidate alternatives (profiles) are created using a FFD; and then the profiles are assigned to the choice sets using a BIBD. The FFD and BIBD used in the function must satisfy the following:

1) The level values in the FFD and those in the BIBD are serial integer values starting from 1.

2) The number of columns of the BIBD is equal to or larger than three.

3) The number of treatments in the BIBD is equal to the number of rows of the FFD. 

4) The number of columns of the FFD is equal to the number of attributes.

5) The order of columns (from left to right) of the FFD corresponds to the order of component names in the list assigned to the argument \code{attribute.levels}.
}

\value{
This function returns an object of S3 class \code{"cedes"} which is a list of the following components (see also the help for \code{\link[support.CEs]{rotation.design}} in the \bold{support.CEs} package):

\item{alternatives}{
A list of objects, \code{alt.j}: the \eqn{j}-th alternative in each choice set created by the function. Each \code{alt.j} includes the variables \code{BLOCK}, \code{QES}, \code{ALT}, and attribute variables corresponding to the argument \code{attribute.levels}.}

\item{design.information}{
Information related to the design for Case 3 BWS created by the function. This list includes objects such as the number of blocks into which the design is divided (\code{nblocks}), the number of questions per block (\code{nquestions}), the number of alternatives per choice set (\code{nalternatives}), and the number of attributes per alternative (\code{nattributes}).}
}

\references{
See the help page for \code{\link[support.BWS3]{support.BWS3-package}}.
}

\seealso{
\code{\link[support.BWS3]{bws3.dataset}}, \code{\link[support.BWS3]{bws3.response}}, \code{\link[support.BWS3]{support.BWS3-package}}, \code{\link[support.CEs]{rotation.design}}
}

\examples{
# See the section Examples in the help page for the function bws3.response.
}

\keyword{ design }

