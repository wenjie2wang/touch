\name{cmbd}
\alias{cmbd}
\title{
Comorbidity measures from AHRQ HCUP
}
\description{
  This function maps a matrix of ICD9 codes to AHRQ comorbidity measures.
}
\usage{
cmbd(icd, drg = NULL, needClean = TRUE, needPrep = TRUE)
}
\arguments{
  \item{icd}{
    a character matrix of icd9 codes, with rows representing patients.
  }
  \item{drg}{
    a numeric vector of drg codes, with length the same as \code{nrow(icd)}.
  }
  \item{needClean}{
    logical, TRUE means cleaning is needed (string trimming, zero supplementation)
  }
  \item{needPrep}{
    logical, TRUE means preparation is needed (convert char to numeric)
  }
}
\value{
  a matrix with the same number of rows as the input
  and with the comorbidity measures in columns
}
\references{
  Elixhauser et. al. (1998)
}
\author{
  Jun Yan
}
\examples{
data(icd9sample)
output <- cmbd(icd9sample)
}
\keyword{manipulation}
