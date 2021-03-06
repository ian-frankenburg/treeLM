% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseTreeSums.R
\name{parseTreeSums}
\alias{parseTreeSums}
\title{Parse BART-BMA object into linear model form}
\usage{
parseTreeSums(bart_bma)
}
\arguments{
\item{model}{a fitted BART-BMA object}
}
\value{
Following the notation in section 3.1
\itemize{
\item O - list of stacked terminal node parameters for trees within the sum
\item W - the big ANOVA design matrix combining all the individual ANOVAs to represent the sum-of-trees model
\item anovas - a list comprising of the individual design matrices that make up W
}
}
\description{
The parseTreeSums function transforms the fitted BART-BMA object into a set of
linear models as follows from section 3.1 in Hernandez, Belinda & Raftery, Adrian & Pennington, Stephen & Parnell, Andrew. (2017). Bayesian Additive Regression Trees using Bayesian Model Averaging. Statistics and Computing. 28. 10.1007/s11222-017-9767-1.
}
\details{
Represent a fitted regression or classification tree as an ANOVA model
}
\author{
Ian Frankenburg
}
