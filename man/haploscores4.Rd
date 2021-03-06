% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/haploscores_2.R
\name{haploscores4}
\alias{haploscores4}
\title{Locus Haplotype Scores - Same Effect Merged for Complex}
\usage{
haploscores4(chromo, start, end, allele.effects)
}
\arguments{
\item{chromo}{Integer, chromosome number.}

\item{start}{Numeric, locus start position (Mb).}

\item{end}{Numeric, locus end position (Mb).}

\item{allele.effects}{Data frame, describes allele effects
(e.g. A.score = 0, B.score = 1...)}
}
\value{
Data frame containing strain, alias, chromosome,
haplotype, start_position, end_position, and founder
}
\description{
Associate allele effect scores with each CC strain
based on founder haplotype - ONLY for CC strains with recombinations
or residual heterozygosity that DO NOT EFFECT the haplotype score
}
\examples{
haploscores4(11, 70.5, 72, allele.effects)

}
