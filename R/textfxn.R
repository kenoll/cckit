#' @title test function
#'
#' @description test function
#'
#' @param chromo Integer, chromosome number.
#' @param start Numeric, locus start position (Mb).
#' @param end Numeric, locus end position (Mb).
#'
#' @return Data frame containing strain, alias, chromosome,
#' haplotype, start_position, end_position, and founder
#'
#' @examples locushaplos(1, 70.5, 72)
#'
#' @export

testfxn <- function(chromo,start,end){
  region.1 <- subset(mosaics,
                     mosaics$start_position<=start*1000000 &
                       mosaics$end_position>=end*1000000 &
                       mosaics$chromosome==chromo)
}
