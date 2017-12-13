#call up master founder mosaic file
setwd("~/Dropbox/Heise/U19-Ab/qtl_info/")
mosaics=read.csv("~/Dropbox/Heise/CC/cc_founder_mosaics/founder_mosaics.csv")
mosaics$strain=gsub("/.+","",mosaics$strain)


#' @title Toolkit for Manipulation and Analysis of CC-derived data
#'
#' @description Add haplotypes at a locus of interest
#'
#' @param chromo Integer, chromosome number.
#' @param start Numeric, locus start position (Mb).
#' @param end Numeric, locus end position (Mb).
#'
#' @return region
#'
#' @examples head(locushaplos(1,10000000,13000000))
#'
#' @export


#functions
locushaplos <- function(chromo,start,end){
  region.2 <- subset(mosaics,
                  mosaics$start_position<=start &
                    mosaics$end_position>=start &
                    mosaics$chromosome==chromo)

  region.3 <- subset(mosaics,
                  mosaics$start_position<=end &
                    mosaics$end_position>=end &
                    mosaics$chromosome==chromo)

  region.4=subset(mosaics,
                  mosaics$start_position>=start &
                    mosaics$end_position<=end &
                    mosaics$chromosome==chromo)

  region=merge(region.2,region.3,all=T)
  region=merge(region,region.4,all=T)
  return(region)
}

#' @title Toolkit for Manipulation and Analysis of CC-derived data
#'
#' @description Add haplotypes at a locus of interest
#'
#' @param chromo Integer, chromosome number.
#' @param start Numeric, locus start position (Mb).
#' @param end Numeric, locus end position (Mb).
#' @param allele.effects Data frame, describes allele effects (e.g. A.score = 0, B.score = 1)
#'
#' @return haplos
#'
#' @examples
#'
#' @export


#
haploscores=function(chromo,start,end,allele.effect){
  region=locushaplos(chromo,start,end)
  founders=c("A","B","C","D","E","F","G","H")
  CC.effects=data.frame(founders,allele.effect)

  #eliminate second haplotype if strain has same founder for both (most cases)
  #if there's a double recombination so that the same founders repeat, it will only keep one of each
  haplos=merge(region,CC.effects,by="founders")
  haplos=haplos[order(haplos$strain,haplos$chromosome,haplos$haplotype,haplos$start_position),]
  haplos=haplos[c(2:length(haplos),1)]
  haplos$strain_founder=paste(haplos$strain,haplos$founder,sep="_")
  haplos = haplos[!duplicated(haplos$strain_founder),]
  haplos = haplos[1:(length(haplos)-1)]
  return(haplos)
}

#if you only want to look at un-ambiguous calls (no recombinations, no heterozygosity)
haploscores2=function(chromo,start,end,allele.effect){
  haplos2=haploscores(chromo,start,end,allele.effect)
  haplos2=haplos2[!(duplicated(haplos2$strain) | duplicated(haplos2$strain, fromLast = TRUE)),]
  return(haplos2)
}

get.multi=function(chromo,start,end,allele.effect){
  haplos=haploscores(chromo,start,end,allele.effect)
  haplos2=haploscores2(chromo,start,end,allele.effect)
  multi=anti_join(haplos,haplos2)
  multi=multi[order(multi$strain,multi$chromosome,multi$haplotype,multi$start_position),]
  return(multi)
}

#### for effects only, not haplotypes ####
get.quickeffects=function(chromo,start,end,allele.effect){
  haplos=haploscores(chromo,start,end,allele.effect)
  score.avg=summaryBy(allele.effect~strain+alias+chromosome+haplotype,
                      data=haplos, FUN=mean, na.rm=T)
  score.avg=summaryBy(allele.effect.mean~strain+alias,
                      data=score.avg, FUN=mean, na.rm=T)
  colnames(score.avg)[length(score.avg)] = "allele.effect"
  return(score.avg)
}

