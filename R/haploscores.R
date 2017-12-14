#' @title Locus Haplotype Scores
#'
#' @description Add haplotypes and generate allele scores at a locus of interest
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

  region.4 <- subset(mosaics,
                  mosaics$start_position>=start &
                    mosaics$end_position<=end &
                    mosaics$chromosome==chromo)

  region <- merge(region.2,region.3,all=T)
  region <- merge(region,region.4,all=T)
  return(region)
}

#' @title Haplotype Scores
#'
#' @description Generate scores based on haplotype effects
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

haploscores <- function(chromo,start,end,allele.effects){
  region <- locushaplos(chromo,start,end)
  founders <- c("A","B","C","D","E","F","G","H")
  CC.effects <- data.frame(founders,allele.effects)

  #eliminate second haplotype if strain has same founder for both (most cases)
  #if there's a double recombination so that the same founders repeat, it will only keep one of each
  haplos <- merge(region,CC.effects,by="founders")
  haplos <- haplos[order(haplos$strain,haplos$chromosome,haplos$haplotype,haplos$start_position),]
  haplos <- haplos[c(2:length(haplos),1)]
  haplos$strain_founder <- paste(haplos$strain,haplos$founder,sep="_")
  haplos <- haplos[!duplicated(haplos$strain_founder),]
  haplos <- haplos[1:(length(haplos)-1)]
  return(haplos)
}


#' @title Haplotype Scores 2
#'
#' @description Generate scores based on haplotype effects only for animals
#' with no recombinations within the locus or heterozygosity
#'
#' @param chromo Integer, chromosome number.
#' @param start Numeric, locus start position (Mb).
#' @param end Numeric, locus end position (Mb).
#' @param allele.effects Data frame, describes allele effects (e.g. A.score = 0, B.score = 1)
#'
#' @return haplos
#' @examples
#' @export

haploscores2 <- function(chromo,start,end,allele.effects){
  haplos2 <- haploscores(chromo,start,end,allele.effects)
  haplos2 <- haplos2[!(duplicated(haplos2$strain) | duplicated(haplos2$strain, fromLast = TRUE)),]
  return(haplos2)
}

#' @title Complex Haplotype Scores
#'
#' @description Generate scores based on haplotype effects only for animals
#' with residual heterozygosity or recombinations within the locus
#'
#' @param chromo Integer, chromosome number.
#' @param start Numeric, locus start position (Mb).
#' @param end Numeric, locus end position (Mb).
#' @param allele.effects Data frame, describes allele effects (e.g. A.score = 0, B.score = 1)
#'
#' @return multi
#' @examples
#' @export

haploscores3 <- function(chromo,start,end,allele.effects){
  haplos <- haploscores(chromo,start,end,allele.effects)
  haplos2 <- haploscores2(chromo,start,end,allele.effects)
  multi <- anti_join(haplos,haplos2)
  multi <- multi[order(multi$strain,multi$chromosome,multi$haplotype,multi$start_position),]
  return(multi)
}

#' @title Quick Allele Effects
#'
#' @description Quickly enerate scores based on haplotype effects with no founder attributions
#' For strains with residual heterozygosity and/or recombinations
#' Starts by averaging all allele effects on same chromosome
#' Then averages chromosomes
#'
#' @param chromo Integer, chromosome number.
#' @param start Numeric, locus start position (Mb).
#' @param end Numeric, locus end position (Mb).
#' @param allele.effects Data frame, describes allele effects (e.g. A.score = 0, B.score = 1)
#'
#' @return score.avg
#' @examples
#' @export

#### for effects only, not haplotypes ####
get.quickeffects=function(chromo,start,end,allele.effects){
  haplos=haploscores(chromo,start,end,allele.effects)
  score.avg=summaryBy(allele.effects~strain+alias+chromosome+haplotype,
                      data=haplos, FUN=mean, na.rm=T)
  score.avg=summaryBy(allele.effects.mean~strain+alias,
                      data=score.avg, FUN=mean, na.rm=T)
  colnames(score.avg)[length(score.avg)] = "allele.effects"
  return(score.avg)
}

