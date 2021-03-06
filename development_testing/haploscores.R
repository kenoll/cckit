#' @title Locus Haplotype Scores
#'
#' @description Get founder haplotype at a locus of interest
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

locushaplos <- function(chromo,start,end){
  region.1 <- subset(mosaics,
                     mosaics$start_position<=start*1000000 &
                       mosaics$end_position>=end*1000000 &
                       mosaics$chromosome==chromo)

  region.2 <- subset(mosaics,
                     mosaics$start_position<=start*1000000 &
                       mosaics$end_position>=start*1000000 &
                       mosaics$chromosome==chromo)

  region.3 <- subset(mosaics,
                     mosaics$start_position<=end*1000000 &
                       mosaics$end_position>=end*1000000 &
                       mosaics$chromosome==chromo)

  region.4 <- subset(mosaics,
                     mosaics$start_position>=start*1000000 &
                       mosaics$end_position<=end*1000000 &
                       mosaics$chromosome==chromo)

  region <- merge(region.2,region.3,all=T) %>% merge(region.4,all=T) %>% merge(region.1,all=T)

  return(region)
}

#' @title Haplotype Scores
#'
#' @description Associate allele effect scores with each CC strain based on founder haplotype
#'
#' @param chromo Integer, chromosome number.
#' @param start Numeric, locus start position (Mb).
#' @param end Numeric, locus end position (Mb).
#' @param allele.effects Data frame, describes allele effects
#' (e.g. A.score = 0, B.score = 1...)
#'
#' @return Data frame containing strain, alias, chromosome,
#' haplotype, start_position, end_position, allele effect score, and founder
#'
#' @examples allele.effects=c(
#' A.score <- 0 ,
#' B.score <- 1 ,
#' C.score <- 0 ,
#' D.score <- 1 ,
#' E.score <- 0 ,
#' F.score <- 0 ,
#' G.score <- 0 ,
#' H.score <- 1 )
#'
#' haploscores(1, 70.5, 72, allele.effects)
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
#' @description Associate allele effect scores with each CC strain
#' based on founder haplotype, only for CC strains with NO recombinations
#' within the locus and NO residual heterozygosity
#'
#' @param chromo Integer, chromosome number.
#' @param start Numeric, locus start position (Mb).
#' @param end Numeric, locus end position (Mb).
#' @param allele.effects Data frame, describes allele effects
#' (e.g. A.score = 0, B.score = 1...)
#'
#' @return Data frame containing strain, alias, chromosome,
#' haplotype, start_position, end_position, allele effect score, and founder
#' @examples allele.effects=c(
#' A.score <- 0 ,
#' B.score <- 1 ,
#' C.score <- 0 ,
#' D.score <- 1 ,
#' E.score <- 0 ,
#' F.score <- 0 ,
#' G.score <- 0 ,
#' H.score <- 1 )
#'
#' haploscores2(chromo = 1, start = 70.5, end = 72)
#' @export

haploscores2 <- function(chromo,start,end,allele.effects){
  haplos2 <- haploscores(chromo,start,end,allele.effects)
  haplos2 <- haplos2[!(duplicated(haplos2$strain) | duplicated(haplos2$strain, fromLast = TRUE)),]
  return(haplos2)
}

#' @title Complex Haplotype Scores (Haplotype Scores 3)
#'
#' @description Associate allele effect scores with each
#' CC strain based on founder haplotype, ONLY for CC strains
#' WITH recombinations within the locus OR WITH residual heterozygosity
#'
#' @param chromo Integer, chromosome number.
#' @param start Numeric, locus start position (Mb).
#' @param end Numeric, locus end position (Mb).
#' @param allele.effects Data frame, describes allele effects
#' (e.g. A.score = 0, B.score = 1...)
#'
#' @return Data frame containing strain, alias, chromosome,
#' haplotype, start_position, end_position, allele effect score, and founder
#'
#'
#' @examples allele.effects=c(
#' A.score <- 0 ,
#' B.score <- 1 ,
#' C.score <- 0 ,
#' D.score <- 1 ,
#' E.score <- 0 ,
#' F.score <- 0 ,
#' G.score <- 0 ,
#' H.score <- 1 )
#'
#' haploscores3(1, 70.5, 72, allele.effects)
#'
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
#' @description Quickly generate scores based on haplotype effects with no founder attributions.
#' For strains with residual heterozygosity and/or recombinations, allele effects on the same
#' same chromosome are averaged, followed by averaging of the two chromosomes.
#'
#' @param chromo Integer, chromosome number.
#' @param start Numeric, locus start position (Mb).
#' @param end Numeric, locus end position (Mb).
#' @param allele.effects Data frame, describes allele effects (e.g. A.score = 0, B.score = 1...)
#'
#' @return Data frame containing strain, alias, and allele effects
#' @examples allele.effects=c(
#' A.score <- 0 ,
#' B.score <- 1 ,
#' C.score <- 0 ,
#' D.score <- 1 ,
#' E.score <- 0 ,
#' F.score <- 0 ,
#' G.score <- 0 ,
#' H.score <- 1 )
#'
#' quickeffects(chromo = 1, start = 70.5, end = 72, allele.effects)
#' @export

#### for effects only, not haplotypes ####
quickhaplos=function(chromo,start,end,allele.effects){
  haplos=haploscores(chromo,start,end,allele.effects)
  score.avg=summaryBy(allele.effects~strain+alias+chromosome+haplotype,
                      data=haplos, FUN=mean, na.rm=T)
  score.avg=summaryBy(allele.effects.mean~strain+alias,
                      data=score.avg, FUN=mean, na.rm=T)
  colnames(score.avg)[length(score.avg)] = "allele.effects"
  return(score.avg)
}

#' @title Locus Haplotype Scores - Same Effect Merged for Complex
#'
#' @description Associate allele effect scores with each CC strain
#' based on founder haplotype - ONLY for CC strains with recombinations
#' or residual heterozygosity that DO NOT EFFECT the haplotype score
#'
#' @param chromo Integer, chromosome number.
#' @param start Numeric, locus start position (Mb).
#' @param end Numeric, locus end position (Mb).
#' @param allele.effects Data frame, describes allele effects
#' (e.g. A.score = 0, B.score = 1...)
#'
#' @return Data frame containing strain, alias, chromosome,
#' haplotype, start_position, end_position, and founder
#'
#' @examples haploscores4(11, 70.5, 72, allele.effects)
#'
#' @export

haploscores4 <- function (chromo, start, end, allele.effects)
{

  haplos <- haploscores(chromo, start, end, allele.effects) %>%
    group_by(strain,alias,chromosome,allele.effects) %>%
    summarise(haplotype = paste(haplotype, collapse = ", "),
              start_position = paste(start_position, collapse = ", "),
              end_position = paste(end_position, collapse = ", "),
              founders = paste(founders, collapse = ", "))

  haplos <- haplos[!(duplicated(haplos$strain)|duplicated(haplos$strain,from.last=T)), ]

  return(haplos)
}


#' @title Locus Haplotypes without allele effects
#'
#' @description Assign CC strain haplotypes without locus scores
#'
#' @param chromo Integer, chromosome number.
#' @param start Numeric, locus start position (Mb).
#' @param end Numeric, locus end position (Mb).
#'
#' @return Data frame containing strain, alias, chromosome,
#' haplotype, start_position, end_position, and founder
#'
#' @examples locushaplos(11, 70.5, 72)
#'
#' @export

haplotypes <- function (chromo, start, end)
{
  haplos <- locushaplos(chromo,start,end)
  haplos <- haplos[!duplicated(haplos[,c('strain','alias','start_position','end_position','founders')]),]

  haplos <- haplos %>% group_by(strain,alias,chromosome) %>%
    summarise(haplotype = paste(haplotype, collapse = ", "),
              start_position = paste(start_position, collapse = ", "),
              end_position = paste(end_position, collapse = ", "),
              founders = paste(founders, collapse = ", "))

  return(as.data.frame(haplos))
}
