haploscores4 <- function (chromo, start, end, allele.effects)

  {

haplos <- haploscores(chromo, start, end, allele.effects)

haplos <- haplos[-length(haplos)]

haplos <- split(haplos, haplos$allele.effects)

haplos1 <- haplos[[1]]
haplos1 <- haplos1[!(duplicated(haplos1$strain)), ]

haplos2 <- haplos[[2]]
haplos2 <- haplos2[!(duplicated(haplos2$strain)), ]
haplos <- rbind(haplos1,haplos2)

return(haplos)

}
