haploscores4 <- function (chromo, start, end, allele.effects)
  
{
  
  haplos <- haploscores(chromo, start, end, allele.effects) %>%
    group_by(strain,alias,chromosome,allele.effects) %>%
    summarise(haplotype = paste(haplotype, collapse = ", "),
              start_position = paste(start_position, collapse = ", "),
              end_position = paste(end_position, collapse = ", "),
              founders = paste(founders, collapse = ", "))
  
  haplos <- haplos[!(duplicated(haplos$strain)), ]

  return(haplos)
  
}

