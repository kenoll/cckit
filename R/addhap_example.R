#install cckit from kelsey's github (only need to do this the first time)
devtools:::install_github("kenoll/cckit")

#load required packages
library(ggplot2)
library(cckit)

#set region of interest
chromo=11
start=70.5
end=72
locus.name="chr11"

#set allele effects
allele.effects=c(
  A.score=1 ,
  B.score=1 ,
  C.score=1 ,
  D.score=1 ,
  E.score=1 ,
  F.score=1 ,
  G.score=1 ,
  H.score=0 )

#haploscores2 only gives haplotypes for strains which have neither
#heterozygosity nor recombination in the region of interest
#use "haploscores" to get all strains
#or "haploscores3" to get ONLY strains with heterozygosity/recombination
haplos=haploscores2(chromo,start,end,allele.effects)
# write.csv(haplos,"~/Desktop/haplotypes.csv") #save it if you want it

#if you just want a list of which strains have which haplotypes, stop here
#if you want to associate haplotypes with a data file of phenotypes, continue

#read in data file with phenotype
pheno <- read.csv("~/Dropbox/Heise/U19-Ab/antibody/data_bin/d10/dat.10.auc.csv")

#set a name for the locus to identify it
locus.name="chr11"

#set the name of the column in your phenotype dataframe that has your strains in it
mice="RIX"

#add the haplotype information to the phenotype data frame
pheno <- addhap(pheno,haplos,locus.name,mice)

#plot genotype by phenotype
inheritance.model="add_chr11" #set inheritance model to plot on the x axis
response.var="IgG3"           #set phenotype to plot on the y axis

g <- ggplot(pheno, aes_string(inheritance.model,response.var))

g + geom_jitter(aes_string(color=inheritance.model),na.rm=T)+
  geom_boxplot(aes_string(color=inheritance.model,fill=inheritance.model,alpha=.5),outlier.alpha=0,na.rm=T)+
  theme_bw()+theme(legend.position='none')
