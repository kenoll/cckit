alleles.to.numeric=function(genos){
  genos <- as.matrix.data.frame(genos)
  
  for (i in 1:nrow(genos)){
    preserve.alleles=data.frame(A1=genos[i,"A1"],A2=genos[i,"A2"])
    preserve.alleles=as.matrix(preserve.alleles)
    temp=genos[i,]
    temp[temp==temp["A1"]]=0
    temp[temp==temp["A2"]]=2
    temp[temp=="H"]=1
    temp[temp=="N"]=NA
    temp["A1"]=preserve.alleles[1]
    temp["A2"]=preserve.alleles[2]
    genos[i,]=temp
  }
  
  genos=as.data.frame(genos)
  genos=droplevels(genos)
}