#R



#create plot with black backgroud

fm<-lm(atrans~ltrans,data=data)
par(bg='black',fg='white')
plot(data$ltrans,data$atrans,col.lab='white',col.axis='white')
abline(fm,col='white')

#change libpath to install in root-access directory (make library available to all users)
#for lab linux machine example
.libPaths("/usr/lib/R/library")
install.packages()

#read delimited files with special characters
data<-read.table('S1_expressionAndAnno.txt',header=T,sep="\t",row.names=NULL,stringsAsFactors=F,quote="\"",comment.char="")

#estimate p val from point estimate and null (random) distribution
permPval<-function(vec,est) {
  prob<-ecdf(vec)(est)
  if (prob <= 0.5) {p=prob} else {p=1-prob}
  p=p*2 #accounts for 2-tailed search
  return(p)
}


#heat mapping
library(gplots)
library(RColorBrewer)

mat<-matrix(nrow=100,ncol=10,byrow=T,rnorm(100*10)*4)

chop<-function(x,thresh=2) {
  elchop<-function(y) {
    if (is.na(y)==FALSE & abs(y) > thresh) {
      if (y > thresh) {y<-thresh}
      if (y < -thresh) {y<--thresh}
    }
    return(y)
  }
  sapply(x,elchop)
}

clus<-heatmap.2(mat,col=brewer.pal(10,"PiYG"),trace="none",Colv=F)
choppedData<-apply(mat,2,chop,thresh=2)
heatmap.2(choppedData,col=brewer.pal(10,"PiYG"),trace="none",Colv=F,Rowv=clus$rowDendrogram)


