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

#Parallelize

gsa<-function(ids,data,processors=8,nit=50000) {

    medVal<-median(abs(data[data[,1] %in% ids,2]))
    n<-length(ids)
    library(doParallel)
    cl <- makeCluster(processors)
    registerDoParallel(cl)
    out<-foreach(i=1:nit,.combine='c') %dopar% {
        inds<-sample(1:nrow(data),n)
        return( median(abs(data[inds,2])) )
    }
    stopCluster(cl)
    return(1-ecdf(out)(medVal))
}

#Combine pvals with fisher method
fisherCombine<-function(x) {
    chisq<- -2*sum(log(x))
    return(1-pchisq(chisq,2*length(x)))
}


#change plot axis font size
plot(x,y,xlim=c(20,90),ylim=c(0,80),xlab='dosage EPO',ylab='hematocrit',cex.axis=2,cex.lab=2,cex=2)


#pre-planned contrasts
fm<-lm(sqrt(dayToEclosion)~factor(host)*factor(treatment),data=data[data$diapause==1,])

#                Intercept  haw  m2  m3  m4  m5  m6 h*m2  h*m3  h*m4  h*m5  h*m6
#h1                  1    ,   1,  0 , 0  , 0, 0,  0,   0,   0,    0,     0,   0 
#a1                  1    ,   0,  0 , 0  , 0, 0,  0,   0,   0,    0,     0,   0 
#a<-c(                0    ,   1,  0 , 0  , 0, 0,  0,   0,   0,    0,     0,   0 )

#values for coefficients in rows (contrasts in rows)
conMat<-cbind( c( 0    ,   1,  0 , 0  , 0, 0,  0,   0,   0,    0,     0,   0),
              c(0    ,   1,  0 , 0  , 0, 0,  0,   1,   0,    0,     0,   0),
              c(0    ,   1,  0 , 0  , 0, 0,  0,   0,   1,    0,     0,   0),
              c(0    ,   1,  0 , 0  , 0, 0,  0,   0,   0,    1,     0,   0),
              c(0    ,   1,  0 , 0  , 0, 0,  0,   0,   0,    0,     1,   0),
              c(0    ,   1,  0 , 0  , 0, 0,  0,   0,   0,    0,     0,   1))
library(multcomp)
a<-glht(fm,linfct=t(conMat))
summary(a)




# from https://stackoverflow.com/questions/63390194/package-xxx-was-installed-before-r-4-0-0-please-re-install-it
#remove all old packages from old R installation after installing new version
#first three lines optional, only if you want to re-install every
#single packge
old_packages <- installed.packages()
old_packages <- as.data.frame(old_packages)
list.of.packages <- unlist(old_packages$Package)
remove.packages( installed.packages( priority = "NA" )[,1] )


# reinstall all packages 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)})
