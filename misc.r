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

