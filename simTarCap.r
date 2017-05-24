flen=400 #fragment length to be sequenced
tarlen=100 #length of target region
minMatch=40 #length of shortest match that will still capture the fragment


span=tarlen+2*(flen-minMatch) #total lenth of sequence where cutsite could yield a capturable fragment
tarpos=c((span/2)-tarlen/2,(span/2)+tarlen/2) #target position

nrep=1000
cutsite<-sample(1:span,nrep,replace=T) #sample cutsite
direction<-sample(1:2,nrep,replace=T) #randomly determine whether fragment is 5' or 3' of the cutsite

capturable=c(0,0)
for (i in 1:nrep) {
	cut=cutsite[i]
	if (direction[i]==1) {
		frag<-c(cut,(cut+400))
	} else {
		frag<-c((cut-400),cut)
	}
	if (frag[1] >=1 & frag[2] <= span) {
		capturable=rbind(capturable,frag)
	}
}
capturable<-capturable[-1,]

snpPos=span/2 #assume snp position is dead center in target
readlen=100 #read length, assume paired end

coverage=0
kept<-rep(FALSE,nrow(capturable))
for (i in 1:nrow(capturable)) {
	forwardEnd=capturable[i,1]+(readlen-1)
	reverseStart=capturable[i,2]-(readlen-1)
	if ( (snpPos >= capturable[i,1] & snpPos <= forwardEnd) | 
	(snpPos >= reverseStart & snpPos <= capturable[i,2]) ) {
		coverage=coverage+1
		kept[i]=TRUE
	}
}
percentReadsCoveringSnp=coverage/nrow(capturable)
percentReadsCoveringSnp
