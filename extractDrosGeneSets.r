#R
#GJR 6/15/2017
#create gene lists (in list form) for all GO and Kegg ids annotated in the bioconductor drosophila annotate package
#saves lists to r data file in current working directory
#REQUIRES: Bioconductor and 'org.Dm.eg.db'

extractDrosGeneSets<-function() {
    library(org.Dm.eg.db)
    getGenesInSet<-function(catId,db) {
        memberEgs<-get(catId,get(db))
        memberFbs<-unlist(mget(memberEgs,revmap(get(fbToEg))))
        return(memberFbs)
    }
    fbToEg<-'org.Dm.egFLYBASE2EG'
    db='org.Dm.egGO2EG'
    catIds<-Rkeys(get(db))
    DrosGoLists <- vector("list", length(catIds))
    for (i in 1:length(catIds)) {
        DrosGoLists[[i]]<-getGenesInSet(catIds[i],db)
    }
    names(DrosGoLists)<-catIds
    db='org.Dm.egPATH2EG'
    catIds<-Rkeys(get(db))
    DrosKeggLists <- vector("list", length(catIds))
    for (i in 1:length(catIds)) {
        DrosKeggLists[[i]]<-getGenesInSet(catIds[i],db)
    }
    names(DrosKeggLists)<-catIds
    save(DrosKeggLists,DrosGoLists,file='DrosGoAndKeggGeneLists.rdat')
}
