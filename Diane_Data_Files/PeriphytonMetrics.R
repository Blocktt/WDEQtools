#### Calculate WY USGS metrics 

#### bottom half of code adapted from: R_NATIONAL_metric_IN_20200609.R
#### NATIONAL metric workup S.A. Spaulding 31 January 2020 ~~ Revised by BJessup for IN
#### for use with WY dataset

#### Apply USGS traits tables to WY dataset
#### First, add all missing taxa to traits tables, even if there are no traits

library("readxl")
library (vegan)
library(reshape2)
library(dplyr)
library(qdapTools)

#~~ Set working directory
setwd("C:/Users/diane.allen/Documents/WYdiatoms")

diatoms<- read_excel("DiatomSampleTaxaList_ni.xlsx",sheet=1); names(diatoms); dim(diatoms)				##### 31005  9
USGStraits<-read_excel("Traits_USGS_July2021_Export.xlsx",sheet=1); names(USGStraits); dim(USGStraits)	##### 2565  63

length(unique(diatoms$DiatomTaxon_WDEQ))														#####  862													##### 747 includes the 0 so 746
length(unique(diatoms$Taxa_MatchUSGStraits))													##### 756  

length(unique(diatoms$Taxa_MatchUSGStraits[diatoms$Taxa_MatchUSGStraits %in% USGStraits$TAXON]))		##### 746   matches	

#### taxa without traits  
missing<-unique(diatoms$Taxa_MatchUSGStraits[!diatoms$Taxa_MatchUSGStraits %in% USGStraits$TAXON])			##### 

#table(diatoms$DiatomTaxon_WDEQ[diatoms$DiatomTaxon_WDEQ %in% missing])
#                 Halamphora         Kolbesia ploenensis        Kolbesia suchlandtii 
#                          3                           1                           1 
#                 Parlibellus 
#                          1 
#              Rhoicosphenia                Rossithidium         Staurophora brantii 
#                         13                           1                           1 
#      Undetermined Pennate             Unknown centric               unknown genus 
#                         17                           1                           2 

missing<-missing[-c(3,4,9)]
############# findout if the IDs are taxa ids or just an index
addendum<-data.frame(ID=2567:(2567+(length(missing)-1)),TAXON=missing)

for (i in 3:(dim(USGStraits)[2]))  {
addendum[,i]<-0
cbind(addendum,addendum[,i])
}
head(addendum)
names(addendum)<-names(USGStraits)

USGStraits_mod<-rbind(USGStraits,addendum)
write.csv(USGStraits_mod,"USGStraits_addended.csv",row.names=F)

# Read species abundance table (long format). 
biolist<-diatoms
head(biolist)

###### make new sampleID with rep number included
names(biolist)
biolist$SAMPLEID<-paste(biolist$PerSampID,biolist$RepNum,sep="_")
head(biolist$SAMPLEID)
dim(biolist); names(biolist)														#### 31005   10
# Omit some fields (did not do)
# biolist <- biolist [,c(1,2,5:7)]
length(unique(biolist$SAMPLEID))												####  708
biolist<-	biolist [,c(7,8,10)]											#### 629
# Check the sum of counts for each sample. 

TotValves <- as.data.frame(tapply(biolist$Individuals,biolist$SAMPLEID,sum)); hist(TotValves[,1]); range(TotValves[,1])
par(mfrow=c(1,1));hist(TotValves[,1],xlab="Valve count per sample")

# added conversion to RA
dim(biolist)		##### 31005     3
table(duplicated(biolist[,c("SAMPLEID","Taxa_MatchUSGStraits")]));biolist[duplicated(biolist[,c("SAMPLEID","Taxa_MatchUSGStraits")]),]
biolist2<-aggregate(biolist$Individuals, FUN=sum,by=list(biolist$SAMPLEID,biolist$Taxa_MatchUSGStraits)); head(biolist2)
dim(biolist2)   ########### 30902     3   there were 103 duplicates
										
names(biolist2)<-c("SAMPLEID","TAXON","COUNT")
TotValves <- as.data.frame(tapply(biolist2$COUNT,biolist2$SAMPLEID,sum)); dim(TotValves); names(TotValves)[1]<-"Total"
TotValves$SAMPLEID<-row.names(TotValves)
# Remove spaces in taxon names. Replace white space with "_". Shouldn't be any at this point...
biolist2$TAXON <- gsub(" ", "_", biolist2$TAXON)
biolist3<-merge(biolist2,TotValves); dim(biolist3); names(biolist3); head(biolist3)
biolist3$COUNT<-biolist3$COUNT/biolist3$Total
names(biolist3); head(biolist3)


#Read dataframe of traits table where first column is "TAXON", successive columns are binary traits
head(USGStraits_mod)
traits <- USGStraits_mod[,-1]
#~~ colnames(traits)[1] = "TAXON"; head(traits)

# how many unique values in dataframe by column; should be two for all metric columns
rapply(traits,function(x)length(unique(x))); unique(rapply(traits,function(x)length(unique(x))))

#what proportion of the TAXON list are found in each metric category
#~~ For the whole traits taxa list, what proportion of taxa have a "1" value?
otusinmetrics <- colSums(traits[,-1], na.rm=TRUE)/nrow(traits)

#what proportion of the OTU list are found in each metric
#note that this only looks at the list of taxa in the traits table. It does not say anything about the taxa in the
#dataset and their occurrence in particular metrics
otusinentiremetrics <- data.frame(BC = sum (otusinmetrics [1:5]),
                                  TROPHIC = sum (otusinmetrics [6:12]), 
                                  SAP = sum (otusinmetrics [13:17]),
                                  POLLUTION_TOLERANCE = sum (otusinmetrics [18:22]),
                                  OXYGEN = sum (otusinmetrics [23:27]),
                                  SALINITY = sum (otusinmetrics [28:31]),
                                  BAHLS = sum (otusinmetrics [32:34]),
                                  PandN = sum (otusinmetrics [35:38]),
                                  BENTHIC_SESTONIC = sum (otusinmetrics [39:40]),
                                  N_FIX = sum (otusinmetrics [41:42]),
                                  MOTILITY = sum (otusinmetrics [43:47]),
                                  SIZE = sum (otusinmetrics [48:52]),
                                  ACHNANTHIDIUM = otusinmetrics [53],
                                  ACHNANTHIDIDAE = otusinmetrics [54],
                                  BACILLARIACEAE = otusinmetrics [55],
                                  NAVICULA = otusinmetrics [56],
                                  STALKED = otusinmetrics [57],
                                  ADNATE = otusinmetrics [58],
                                  HIGHLY_MOTILE.1 = otusinmetrics [59],
                                  ARAPHID = otusinmetrics [60],
                                  CENTRIC = otusinmetrics [61])
#Export to file.  
#~~ For each traits category, what proportion of taxa had positive values
write.csv(x=otusinentiremetrics, file=paste("metrictaxafound_",format(Sys.time(), "%Y%m%d"), ".csv", sep = ""), row.names=FALSE)

# Do some data checking to determine how these tables overlap

# what taxa are present?
taxa <- unique(biolist3$TAXON)

# what taxa are in the counts, but not in traits table?
traits$TAXON <- gsub(" ", "_", traits$TAXON)
notintraits <- setdiff(unique(biolist3$TAXON), unique(traits$TAXON))   ##### 
####"unknown_genus"        "Undetermined_Pennate" "Unknown_centric" 

#what taxa are in traits table, but not in the counts?
notincounts <- setdiff(unique(traits$TAXON), unique(biolist3$TAXON))  #### 

biolist<-biolist3;			length(unique(biolist$SAMPLEID))			#### 708
#Merge traits table and biological table.  NOTE that OTUs in the species abundance table that don't match with an 
#OTU in the traits table will be LOST.
ttmerge <- merge (biolist, traits, by.x = "TAXON", by.y = "TAXON")
length(unique(ttmerge$SAMPLEID))									#### 708
# Select only the records for which a taxon was recorded, that is, when relabundance is greater than zero
tttmerge <- ttmerge[ which(ttmerge$COUNT > 0),]; names(tttmerge); tttmerge<-tttmerge[,-4]  ##### get rid of "Total"

ValvesWithTraits <- as.data.frame(tapply(tttmerge$COUNT,tttmerge$SAMPLEID,sum)); head(ValvesWithTraits)
dim(ValvesWithTraits)
#Replicate vector containing "SAMPLEID" (unique sample ID) so it can be used as list for the aggregate function
tsample <- tttmerge$SAMPLEID
head(tsample); length(unique(tsample))

#~~ Check the new data
write.csv(x=tttmerge, file=paste("biolist_withtraits09202021.csv", sep = ""), row.names=FALSE)

### CREATE MATRIX of TOTAL SAMPLE RICHNESS FOR EACH TRAIT

#Note which columns have numeric data, then index these columns in the aggregate command. 
#Be certain to index [,X:X], where X are the numbers of the columns containing numeric data 
#(including the "relabun" field). That is,  NOTE that the number of columns may need to be 
#adjusted depending on the number of metrics in each file.
names(tttmerge)

#Aggregate the data, by sample. Check for the appropriate number of colums to select
traitrich <- aggregate (tttmerge[,3:ncol(tttmerge)], list(SAMPLEID = tsample), sum, na.rm = T)
head(traitrich); dim(traitrich)						#### 708  63

### CREATE MATRIX OF PROPORTION SAMPLE RICHNESS FOR EACH TRAIT

#Returns two vectors:  "TID" and the count of TAXON records from the tttmerge file THAT WERE IN THE TRAITS TABLE!
taxacount <- tapply (tttmerge[ ,1], tttmerge$SAMPLEID, length)
head(taxacount)

#Convert table to a matrix
taxacount<-as.matrix(taxacount)
head(taxacount)

#Multiply the taxacount array and the traitrich dataframe.  Index the columns in traitrich that contain TRAIT data
names(traitrich)
proprich <- traitrich[,3:ncol(traitrich)]/taxacount

#combine vector of taxa counts and proportions table
proptaxarich<- cbind(taxacount,proprich)
proptaxarich$SAMPLEID <- row.names(proptaxarich)

# make "SAMPLEID" the first column
proptaxarich<-proptaxarich[ ,c(ncol(proptaxarich), 1:(ncol(proptaxarich)-1))]
head(proptaxarich)[,1:8]; dim(proptaxarich)

####CREATE MATRIX OF TRAIT PROPORTION ABUNDANCE  
names(tttmerge)
#Multiply all trait columns [INDEX THEM]in tttmerge by relative abundance   names(tttmerge); dim(tttmerge)
counts.ini<-tttmerge[,4:ncol(tttmerge)]*tttmerge$COUNT
head(counts.ini)
dim(counts.ini)
#Connect new file with SampleID and Counts
counts<-cbind(tttmerge[,c(2,3)],counts.ini)
names(counts)
head (counts)

#Sum across taxa within each sample as done previous for sample richness
tsample<- tttmerge[,2]
head (tsample); length(unique(tsample))

traitcounts<- aggregate(counts[,3:ncol(counts)],list(SAMPLEID=tsample),sum,na.rm=T)
names(traitcounts)

#Now, procede as before for proportion sample richness.  First, get sample count totals. They should sum to 1, or close to that.
sampletotals<- tapply(tttmerge[,3],tttmerge$SAMPLEID,sum)

#Then, convert to matrix
sampletotals<-as.matrix(sampletotals); dim(sampletotals); table(sampletotals)


#Multiply the sample count array and the traitcount dataframe.  Index the columns that contain trait data
propabund<-traitcounts[,2:ncol(traitcounts)]/sampletotals; dim(propabund)

#combine vector of sample counts and proportions table
propabundtrait<- cbind(sampletotals,propabund)
propabundtrait$SAMPLEID<-row.names(propabundtrait)
propabundtrait<-propabundtrait[ ,c(ncol(propabundtrait), 1:(ncol(propabundtrait)-1))]
head(propabundtrait)[,1:8]

####################
# Add fields from site table  names(diatoms)
siteinfo<-unique(diatoms[,1:4]); dim(siteinfo); head(siteinfo); table(diatoms$RepNum)
siteinfo$SAMPLEID<-paste(siteinfo$PerSampID,siteinfo$RepNum,sep="_")

siteinfo1<-unique(diatoms[,1:3]); dim(siteinfo1); length(unique(siteinfo$PerSampID))
names(siteinfo)[3]<-"SAMPLEID"

site_traitrich <- merge(siteinfo, traitrich); dim(site_traitrich)  					##### 708  67
site_proptaxarich <- merge (siteinfo, proptaxarich); dim(site_proptaxarich)			##### 708  67
site_propabundtrait <- merge (siteinfo, propabundtrait); dim(site_propabundtrait)		##### 708  67

names(site_traitrich); names(site_traitrich)[7:67]<- paste(names(site_traitrich)[7:67],"r",sep=".")
names(site_proptaxarich); names(site_proptaxarich)[7:67]<- paste(names(site_proptaxarich)[7:67],"pt",sep=".")
names(site_propabundtrait); names(site_propabundtrait)[7:67]<- paste(names(site_propabundtrait)[7:67],"pa",sep=".")

###########################################################
#EXPORTING TO CSV


#Export to file.  
write.csv(x=site_traitrich, file=paste("WY_TraitRich_",format(Sys.time(), "%Y%m%d"), ".csv", sep = ""), row.names=FALSE)

write.csv(x=site_proptaxarich, file=paste("WY_TraitPropRich_",format(Sys.time(), "%Y%m%d"), ".csv", sep = ""), row.names=FALSE)

write.csv(x=site_propabundtrait, file=paste("WY_TraitPropAbund_",format(Sys.time(), "%Y%m%d"), ".csv", sep = ""), row.names=FALSE)

##########   END OF CODE  ##################
