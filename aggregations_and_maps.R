##You will need to merge this file(Tracts to NSA) with the tracts file that you want and the other file with NSAs to the zonings file.

##You can apply aggregate(variable of interest~NSA_NAME, data=tracts, FUN) to create aggregated demographic measures (say, mean medincome of the region).

##You can do something similar with aggregate(variable of interest~NSA_NAME, data=zonings, FUN) to create aggregated zoning measures.


Censustracks<-read.csv('/home/havb/code/Hassan/Census Tracts.csv')
View(Censustracks)

Ecometrics<-read.csv('/home/havb/code/Hassan/CT_All_Ecometrics_2014.csv')
View(Ecometrics)


ZoningNSA<-read.csv('/home/havb/code/Hassan/Tracts to NSA.csv')
View(ZoningNSA)

Baritracks<-read.csv('/home/havb/code/Hassan/Tracts_Boston_BARI.csv')
View(Baritracks)

ZoningNSAID<-read.csv('/home/havb/code/Hassan/Zones_with_NSA_R_ID.csv')
View(ZoningNSAID)
options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)options(scipen = 999)

ZoningClearancesFinal<-read.csv('/home/havb/code/Hassan/Zoning Clearances.csv')



#Checking and identifying required manifest variables##
options(scipen = 999)

View(ZoningClearancesFinal[90:100])
View(ZoningClearancesFinal[13:18])
View(ZoningClearancesFinal[78:88])
View(ZoningClearancesFinal[71:77])
View(ZoningClearancesFinal[19:29])
View(ZoningClearancesFinal[52:56])


#Creating and calculating the subindex variables##

ZoningClearancesFinal$Elderly_Healthcare_Subindex<-apply(ZoningClearancesFinal[52:56]=='A',1,mean)
ZoningClearancesFinal$Elderly_Culture_Subindex<-apply(ZoningClearancesFinal[19:29]=='A',1,mean)
ZoningClearancesFinal$Elderly_Openspace_Subindex<-apply(ZoningClearancesFinal[71:77]=='A',1,mean)
ZoningClearancesFinal$Elderly_Publicservice_Subindex<-apply(ZoningClearancesFinal[78:88]=='A',1,mean)
ZoningClearancesFinal$Elderly_Community_Subindex<-apply(ZoningClearancesFinal[13:18]=='A',1,mean)
ZoningClearancesFinal$Elderly_Residential_Subindex<-apply(ZoningClearancesFinal[90:100]=='A',1,mean)


#Aggregating the Sub-Index Variables by District 

DistrictHealthAgg<-aggregate(Elderly_Healthcare_Subindex~district,data=ZoningClearancesFinal,mean)
View(DistrictHealthAgg)
DistrictCultureAgg<-aggregate(Elderly_Culture_Subindex~district,data=ZoningClearancesFinal,mean)
View(DistrictCultureAgg)
DistrictOpenAgg<-aggregate(Elderly_Openspace_Subindex~district,data=ZoningClearancesFinal,mean)
View(DistrictOpenAgg)
DistrictPubAgg<-aggregate(Elderly_Publicservice_Subindex~district,data=ZoningClearancesFinal,mean)
View(DistrictPubAgg)
DistrictCommAgg<-aggregate(Elderly_Community_Subindex~district,data=ZoningClearancesFinal,mean)
View(DistrictCommAgg)
DistrictResAgg<-aggregate(Elderly_Residential_Subindex~district,data=ZoningClearancesFinal,mean)
View(DistrictResAgg)

#Creating a separate dataframe for the Elderly Livability Index
Elderly_Livability_Index<-data.frame(aggregate(cbind(Elderly_Healthcare_Subindex,Elderly_Community_Subindex,Elderly_Publicservice_Subindex,Elderly_Openspace_Subindex,Elderly_Culture_Subindex,Elderly_Residential_Subindex)~district,ZoningClearancesFinal,mean))

#Created Total ,and Weighted Index Score Variables
Elderly_Livability_Index$Total_Index_Score<-rowSums(Elderly_Livability_Index[,c(2:7)])
Elderly_Livability_Index$Total_Weighted_Score<-(Elderly_Livability_Index$Elderly_Healthcare_Subindex*1)+(Elderly_Livability_Index$Elderly_Community_Subindex*0.8)+(Elderly_Livability_Index$Elderly_Publicservice_Subindex*0.6)+(Elderly_Livability_Index$Elderly_Openspace_Subindex*0.4)+(Elderly_Livability_Index$Elderly_Culture_Subindex*0.2)+(Elderly_Livability_Index$Elderly_Residential_Subindex*0.1)
View(Elderly_Livability_Index)

#Merging Zoning Clearances Final & ZoningNSAID
ZoningNSAMerge<-merge(ZoningClearancesFinal,ZoningNSAID,by='cartodb_id')
names(ZoningNSAMerge)

#Creating Aggregations between Sub-Indexes and NSA Name
AggHealthNSA<-aggregate(Elderly_Healthcare_Subindex~NSA_NAME,data=ZoningNSAMerge,mean)
AggResNSA<-aggregate(Elderly_Residential_Subindex~NSA_NAME,data=ZoningNSAMerge,mean)
AggPubNSA<-aggregate(Elderly_Publicservice_Subindex~NSA_NAME,data=ZoningNSAMerge,mean)
AggComNSA<-aggregate(Elderly_Community_Subindex~NSA_NAME,data=ZoningNSAMerge,mean)
AggOpenNSA<-aggregate(Elderly_Openspace_Subindex~NSA_NAME,data=ZoningNSAMerge,mean)
AggCulNSA<-aggregate(Elderly_Culture_Subindex~NSA_NAME,data=ZoningNSAMerge,mean)
View(AggOpenNSA)

#Merging Census Data & ZoningNSA Key
CensusNSAMerge<-merge(ZoningNSA,Censustracks,by='CT_ID_10')
names(CensusNSAMerge)


CensusIndex<-merge(CensusNSAMerge,AggHealthNSA,by='NSA_NAME')
CensusIndex<-merge(CensusNSAMerge,AggResNSA,by='NSA_NAME')
CensusIndex<-merge(CensusNSAMerge,AggPubNSA,by='NSA_NAME')
CensusIndex<-merge(CensusNSAMerge,AggComNSA,by="NSA_NAME")
CensusIndex<-merge(CensusNSAMerge,AggOpenNSA,by='NSA_NAME')
CensusIndex<-merge(CensusNSAMerge,AggCulNSA,by='NSA_NAME')
names(CensusIndex)


#Merging Sub-index NSA Aggregations with each other by NSA NAME 
NSAaggregations<-merge(AggCulNSA,AggOpenNSA,by='NSA_NAME')
NSAaggregations<-merge(NSAaggregations,AggComNSA,by='NSA_NAME')
NSAaggregations<-merge(NSAaggregations,AggPubNSA,by='NSA_NAME')
NSAaggregations<-merge(NSAaggregations,AggResNSA,by='NSA_NAME')
NSAaggregations<-merge(NSAaggregations,AggHealthNSA,by='NSA_NAME')
View(NSAaggregations)

CensusIndex<-merge(CensusNSAMerge,NSAaggregations,by='NSA_NAME')

#Aggregating Important Variables with NSA names within the Census dataset
Census65Agg<-aggregate(p65older~NSA_NAME,data=CensusIndex,mean)
CensusWhiAgg<-aggregate(propwhite~NSA_NAME,data=CensusIndex,mean)
CensusBlkAgg<-aggregate(propblack~NSA_NAME,data=CensusIndex,mean)
CensusAsAgg<-aggregate(propasian~NSA_NAME,data=CensusIndex,mean)
CensusOtherRaceAgg<-aggregate(propother~NSA_NAME,data=CensusIndex,mean)
CensusImmigAgg<-aggregate(propimmigrant~NSA_NAME,data=CensusIndex,mean)
CensusParkAgg<-aggregate(Park~NSA_NAME,data=CensusIndex,mean)

#Merging Aggregated Census Variables together
CensusAggMerge<-merge(Census65Agg,CensusWhiAgg,by='NSA_NAME')
CensusAggMerge<-merge(CensusAggMerge,CensusBlkAgg,by='NSA_NAME')
CensusAggMerge<-merge(CensusAggMerge,CensusOtherRaceAgg,by='NSA_NAME')
CensusAggMerge<-merge(CensusAggMerge,CensusAsAgg,by='NSA_NAME')
CensusAggMerge<-merge(CensusAggMerge,CensusImmigAgg,by='NSA_NAME')
CensusAggMerge<-merge(CensusAggMerge,CensusParkAgg,by='NSA_NAME')
names(CensusAggMerge)





#Merging CensusAggMerge(Aggregated Census Variables) with NSA Aggregations(Sub Index NSA Aggregations)
CensusZoningNSA<-merge(CensusAggMerge,NSAaggregations,by='NSA_NAME')
View(CensusZoningNSA)
names(CensusZoningNSA)




######################
######## MAPS ########
######################

library(dplyr)
library(extrafont)
library(rgdal)
library(maptools)
library(ggplot2)

tracts.shp <- readOGR(dsn = '/home/havb/data/Boston/shapefiles/Tracts_Boston_2015_BARI', 'Tracts_Boston BARI')

tracts.shp <- merge(tracts.shp, CensusIndex, by = 'CT_ID_10', all.x = TRUE, sort = FALSE)

tracts.df <- fortify(tracts.shp, region = "CT_ID_10")

tracts.df <- merge(tracts.df, tracts.shp@data, by.x = 'id', by.y = 'CT_ID_10', all.x = TRUE)

my.map.theme <- theme(text = element_text(family = "Futura Bk BT"),
                      plot.title = element_text(size = 30),
                      axis.line = element_blank(),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank(),
                      #legend.title = element_text(size = 16, face = "bold"),
                      legend.title = element_text(size = 14),
                      legend.text = element_text(size = 12),
                      panel.grid.major = element_blank (), # remove major grid
                      panel.grid.minor = element_blank (),  # remove minor grid
                      axis.text = element_blank (), 
                      axis.title = element_blank (),
                      axis.ticks = element_blank (),
                      strip.text = element_text(size = 20))


# Get Neighborhood names and locations

NBs.df <- fortify(tracts.shp, region = "BRA_PD.x")
NBcentroids <-aggregate(cbind(long, lat) ~ id, data = NBs.df, FUN = function(x) mean(range(x)))
#Shorter names please!
NBcentroids$id <- sub("Fenway/Kenmore", "Fenway", NBcentroids$id)
NBcentroids$id <- sub("Back Bay/Beacon Hill", "Back Bay", NBcentroids$id)

# For prettier color scales
library(scales)


# Public Services

ggplot(data=tracts.df, aes(x=long, y=lat, group=group, fill = Elderly_Publicservice_Subindex )) +
  geom_polygon() +
  scale_fill_gradient2(high = muted("darkgoldenrod1")) +
  geom_label(data=NBcentroids, 
            aes(long, lat, label = id, group = NULL, fill=NULL), size=4) +
  labs(title = "Public Services Subindex", fill = "Value") +
  theme_minimal() +  my.map.theme + coord_map()


# Open Space

ggplot(data=tracts.df, aes(x=long, y=lat, group=group, fill = Elderly_Openspace_Subindex )) +
  geom_polygon() +
  scale_fill_gradient2(high = muted("darkgreen")) +
  geom_label(data=NBcentroids, 
             aes(long, lat, label = id, group = NULL, fill=NULL), size=4) +
  labs(title = "Open Space Subindex", fill = "Value") +
  theme_minimal() +  my.map.theme + coord_map()


# Health Care

ggplot(data=tracts.df, aes(x=long, y=lat, group=group, fill = Elderly_Healthcare_Subindex )) +
  geom_polygon() +
  scale_fill_gradient2(high = muted("darkmagenta")) +
  geom_label(data=NBcentroids, 
             aes(long, lat, label = id, group = NULL, fill=NULL), size=4) +
  labs(title = "Healthcare Subindex", fill = "Value") +
  theme_minimal() +  my.map.theme + coord_map()


# Cultural Amenities

ggplot(data=tracts.df, aes(x=long, y=lat, group=group, fill = Elderly_Culture_Subindex )) +
  geom_polygon() +
  scale_fill_gradient2(high = muted("darkred")) +
  geom_label(data=NBcentroids, 
             aes(long, lat, label = id, group = NULL, fill=NULL), size=4) +
  labs(title = "Cultural Amenities Subindex", fill = "Value") +
  theme_minimal() +  my.map.theme + coord_map()


# Community Facilities 

ggplot(data=tracts.df, aes(x=long, y=lat, group=group, fill = Elderly_Community_Subindex )) +
  geom_polygon() +
  scale_fill_gradient2(high = muted("darkblue")) +
  geom_label(data=NBcentroids, 
             aes(long, lat, label = id, group = NULL, fill=NULL), size=4) +
  labs(title = "Community Facilities Subindex", fill = "Value") +
  theme_minimal() +  my.map.theme + coord_map()


# Residential Amenities

ggplot(data=tracts.df, aes(x=long, y=lat, group=group, fill = Elderly_Residential_Subindex )) +
  geom_polygon() +
  scale_fill_gradient2(high = muted("cornsilk4")) +
  geom_label(data=NBcentroids, 
             aes(long, lat, label = id, group = NULL, fill=NULL), size=4) +
  labs(title = "Residential Amenities Subindex", fill = "Value") +
  theme_minimal() +  my.map.theme + coord_map()
