# R script for visualizing typology data on world maps using Voronoi tessellation
# Curdin Derungs <curdin.derungs@uzh.ch>
# Steven Moran <steven.moran@uzh.ch>
# To run this script you will need libraries outside of R: gdal, v8


###read input data as downloaded from wals.info----

##information for all languages, including coordinates
#from: wals.info/download
lang<-read.csv("input/languages.csv")

#no duplicates
lang<-lang[!duplicated(lang[,c("longitude","latitude")]),]

#limit spatial extents for later visualisation
lang<-lang[lang$latitude<86 & lang$latitude>-86,]
lang<-lang[lang$longitude<175 & lang$longitude>-175,]

##information for one linguistic feature
#from: http://wals.info/feature -> select one -> download .xls below map -> store as .csv
feat<-read.csv("input/values.csv")

#create common id
feat$wals_code<-sapply(strsplit(as.character(feat$ID),split = "-"),function(x)x[2])

#merge information on laguages and features
feat<-merge(feat,lang[,c("wals_code","latitude","longitude")],by="wals_code",all=T)

#only complete cases
feat<-feat[complete.cases(feat[,c("longitude","latitude")]),]

#defining a label for languages that were not sampled
feat$Name<-as.character(feat$Name)
feat$Name[is.na(feat$Name)]<-"99 not sampled"



###creating spatial data from the coordinates----
library(sp)
coordinates(feat)<-~longitude+latitude

#defining the projection of the input data, which is WGS84
proj4string(feat) <- CRS("+init=epsg:4326")


##reprojecting the data into mercator
#coordinates are metric
# SM: On OSX gdal needs to be installed for rgdal to work in R: brew install rgdal
library(rgdal)
feat <- spTransform(feat, CRS("+init=ESRI:54012"))


###buffer features (200km)----
library(raster)
feat.buf<-buffer(feat,width=200000)
proj4string(feat.buf) <- CRS("+init=ESRI:54012")


###voronoy tesselation----
#code from: http://carsonfarmer.com/2009/09/voronoi-polygons-with-r/
library(deldir)
voronoipolygons = function(layer) {
  require(deldir)
  crds = layer@coords
  z = deldir(crds[,1], crds[,2])
  w = tile.list(z)
  polys = vector(mode='list', length=length(w))
  require(sp)
  for (i in seq(along=polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP = SpatialPolygons(polys)
  voronoi = SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1], 
                                                         y=crds[,2], 
                                                         ID=sapply(slot(SP, 'polygons'),function(x) slot(x, 'ID'))
                                                         ),
                                     match.ID = "ID"
                                     )
}

feat.vor<-voronoipolygons(feat)
proj4string(feat.vor) <- CRS("+init=ESRI:54012")


###merge and postprocess data----
##clip buffer and voronoi
library(rgeos)
feat.clip <- gIntersection(feat.buf,feat.vor, byid = TRUE, drop_lower_td = TRUE)


##intersect clipped voronoi geometry with feature information
#generate the ID field
ids<-data.frame(ID=sapply(slot(feat.clip, 'polygons'),function(x) slot(x, 'ID')))
ids$ID<-as.character(ids$ID)

#add language info to all geometries
feat.pol<-SpatialPolygonsDataFrame(feat.clip,data = cbind(ids,feat@data),match.ID = "ID")

feat.pol$Name<-factor(feat.pol$Name)

feat.pol.pj <- spTransform(feat.pol,CRS("+init=epsg:4326"))



###visualisation in geo maps----
library(ggplot2)
# SM: requires R library maptools
feat.pol.gg<-fortify(feat.pol,region="Name")

#reproject coordinates for visualisation in ggplot
x <- feat.pol.gg$long
y <- feat.pol.gg$lat

d <- data.frame(lon=x, lat=y)
coordinates(d) <- c("lon", "lat")
proj4string(d) <- CRS("+init=ESRI:54012")
d.trans <- spTransform(d, CRS("+init=epsg:4326"))

feat.pol.gg$long<-d.trans@coords[,1]
feat.pol.gg$lat<-d.trans@coords[,2]

library(randomcoloR)
# SM: requires that v8 is installed on your OS, e.g. brew install v8-315
# install.packages('V8')
mapWorld <- borders("world", colour="gray50", fill="gray80")
mp <- ggplot()+ 
  mapWorld+ 
  geom_polygon(data = feat.pol.gg, aes(x=long, y=lat, group=group, fill=id))+ 
  scale_fill_manual(values=c("grey60",randomColor(count = length(unique(feat.pol.gg$id))-1)))+
  theme_minimal()
mp


