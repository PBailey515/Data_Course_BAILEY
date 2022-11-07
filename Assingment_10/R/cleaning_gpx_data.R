library(tidyverse)
library(XML)
library(gpx)
library(RCurl)
library(methods)
# I am making a app to show my runs and to explore the data, before i can do that i need to parse it out. 
#aome of the stuff in the code doesnt work i am still reading online how to use all of this stuff, but i am getting the
#hang of it
# read in the file 
 pfile <- htmlTreeParse(file = "~/Desktop/Data_Course_Bailey/Assingment_10/Data/gpx_output/0a5f2849-4b4f-48d9-950b-c1be04a28c0c.gpx",useInternalNodes = T)


#getting the root node
rootnode <- xmlRoot(x= pfile)
#tells me the size i have 1 node
 xmlSize(rootnode)
 #shows me what the node is
rootnode[1]

attri <- rootnode[1]
#working on this part
#cat('number of nodes: ', nodes)
#print ('details of 2 record: ')
print (attri)
# this hasnt worked yet im not sure why quite yet but working on it
#dataframe<- xmlToDataFrame(pfile)

xmlToDataFrame(nodes = getNodeSet(attr, "<time>"))
# this extracts the elevations, times and coordinates
elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
# Extract latitude and longitude from the coordinates
lats <- as.numeric(coords["lat",])
lons <- as.numeric(coords["lon",])
# this doesnt work because they are different sizes so troubleshooting this 
geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times)
rm(list=c(“elevations”, “lats”, “lons”, “pfile”, “times”, “coords”))

head(geodf)


# thsi is a function i saw online im not quite sure what it does yet 
shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }

