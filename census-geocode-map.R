# efg, 24 Oct 2014

setwd("C:/2014/R/Census-Tract-from-Address/")

library(stringr)   # str_trim
library(RJSONIO)   # fromJSON

fix <- function(s)
{
  gsub(" ", "+", str_trim(s))
}

# http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
simpleCap <- function(x)
{
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}

GEOCODE.PREFIX <- "http://geocoding.geo.census.gov/geocoder/geographies/address?street="
GEOCODE.SUFFIX <- "&benchmark=Public_AR_Census2010&vintage=Census2010_Census2010&layers=14&format=json"

##############################################################################

street <- "2411 Holmes St"
city   <- "Kansas City"
state  <- "MO"

URL <- paste0(GEOCODE.PREFIX, fix(street), "&city=", fix(city), "&state=", fix(state), GEOCODE.SUFFIX)

JSON <- fromJSON(URL)

length(JSON$result$addressMatches)
stopifnot(length(JSON$result$addressMatches) > 0)

target.matched <- JSON$result$addressMatches[[1]]$matchedAddress
# [1] "2411 Holmes St, KANSAS CITY, MO, 64108"

target.stateabbr <- JSON$result$addressMatches[[1]]$addressComponents["state"]
#state
# "MO"

target.zip <- JSON$result$addressMatches[[1]]$addressComponents["zip"]
#    zip
#"64108"

target.coordinates <- JSON$result$addressMatches[[1]]$coordinates
#        x         y
#-94.57584  39.08360

length(JSON$result$addressMatches[[1]]$geographies$`Census Blocks`)
stopifnot(length(JSON$result$addressMatches[[1]]$geographies$`Census Blocks`) > 0)
fips.state   <- JSON$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$STATE   # "29"
fips.county  <- JSON$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$COUNTY  # "095"
target.tract <- JSON$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$TRACT   # "004300"
target.block <- JSON$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$BLOCK   # "1004"
target.geoid <- JSON$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$GEOID   # "290950043001004"

##############################################################################

library(maps)
data(state.fips)
data(county.fips)

fips.state.name <- as.character(state.fips$polyname[ which(as.numeric(fips.state) == state.fips$fips)[1] ])
fips.county.name <- as.character(county.fips$polyname[ county.fips$fips == as.integer(substr(target.geoid,1,5)) ])

# Some states come in parts.  Here only state name wanted.
fips.state.name <- unlist(strsplit(fips.state.name, ":"))[1]

# State
county.info <- map("county", fips.state.name, plot=FALSE)
map("county", fips.state.name,
   fill=TRUE,
   col=ifelse(county.info$names == fips.county.name, "blue", "transparent"))

map("state", fips.state.name, lwd=2, add=TRUE)
title(simpleCap(fips.state.name), cex.main=1.5)

SubdivisionName <- "County"
if (fips.state.name == "alaska") SubdivsionName <- "Borough"
if (fips.state.name == "louisiana") SubdivsionName <- "Parish"
county.name <- unlist(strsplit(fips.county.name, ","))[2]          # "jackson"
county.name.full <- paste(simpleCap(county.name),SubdivisionName)  # "Jackson County"
mtext(county.name.full, col="blue")

points(target.coordinates["x"], target.coordinates["y"], col="green", pch=8)


# Census Tract
library(UScensus2010)
library(UScensus2010tract)
county.coordinates <- county(name=county.name, state=target.stateabbr, level="tract")
plot(county.coordinates, main=county.name.full)
title(paste0(county.name.full, ", ", target.stateabbr), col.main="blue", cex.main=1.5)

# load creates R object, e.g., missouri.tract10, from data read from file.
load(paste0(find.package("UScensus2010tract"), "/data/", fips.state.name, ".tract10.rda"), verbose=TRUE)

# Assign object (e.g., missouri.tract10) to standard name (state.tract10) for
# manipulation and delete original name.  Because of R's "lazy loading" this
# is effectively a rename and does not cause a duplicate copy in memory.
R.statement <- paste0("state.tract10 <- ", fips.state.name, ".tract10;\n",
                      "rm(", fips.state.name, ".tract10)")
connection <- textConnection(R.statement)
R.parse <- parse(connection)
close(connection)
eval(R.parse)

plot(state.tract10,
     col=ifelse(state.tract10@data$tract == target.tract, "red", "transparent"),
     border="transparent",
     add=TRUE)
rm(state.tract10)

tract.parts <- paste(substr(target.geoid,1,2), substr(target.geoid,3,5), substr(target.geoid,6,11))
mtext(paste("Census Tract",tract.parts), col="red")

points(target.coordinates["x"], target.coordinates["y"], col="green", pch=8)

