# List of required packages
list.of.packages <- c(
	"foreach",
	"future",
	"future.apply",
	"future.batchtools",
	"doParallel",
	"rslurm",
	"here",
	"adehabitatLT",
	"adehabitatHR",
	"rgdal",
	"terra",
	"sf"
)

# Check if installed, else install
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages) > 0){
  install.packages(new.packages, dep=TRUE)
}

# Load required packages
for(package.i in list.of.packages){
  suppressPackageStartupMessages(
    library(
      package.i, 
      character.only = TRUE
      )
    )
}

# Load Data
###############Toy Data#################
data(puechcirc)
pc1 <- puechcirc[1]
pc2 <- puechcirc[2]
pc3 <- puechcirc[3]
Traj_li <- vector("list", 3)
Traj_li[[1]] <- pc1
Traj_li[[2]] <- pc2
Traj_li[[3]] <- pc3
DLik <- c(2.1, 2.2, 4)
thenames <- c("pn1", "pn2", "pn3")

##############Actual Data###############
Caribou_BMTR <- read.csv("rad/Caribou_noclip.csv")

# Data Frame Parameters
#############################################################################
x = "E" # Easting
y = "N" # Northing
## See depreciated: https://inbo.github.io/tutorials/tutorials/spatial_crs_coding/
crs.proj <- CRS(SRS_string = "EPSG:26910") # For raster formatting
input.projection = "+init=epsg:26910" # For SpatialDataFrame formatting
Datecolumn= "Date"
Timecolumn= "Time"
timezone = "GMT"
loc.output <- paste0("adeHRoutput/")
#############################################################################

# Format Data
print(Caribou_BMTR[1])