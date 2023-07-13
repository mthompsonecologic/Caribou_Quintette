gc()
gcinfo(TRUE)
wd <- getwd()
newwd <- paste("~/Github/Caribou_Quintette/rad", "data", "BM/CA/Job_0/", sep = "/")
setwd(newwd)
# Create Directories for Outputs
# BRB Files
if (dir.exists("BRB_UDs") == FALSE){
	dir.create("BRB_UDs")
}
# GetVertices Files
if (dir.exists("BRB_hrs") == FALSE){
	dir.create("BRB_hrs")
}
# GetVolume Files
if (dir.exists("BRB_vUDs") == FALSE){
	dir.create("BRB_vUDs")
}
# dir.create(file.path(newwd, "BRB_UDs"), showWarnings = FALSE)
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
options(future.globals.maxSize= 11912896000)

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
##############Actual Data###############
Caribou_BM_CA<- read.delim("Job_0.txt", sep = "\t")

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

print("###########################Data Preparation Step##################################################")
years <- unique(Caribou_BM_CA$YearSeas)

Caribou_BM_CA_LI <- list()
tally = 1
x = 1
IndID <- data.frame(matrix(ncol=1))
colnames(IndID) <- "ID"
for(y in 1:length(years)){
  if(nrow(data.frame(Caribou_BM_CA[which(Caribou_BM_CA$YearSeas == years[y]), ])) > 0){
    assign("temp",data.frame(Caribou_BM_CA[which(Caribou_BM_CA$YearSeas == years[y]), ]))
    Ind <- unique(temp$AnimalID)
    for(i in 1:length(Ind)){
      if(nrow(temp[which(temp$AnimalID == Ind[i]),]) < 5){  ## Threshold on #track points
        IndID[x,] <- Ind[i]
        x = x + 1
      }else{next()}}
    Caribou_BM_CA_LI[[tally]] <- temp[!temp$AnimalID %in% IndID$ID,]
    names(Caribou_BM_CA_LI)[tally] <- paste0("Caribou_BM_CA_",years[y])
    tally = tally + 1
  }else{next()}
}
Caribou_BM_CA_LI[sapply(Caribou_BM_CA_LI,function(x) all(is.na(x)))] <- NULL


print("#################################Trajectories#############################################")
BM_CA_Traj <- list()
for(l in 1:length(Caribou_BM_CA_LI)){
  BM_CA_Traj[[l]] <-  as.ltraj(xy = Caribou_BM_CA_LI[[l]][,c("E", "N")], 
                               date =  as.POSIXct(Caribou_BM_CA_LI[[l]]$timestamp,
                                                  format = "%Y-%m-%d %H:%M:%S", tz = "GMT"),
                               id = Caribou_BM_CA_LI[[l]]$AnimalID,
                               typeII = TRUE)
  names(BM_CA_Traj)[l] <- paste0("BM_CA_",unique(Caribou_BM_CA_LI[[l]]$YearSeas))
}
print("#############################################################################")



print("#############################Diffusion Parameter################################################")
DLik_BM_CA <- list()
tally = 1
BM_CA_Traj <- BM_CA_Traj[order(names(BM_CA_Traj))]

for(t in 1:length(BM_CA_Traj)){
  DLik_BM_CA[[tally]] <- BRB.likD(BM_CA_Traj[[t]], Tmax=1500*60, Lmin=2,  Dr=c(2,7))
  tally = tally + 1
}

names(DLik_BM_CA) <- names(BM_CA_Traj)


DLik_BM_CA_u <- unlist(DLik_BM_CA)
DLik_BM_CA_u <- DLik_BM_CA_u[1:(length(DLik_BM_CA_u)/2)*2]
DLik_BM_CA_u <- as.numeric(as.vector(DLik_BM_CA_u))

Traj_len <- as.numeric(as.vector(lengths(BM_CA_Traj)))


Traj_li <- vector("list", length(DLik_BM_CA_u))
a = 1
b = 1

for(y in 1:length(Traj_len)){
  b = 1
  while(b <= Traj_len[y]){
    Traj_li[[a]] <- BM_CA_Traj[[y]][b]
    names(Traj_li[[a]])  <- names(BM_CA_Traj[y])
    b = b + 1
    a = a + 1
  }
}

## The BRBs_BM_WI need to be named. The names are contained here:
thenames <- unlist(lapply(Traj_li, function (x) paste0(names(x),"_",id(x))))

print("#############################################################################")

print("##################################Home Range Analysis############################################")
n.cores <- as.vector(future::availableCores())-5
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
print("##################################BRB Analysis############################################")
system.time({
  BRBs_BM_CA <- foreach(i = 1:length(Traj_li),
                        .combine = c,
                        .packages = c("adehabitatHR","adehabitatLT")) %dopar% {
							BRB(Traj_li[[i]][1],
                                      D = DLik_BM_CA_u[i],
                                      Tmax = 1500*60,
                                      Lmin = 2,
                                      hmin = 20,
                                      type = "UD",
                                      grid = 4000)
                        }})
stopCluster(my.cluster)

print("##################################Saving BRB Analysis############################################")
savingBRB <- function(dat, name){
	print(name)
	writeVector(vect(dat), paste("BRB_UDs/", name, ".shp", sep = ""))
}
system.time(
	mapply(savingBRB, BRBs_BM_CA, thenames)
)
print("#############################################################################")
# Now we calculate the BRB metrics
######################
# BRB metric: Vertices
# This is the extraction of the home-range contours
# I want the 50% home range contour, which is
# considered the core home range. If time, we can also
# set percent=95, as this is usually considered to be
# the accepted home range. In my original run, I realize
# now that I was doing it for every percent as I did
# not specify the percent = x, yikes! However,
# even with one contour estimate it takes a lot of memory.
######################

## I believe you can use either the future or
## the parallel approach, both worked. The future
## approach ran faster.

# future approach
print("##################################BRB Vertices############################################")
GettingVertices <- function(data, name){
	Vertices <- getverticeshr.estUD(data, percent=50)
	writeVector(vect(Vertices), paste("BRB_hrs/", name, "_hr.shp", sep = ""))
}

system.time(
	homerange <- mapply(GettingVertices, BRBs_BM_CA, thenames)
	)


print("#############################################################################")
######################
# BRB metric: getvolumeUD
# Description from adehabitat package:
# The function getvolumeUD modifies the UD component
# of the object passed as argument: that the pixel
# values of the resulting object are equal to the
# percentage of the smallest homerange containing
# this pixel. This function is used in the function
# kernel.area, to compute the home-range size.
# Note, that the function plot.hrsize (see the help
# page of this function) can be used to display the
# home-range size estimated at various levels.
######################

#create the cluster
print("##################################BRB Volume############################################")
GettingVolume <- function(data, name){
	Volume <- getvolumeUD(data)
	writeVector(vect(Volume), paste("BRB_vUDs/", name, "_vUD.shp", sep = ""))
	# vect(Volume, paste("BRB_vUDs/", name, "vUD.shp", sep = ""))
}
system.time(
	vud <- (mapply(GettingVolume, BRBs_BM_CA, thenames))
	)

# This part I had working for my big for loop.
# It needs to be fixed for the individual animals.
# This is creating a dataframe that pulls out the
# area calculations that are contained in the
# getvolumeUD object. The list element and item
# calls will have to be adjusted and checked.

# BRB_area <- data.frame(matrix(ncol=4))
# colnames(BRB_area) <- c("id", "year", "area", "nb.reloc")

# system.time(for(i in 1:length(thenames)){
#   homerangedf <- as.data.frame(vud[i])
#   BRB_area[i,c(1:4)] <- rbind(data.frame(id = thenames[[i]],
#                                          year = substr(names(BM_CA_Traj[i]),7,11),
#                                          area = homerangedf[,2],
#                                          nb.reloc = nrow(BM_CA_Traj[[i]][[1]])))
# })
# print("#############################################################################")
# # Save the output.
# write.csv(BRB_area, paste("BRB_UDs/", "/BM_CA_BRB_areas.csv", sep=""), row.names = FALSE)
