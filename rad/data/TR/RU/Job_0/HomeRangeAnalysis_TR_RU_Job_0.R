wd <- getwd()
newwd <- paste("~/Github/Caribou_Quintette/rad", "data", "TR/RU/Job_0/", sep = "/")
setwd(newwd)
dir.create(file.path(newwd, "BRB_UDs"), showWarnings = FALSE)
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
options(future.globals.maxSize= 2912896000)

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
Caribou_TR_RU<- read.delim("Job_0.txt", sep = "\t")

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


##############SKIPPED BECAUSE NO LONGER APPLICABLE AT SUBSET LEVEL########################
# # Trajectories
# CaribouTR.ltraj <- as.ltraj(xy = Caribou_TR[,c("E", "N")], 
#                             date =  as.POSIXct(Caribou_TR$timestamp,
#                                                format = "%Y-%m-%d %H:%M:%S", tz = "GMT"),
#                             id = Caribou_TR$ANIMALID, typeII = TRUE)
# total.path.df <- data.frame(CaribouTR.ltraj[[1]],
#                             id = attr(CaribouTR.ltraj[[1]],
#                                       "id"))
# 
# # Fill Data frame with trajectories
# for(i in 2:length(CaribouTR.ltraj)) {
#   total.path.df <- rbind(total.path.df,
#                          data.frame(CaribouTR.ltraj[[i]],
#                                     id = attr(CaribouTR.ltraj[[i]],
#                                               "id")))
# }
#############################################################################

# Calculate distance traveled per day and add it to the dataframe
# total.path.df$distperday <- total.path.df$dist / (total.path.df$dt/60/60/24)
# # Aggregate to show mean distance per day for each animal
# path.summary <- aggregate(distperday~id, data = total.path.df, FUN = mean)
# path.summary$sd <- aggregate(distperday~id, data = total.path.df, FUN = sd)$distperday
# # Look at summary dataframe
# path.summary


######SKIPPED - WINTER ORGANIZED AS SINGLE DURING DATA SPLIT##############


print("###########################Data Preparation Step##################################################")
years <- unique(Caribou_TR_RU$YearSeas)

Caribou_TR_RU_LI <- list()
tally = 1
x = 1
IndID <- data.frame(matrix(ncol=1))
colnames(IndID) <- "ID"
for(y in 1:length(years)){
  if(nrow(data.frame(Caribou_TR_RU[which(Caribou_TR_RU$YearSeas == years[y]), ])) > 0){
    assign("temp",data.frame(Caribou_TR_RU[which(Caribou_TR_RU$YearSeas == years[y]), ]))
    Ind <- unique(temp$AnimalID)
    for(i in 1:length(Ind)){
      if(nrow(temp[which(temp$AnimalID == Ind[i]),]) < 5){  ## Threshold on #track points
        IndID[x,] <- Ind[i]
        x = x + 1
      }else{next()}}
    Caribou_TR_RU_LI[[tally]] <- temp[!temp$AnimalID %in% IndID$ID,]
    names(Caribou_TR_RU_LI)[tally] <- paste0("Caribou_TR_RU_",years[y])
    tally = tally + 1
  }else{next()}
}
Caribou_TR_RU_LI[sapply(Caribou_TR_RU_LI,function(x) all(is.na(x)))] <- NULL

##Winter
TR_RU_Traj <- list()
for(l in 1:length(Caribou_TR_RU_LI)){
  TR_RU_Traj[[l]] <-  as.ltraj(xy = Caribou_TR_RU_LI[[l]][,c("E", "N")], 
                               date =  as.POSIXct(Caribou_TR_RU_LI[[l]]$timestamp,
                                                  format = "%Y-%m-%d %H:%M:%S", tz = "GMT"),
                               id = Caribou_TR_RU_LI[[l]]$AnimalID,
                               typeII = TRUE)
  names(TR_RU_Traj)[l] <- paste0("TR_RU_",unique(Caribou_TR_RU_LI[[l]]$YearSeas))
}
print("#############################################################################")



print("#################################Trajectories#############################################")
TR_RU_Traj <- list()
for(l in 1:length(Caribou_TR_RU_LI)){
  TR_RU_Traj[[l]] <-  as.ltraj(xy = Caribou_TR_RU_LI[[l]][,c("E", "N")], 
                               date =  as.POSIXct(Caribou_TR_RU_LI[[l]]$timestamp,
                                                  format = "%Y-%m-%d %H:%M:%S", tz = "GMT"),
                               id = Caribou_TR_RU_LI[[l]]$AnimalID,
                               typeII = TRUE)
  names(TR_RU_Traj)[l] <- paste0("TR_RU_",unique(Caribou_TR_RU_LI[[l]]$YearSeas))
}
print("#############################################################################")



print("#############################Diffusion Parameter################################################")
DLik_TR_RU <- list()
tally = 1
TR_RU_Traj <- TR_RU_Traj[order(names(TR_RU_Traj))]

for(t in 1:length(TR_RU_Traj)){
  DLik_TR_RU[[tally]] <- BRB.likD(TR_RU_Traj[[t]], Tmax=1500*60, Lmin=2,  Dr=c(2,7))
  tally = tally + 1
}

names(DLik_TR_RU) <- names(TR_RU_Traj)


DLik_TR_RU_u <- unlist(DLik_TR_RU)
DLik_TR_RU_u <- DLik_TR_RU_u[1:(length(DLik_TR_RU_u)/2)*2]
DLik_TR_RU_u <- as.numeric(as.vector(DLik_TR_RU_u))

Traj_len <- as.numeric(as.vector(lengths(TR_RU_Traj)))


Traj_li <- vector("list", length(DLik_TR_RU_u))
a = 1
b = 1

for(y in 1:length(Traj_len)){
  b = 1
  while(b <= Traj_len[y]){
    Traj_li[[a]] <- TR_RU_Traj[[y]][b]
    names(Traj_li[[a]])  <- names(TR_RU_Traj[y])
    b = b + 1
    a = a + 1
  }
}

## The BRBs_BM_WI need to be named. The names are contained here:
thenames <- unlist(lapply(Traj_li, function (x) paste0(names(x),"_",id(x))))

print("#############################################################################")

print("##################################Home Range Analysis############################################\n\n")
n.cores <- as.vector(future::availableCores()) - 1
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
print("##################################BRB Analysis############################################")
system.time({
#   BRBs_TR_RU <- foreach(i = 1:length(Traj_li),
	BRBs_TR_RU <- foreach(i = 1:3,
                        .combine = c,
                        .packages = c("adehabitatHR","adehabitatLT")) %dopar% {
                          saveRDS(BRB(Traj_li[[i]][1],
                                      D = DLik_TR_RU_u[i],
                                      Tmax = 1500*60,
                                      Lmin = 2,
                                      hmin = 20,
                                      type = "UD",
                                      grid = 4000),paste0(here("BRB_UDs"),"/",thenames[i],".Rds"))
                        }})
stopCluster(my.cluster)
print("#############################################################################")
# Now we calculate the BRB metrics
# Read in the files

BRBs_TR_RUf <- list.files(here("BRB_UDs"),
                          pattern="\\.Rds$",
                          full.names=TRUE)

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
plan(multicore)
BRBs_TR_RU <- lapply(BRBs_TR_RUf, function(x){readRDS(x)})
system.time(BRBs_TR_RUv <- future_lapply(BRBs_TR_RU,
                                         FUN = function(x) {
                                           getverticeshr.estUD(x, percent=50)
                                         }))

### parallel approach
n.cores <- as.vector(future::availableCores())
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK" ## Attempting fork for nodes > 1
)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

BRBs_TR_RU <- lapply(BRBs_TR_RUf, function(x){readRDS(x)})

system.time({
  homerange <- foreach(i = 1:length(BRBs_TR_RU),
                       .combine = c,
                       .packages = c("adehabitatHR","here")) %dopar% {
                         saveRDS(getverticeshr.estUD(BRBs_TR_RU[[i]], percent=50),
                                 paste0(here("BRB_UDs","TR","RU"),"/",thenames[i],"_hr.Rds"))
                       }    
})
# Stop the parallel backend
stopCluster(my.cluster)
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
n.cores <- as.vector(future::availableCores())
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

## This currently saves as a spatVector:
vud <- foreach(i = 1:length(BRBs_TR_RU),
               .combine = c,
               .packages = c("adehabitatHR","here","terra")) %dopar% {
                 vect(getvolumeUD(BRBs_TR_RU[[i]]),
                      paste0(here("BRB_UDs","TR","RU"),"/",
                             thenames[i],"_vUD.shp"))    
               }

# Stop the parallel backend
stopCluster(my.cluster)

# This part I had working for my big for loop.
# It needs to be fixed for the individual animals.
# This is creating a dataframe that pulls out the
# area calculations that are contained in the
# getvolumeUD object. The list element and item
# calls will have to be adjusted and checked.

BRB_area <- data.frame(matrix(ncol=4))
colnames(BRB_area) <- c("id", "year", "area", "nb.reloc")

system.time(for(i in 1:length(thenames)){
  homerangedf <- as.data.frame(vud[i])
  BRB_area[i,c(1:4)] <- rbind(data.frame(id = name_burst[[i]],
                                         year = substr(names(TR_RU_Traj[i]),7,11),
                                         area = homerangedf[,2],
                                         nb.reloc = nrow(TR_RU_Traj[[i]][[1]])))
})
print("#############################################################################")
# Save the output.
write.csv(BRB_area, paste0(here("BRB_UDs"), "/TR_RU_BRB_areas.csv", sep=""), row.names = FALSE)
