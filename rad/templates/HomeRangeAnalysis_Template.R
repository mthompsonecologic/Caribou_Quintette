wd <- getwd()
newwd <- paste("~/Github/Caribou_Quintette/rad", "data", "**SA**/**SEASON**/Job_**JOBID**/", sep = "/")
setwd(newwd)
if (dir.exists("BRB_UDs") == FALSE){
	dir.create("BRB_UDs")
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
Caribou_**SA**_**SEASON**<- read.delim("Job_**JOBID**.txt", sep = "\t")

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
# Caribou**SA**.ltraj <- as.ltraj(xy = Caribou_**SA**[,c("E", "N")], 
#                             date =  as.POSIXct(Caribou_**SA**$timestamp,
#                                                format = "%Y-%m-%d %H:%M:%S", tz = "GMT"),
#                             id = Caribou_**SA**$ANIMALID, typeII = TRUE)
# total.path.df <- data.frame(Caribou**SA**.ltraj[[1]],
#                             id = attr(Caribou**SA**.ltraj[[1]],
#                                       "id"))
# 
# # Fill Data frame with trajectories
# for(i in 2:length(Caribou**SA**.ltraj)) {
#   total.path.df <- rbind(total.path.df,
#                          data.frame(Caribou**SA**.ltraj[[i]],
#                                     id = attr(Caribou**SA**.ltraj[[i]],
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
years <- unique(Caribou_**SA**_**SEASON**$YearSeas)

Caribou_**SA**_**SEASON**_LI <- list()
tally = 1
x = 1
IndID <- data.frame(matrix(ncol=1))
colnames(IndID) <- "ID"
for(y in 1:length(years)){
  if(nrow(data.frame(Caribou_**SA**_**SEASON**[which(Caribou_**SA**_**SEASON**$YearSeas == years[y]), ])) > 0){
    assign("temp",data.frame(Caribou_**SA**_**SEASON**[which(Caribou_**SA**_**SEASON**$YearSeas == years[y]), ]))
    Ind <- unique(temp$AnimalID)
    for(i in 1:length(Ind)){
      if(nrow(temp[which(temp$AnimalID == Ind[i]),]) < 5){  ## Threshold on #track points
        IndID[x,] <- Ind[i]
        x = x + 1
      }else{next()}}
    Caribou_**SA**_**SEASON**_LI[[tally]] <- temp[!temp$AnimalID %in% IndID$ID,]
    names(Caribou_**SA**_**SEASON**_LI)[tally] <- paste0("Caribou_**SA**_**SEASON**_",years[y])
    tally = tally + 1
  }else{next()}
}
Caribou_**SA**_**SEASON**_LI[sapply(Caribou_**SA**_**SEASON**_LI,function(x) all(is.na(x)))] <- NULL

##Winter
**SA**_**SEASON**_Traj <- list()
for(l in 1:length(Caribou_**SA**_**SEASON**_LI)){
  **SA**_**SEASON**_Traj[[l]] <-  as.ltraj(xy = Caribou_**SA**_**SEASON**_LI[[l]][,c("E", "N")], 
                               date =  as.POSIXct(Caribou_**SA**_**SEASON**_LI[[l]]$timestamp,
                                                  format = "%Y-%m-%d %H:%M:%S", tz = "GMT"),
                               id = Caribou_**SA**_**SEASON**_LI[[l]]$AnimalID,
                               typeII = TRUE)
  names(**SA**_**SEASON**_Traj)[l] <- paste0("**SA**_**SEASON**_",unique(Caribou_**SA**_**SEASON**_LI[[l]]$YearSeas))
}
print("#############################################################################")



print("#################################Trajectories#############################################")
**SA**_**SEASON**_Traj <- list()
for(l in 1:length(Caribou_**SA**_**SEASON**_LI)){
  **SA**_**SEASON**_Traj[[l]] <-  as.ltraj(xy = Caribou_**SA**_**SEASON**_LI[[l]][,c("E", "N")], 
                               date =  as.POSIXct(Caribou_**SA**_**SEASON**_LI[[l]]$timestamp,
                                                  format = "%Y-%m-%d %H:%M:%S", tz = "GMT"),
                               id = Caribou_**SA**_**SEASON**_LI[[l]]$AnimalID,
                               typeII = TRUE)
  names(**SA**_**SEASON**_Traj)[l] <- paste0("**SA**_**SEASON**_",unique(Caribou_**SA**_**SEASON**_LI[[l]]$YearSeas))
}
print("#############################################################################")



print("#############################Diffusion Parameter################################################")
DLik_**SA**_**SEASON** <- list()
tally = 1
**SA**_**SEASON**_Traj <- **SA**_**SEASON**_Traj[order(names(**SA**_**SEASON**_Traj))]

for(t in 1:length(**SA**_**SEASON**_Traj)){
  DLik_**SA**_**SEASON**[[tally]] <- BRB.likD(**SA**_**SEASON**_Traj[[t]], Tmax=1500*60, Lmin=2,  Dr=c(2,7))
  tally = tally + 1
}

names(DLik_**SA**_**SEASON**) <- names(**SA**_**SEASON**_Traj)


DLik_**SA**_**SEASON**_u <- unlist(DLik_**SA**_**SEASON**)
DLik_**SA**_**SEASON**_u <- DLik_**SA**_**SEASON**_u[1:(length(DLik_**SA**_**SEASON**_u)/2)*2]
DLik_**SA**_**SEASON**_u <- as.numeric(as.vector(DLik_**SA**_**SEASON**_u))

Traj_len <- as.numeric(as.vector(lengths(**SA**_**SEASON**_Traj)))


Traj_li <- vector("list", length(DLik_**SA**_**SEASON**_u))
a = 1
b = 1

for(y in 1:length(Traj_len)){
  b = 1
  while(b <= Traj_len[y]){
    Traj_li[[a]] <- **SA**_**SEASON**_Traj[[y]][b]
    names(Traj_li[[a]])  <- names(**SA**_**SEASON**_Traj[y])
    b = b + 1
    a = a + 1
  }
}

## The BRBs_BM_WI need to be named. The names are contained here:
thenames <- unlist(lapply(Traj_li, function (x) paste0(names(x),"_",id(x))))

print("#############################################################################")

print("##################################Home Range Analysis############################################\n\n")
n.cores <- as.vector(future::availableCores())*.9
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
print("##################################BRB Analysis############################################")
system.time({
  BRBs_**SA**_**SEASON** <- foreach(i = 1:length(Traj_li),
                        .combine = c,
                        .packages = c("adehabitatHR","adehabitatLT")) %dopar% {
                          saveRDS(BRB(Traj_li[[i]][1],
                                      D = DLik_**SA**_**SEASON**_u[i],
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

BRBs_**SA**_**SEASON**f <- list.files(here("BRB_UDs"),
                          pattern="\\.Rds$",
                          full.names=TRUE)

BRBs_**SA**_**SEASON** <- lapply(BRBs_**SA**_**SEASON**f, function(x){readRDS(x)})
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
# plan(multicore)
# BRBs_**SA**_**SEASON** <- lapply(BRBs_**SA**_**SEASON**f, function(x){readRDS(x)})
# system.time(BRBs_**SA**_**SEASON**v <- future_lapply(BRBs_**SA**_**SEASON**,
#                                          FUN = function(x) {
#                                            getverticeshr.estUD(x, percent=50)
#                                          }))

### parallel approach
n.cores <- as.vector(future::availableCores())*.9
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

system.time({
  homerange <- foreach(i = 1:length(BRBs_**SA**_**SEASON**),
                       .combine = c,
                       .packages = c("adehabitatHR","here")) %dopar% {
                         saveRDS(getverticeshr.estUD(BRBs_**SA**_**SEASON**[[i]], percent=50),
                                 paste0(here("BRB_UDs","**SA**","**SEASON**"),"/",thenames[i],"_hr.Rds"))
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
vud <- foreach(i = 1:length(BRBs_**SA**_**SEASON**),
               .combine = c,
               .packages = c("adehabitatHR","here","terra")) %dopar% {
                 vect(getvolumeUD(BRBs_**SA**_**SEASON**[[i]]),
                      paste0(here("BRB_UDs","**SA**","**SEASON**"),"/",
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
                                         year = substr(names(**SA**_**SEASON**_Traj[i]),7,11),
                                         area = homerangedf[,2],
                                         nb.reloc = nrow(**SA**_**SEASON**_Traj[[i]][[1]])))
})
print("#############################################################################")
# Save the output.
write.csv(BRB_area, paste0(here("BRB_UDs"), "/**SA**_**SEASON**_BRB_areas.csv", sep=""), row.names = FALSE)
