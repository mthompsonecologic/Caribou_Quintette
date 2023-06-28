## Siku bash interactive job:
# One node:
#salloc --time=0:30:0 --nodes=1 --cpus-per-task=40 --mem=0
# Multi node:
#salloc --time=0:30:0 --nodes=3 --cpus-per-task=40 --mem=0

# Load required packages

#automatic install of packages if they are not installed already
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

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages) > 0){
  install.packages(new.packages, dep=TRUE)
}

#loading packages
for(package.i in list.of.packages){
  suppressPackageStartupMessages(
    library(
      package.i, 
      character.only = TRUE
      )
    )
}

# Toy data
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

# Create and save the estBRB objects in parallel:

n.cores <- future::availableCores()
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"  ##
  )

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

system.time({
BRBs_BM_WI <- foreach(i = 1:length(Traj_li),
                      .combine = c,
                      .packages = c("adehabitatHR","adehabitatLT", "here")) %dopar% {
                            saveRDS(BRB(Traj_li[[i]][1],
                            D = DLik[i],
                            Tmax = 1500*60,
                            Lmin = 2,
                            hmin = 20,
                            type = "UD",
                            grid = 4000),paste0(here("BRB_UDs"),"/",thenames[i],"toy.Rds"))
                        }})
stopCluster(my.cluster)

# list in the estBRB object files
BRBs_BM_WIf <- list.files(here("BRB_UDs"),
                 pattern="toy.Rds$",
                 full.names=TRUE)

#Area cluster
###########################################################
### future approach, only for node=1, works fastest 311 sec
plan(multicore)
system.time(BRBs_BM_WI <- lapply(BRBs_BM_WIf, function(x){readRDS(x)}))
system.time(BRBs_BM_WIv <- future_lapply(BRBs_BM_WI, FUN = function(x) {getverticeshr.estUD(x)}))

system.time(for(i in 1:length(thenames)){
	vect(BRBs_BM_WIv[i],paste0(here("BRB_UDs"),"/",thenames[i],"toy_hr.Rds"))})

###########################################################
## parallel approach,370.487 Sec
n.cores <- as.vector(future::availableCores())
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK" ## PSOCK for one node.
  )

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

BRBs_BM_WI <- lapply(BRBs_BM_WIf, function(x){readRDS(x)})

system.time({
homerange <- foreach(i = 1:length(BRBs_BM_WI),
                      .combine = c,
                      .packages = c("adehabitatHR","here")) %dopar% {
                      saveRDS(getverticeshr.estUD(BRBs_BM_WI[[i]]),
                              paste0(here("BRB_UDs"),"/",thenames[i],"toy_hr.Rds"))
                        }    
	})
# Stop the parallel backend
stopCluster(my.cluster)

###########################################################
#SlurmR testing these approaches

getvertf <- function(x){
  getverticeshr.estUD(x)
}

n.cores <- as.vector(future::availableCores())

system.time(
BRBs_BM_WIv <- slurm_map(BRBs_BM_WI,
                  getvertf,
                  nodes = 2, cpus_per_task = n.cores)
)

##
# I note with future, you can plan. This requires from bash
# look at the nodes and their names:
# >sq
# >scontrol show nodes cl['name of the first node'-'name of the last node']
# e.g., for 3 Siku nodes: cl[064-066]
# This does not work, but it should:
plan(cluster, workers = c("cl064", "cl065", "cl066")) # get the names of the nodes from bash in interactive assign.

