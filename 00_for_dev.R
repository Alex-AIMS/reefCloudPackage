## Actions
# Put Dockerfile in the reefcloud package (Julie) - add main script.R
# Write github action to run the docker (Murray)

## Run the following
## docker container kill reef_cloud:latest
##1. Run the docker container
### on server
#### docker run --entrypoint R -it --rm -v /home/ssm-user/dev:/home/Project -v /data:/data reefcloud:latest
### on local machine
#### docker run --entrypoint R -it --rm -v ~/Work/AIMS/Projects/reefCloud/2024/dev:/home/Project -v ~/Work/AIMS/Projects/reefCloud/2024/data:/data reef_cloud:latest
##2. direct editor to run R within the container
## M-x ess-remote
##3. set wd to /home/Project/R

#setwd("/home/Project/R")
#install.packages("validate")

remove.packages(c("reefCloudPackage", "status"))

remotes::install_github('ReefCloud/reefCloudPackage', force = TRUE, dependencies = FALSE)
remotes::install_github('open-AIMS/status', force = TRUE)
library(status) # to go in the Dockerfile
library(reefCloudPackage)
library(validate) # to go in the Dockerfile

## testing startMatter ####
## args = c("--bucket=/data/AUS/",       #path to bucket
##          "--domain=tier",             #tier or site
##           "--by_tier=5",              #tier level
##          "--model_type=5",            #model used for predictions
##          "--debug=false",             #debug mode
##           "--runStage=1",             #current running stage
##          "--refresh_data=true"        #reload data
## )

args = c("--bucket=/data/AUS/",       #path to bucket
         "--domain=tier",             #tier or site
          "--by_tier=5",              #tier level
         "--model_type=5",            #model used for predictions
         "--debug=true",             #debug mode
          "--runStage=1",             #current running stage
         "--refresh_data=false"        #reload data
)
startMatter(args) 

## load data
model_loadData()

## process data
model_processData()

## model fitting
model_fitModel()