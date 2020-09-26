

## Static h5 3D heterogeneity simulation

#load packages
library(gstat)
library(sp)
library(rgl)
library(rhdf5)
library(raster)
library(plot3D)
library(foreach)
library(doSNOW)
library(e1071)
library(doParallel)
library(simsalapar)
library(rhdf5)

# set-up parallel computation
cl<-makeCluster(4) #change the 4 to your number of CPU cores
registerDoSNOW(cl)


# create a regular grid
nx=50 # number of columns
ny=50 # number of rows
nz =20
srgr <- expand.grid(1:ny, 1:nx, 1:nz)
names(srgr) <- c('x','y', 'z')
gridded(srgr)<-~x+y+z

# variogram model to generate simulations
test <- vgm(15, "Gau", range=2) # range 2 because it's 2 x 5 m, 10m
# 3D anisotopy (not used here) anis= c(30, 10, 0, 0.5, 0.3) 
#vgm model types: "Gau", "Sph", "Exp" for Gaussian, sperical and exponential
g<-gstat(formula=z~x+y+z, locations=~x+y+z, dummy=T,beta=15, model=test, nmax=20) 

nsim <- 300 # number of simulations

# simulation 
system.time(
  svm_predictions<-foreach(i=1:1,
                           .combine=c,.packages=c("gstat", "doParallel")) %dopar% {
                             sim1 <- (predict(g,newdata=srgr, nsim=nsim))
                             
                           })

for (l in 1:nsim) {

#format the results accordingly...
  results <- svm_predictions[l]@data
  sim1 <- unlist(results)
  sim1 <-unname(sim1) 
  normalized = (sim1-min(sim1))/(max(sim1)-min(sim1))
  sim1 <-normalized*20
  sim1 <-array(sim1, dim=c(nx,ny,nz))
  sim1 <- aperm(sim1, c(3,2,1))
  
  # h5ls("~/test-000.h5") to look at h5 files
  saturation<-h5read("/run/media/acooke/Elements/Sim/Gaus_10m/test-000.h5", "Time:  0.00000E+00 d/Liquid_Saturation")
  #... to write simualtion as new subsurface properties
  saturation<- sim1
  # create new h5 datafile
  system(paste("cp /run/media/acooke/Elements/Sim/Gaus_10m/test-000.h5 /run/media/acooke/Elements/Sim/Gaus_10m/test-",l,".h5",sep=""))
  # save
  h5write( saturation, file = paste("/run/media/acooke/Elements/Sim/Gaus_10m/test-",l,".h5", sep=""), name= "Time:  0.00000E+00 d/Liquid_Saturation")
  
}



# system("sh /home/acooke/Desktop/Desktop/PflotranR/3DSimpflotran/RealSim/sim.sh")

#####

## download results
system("rsync -arv -e 'ssh -p 10022' cooke@localhost:/home/cooke/Pflotran/tests/Dec/Sim/Sph_20_30percent/output* /home/acooke/Desktop/Desktop/PflotranR/3DSimpflotran/RealSim/Sph_20m_30percent/")

df <- data.frame(matrix(0, ncol=c(10), nrow=c(nsim)))
colnames(df)<- c("sim_#", "VGG_p1", "VGG_p2", "VGG_p3", "grav_h0_p1","grav_h0_p2","grav_h0_p3", "grav_h2_p1","grav_h2_p2", "grav_h2_p3")
# df
nsim <- 100

for( gg in 1:nsim) {
  # read simulated gravity
  gravi <- read.table(paste("/home/acooke/Desktop/Desktop/PflotranR/3DSimpflotran/RealSim/Sph_20m_30percent/output_gravi_test-",gg,".h5.txt", sep=""))
  gravi[,6] <- gravi[,6]*10
  
  grav_h0= gravi[seq(1, nrow(gravi), 2), ]
  grav_h2= gravi[seq(2, nrow(gravi), 2), ]
  
  #tripod height differences
  D1 <- 0.5925# m
  D2 <- 0.5825# m
  D <- D1 + D2
  D1 <- D1*100
  D2 <- D2*100
  D <- D*100
  
  # calculate VGG
  grads02 <- c(0)
  for (i in 1:nrow(grav_h0)) {
    grads02[i] <- (grav_h0[i,6]-grav_h2[i,6])/D
  }
  
  # subtract first value if required
  # grav_h0[,6] <-grav_h0[,6] - grav_h0[1,6]
  # grav_h2[,6] <-grav_h2[,6] - grav_h2[1,6]
  
  # write VGG and gravity in dateframe
  df[gg,c("VGG_p1","VGG_p2","VGG_p3")] <- grads02
  df[gg,c("grav_h0_p1", "grav_h0_p2", "grav_h0_p3")] <- grav_h0[,6] 
  df[gg,c("grav_h2_p1", "grav_h2_p2", "grav_h2_p3")] <- grav_h2[,6] 
  
}

# save dateframe as text file
write.table(df, "/home/acooke/Desktop/Desktop/PflotranR/3DSimpflotran/RealSim/Sph_20m_30percent/df_Sph_20m_higher.txt")

# Exploratory plots

# Histogram
hist(df$VGG_p1*1000, breaks=20) # x 1000 for Eotvoes
