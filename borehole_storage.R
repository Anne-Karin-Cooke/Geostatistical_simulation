setwd('/run/media/acooke/Elements/Sim/borehole_final/')

# Read in dummy h5 file for format
library(rhdf5)
saturation<-h5read("test-000.h5", "Time:  0.00000E+00 d/Liquid_Saturation")

# Empty h5 file
test_sat <- saturation
test_sat[,,]<- 0

# locations
# centre <- 125
# size <- 20

# parameter choices
# z vertical extents
# x, y vertical extents
z_sd2 <- c(5,10) # the 20 m deep boreholes : sd1 and sd1
z_sd1 <- c(5,10)

z_sc1 <- c(5,10,20) # 50 m deep borehole : sc1

x <- c(10,20,30)
y <- c(10,20,30)

x1 <- c(10,20,30)
y1 <- c(10,20,30)

x2 <- c(10,20,30)
y2 <- c(10,20,30)


# discretization 5 m
discr <- 5

# simulation loop

for(i in 1:length(z_sd2)) {
  for(j in 1:length(z_sd1)) {
    for(jj in 1:length(z_sc1)) {
      for(k in 1:length(x)) {
        for (l in 1:length(y)){
          for(m in 1:length(x1)) {
            for (n in 1:length(y1)){
              for(o in 1:length(x2)) {
                for (p in 1:length(y2)){
                  
                  test_sat[,,]<- 0
                  test_sat[(8:(8+z_sd2[i]/discr)),((10-(y[l]/2)/discr)):(10+(y[l]/2)/discr), ((6-(x[k]/2)/discr)):((6+(x[k]/2)/discr))] <- 0.15
                  test_sat[(8:(8+z_sd1[j]/discr)),((10-(y1[n]/2)/discr)):(10+(y1[n]/2)/discr), ((14-(x1[m]/2)/discr)):((14+(x1[m]/2)/discr))] <- 0.15
                  test_sat[(0:(0+z_sc1[jj]/discr)),((10-(y2[p]/2)/discr)):(10+(y2[p]/2)/discr), ((14-(x2[o]/2)/discr)):((14+(x2[o]/2)/discr))] <- 0.15
                  
  #generate new h5 file and write into                
                  system(paste("cp /run/media/acooke/Elements/Sim/borehole_final/test-000.h5 test-z_sd2",z_sd2[i],"_z_sd1",z_sd1[j],"_z_sc1",z_sc1[jj],"_x_sd2",x[k],"_y_sd2",y[l],"_x_sd1",x1[m],"_y_sd1",y1[n],"_x_sc1",x2[o],"_y_sc1",y2[p],".h5",sep=""))
                  h5write( test_sat, file = paste("/run/media/acooke/Elements/Sim/borehole_final/test-z_sd2",z_sd2[i],"_z_sd1",z_sd1[j],"_z_sc1",z_sc1[jj],"_x_sd2",x[k],"_y_sd2",y[l],"_x_sd1",x1[m],"_y_sd1",y1[n],"_x_sc1",x2[o],"_y_sc1",y2[p],".h5",sep=""), name= "Time:  0.00000E+00 d/Liquid_Saturation")
                  
                }
              }
            }
          }
        }
      }
    }
  }
}



# system("rsync -arv -e 'ssh -p port_number' cooke@localhost:/output* /home/acooke/folder/")

df <- data.frame(matrix(0, ncol=c(19), nrow=c(4011)))
colnames(df)<- c("sim_#", "VGG_p1", "VGG_p2", "VGG_p3", "grav_h0_p1","grav_h0_p2","grav_h0_p3", "grav_h2_p1","grav_h2_p2", "grav_h2_p3","z_sd2_","z_sd1_", "z_sc1_","x_","y_", "x1_","y1_","x2_","y2_")


# create a list of files
setwd('/run/media/acooke/Elements/Sim/borehole_final/results/')
list.filenames<-list.files(pattern="output_")
list.filenames

grads02 <- c(0)

# create a loop to read in your data
for (i in 1:(length(list.filenames))){

  gravi <- read.table(paste("/run/media/acooke/Elements/Sim/borehole_final/results/",as.character(list.filenames[i]),sep=""))
  gravi[,6] <- gravi[,6]*10
  
  grav_h0= gravi[seq(1, nrow(gravi), 2), ]
  grav_h2= gravi[seq(2, nrow(gravi), 2), ]
  
  
  D1 <- 0.5925# m
  D2 <- 0.5825# m
  D <- D1 + D2
  D1 <- D1*100
  D2 <- D2*100
  D <- D*100
  
  
  for (j in 1:nrow(grav_h0)) {
    grads02[j] <- (grav_h0[j,6]-grav_h2[j,6])/D
  }
  
  df[i,c("VGG_p1","VGG_p2","VGG_p3")] <- grads02
  df[i,c("grav_h0_p1", "grav_h0_p2", "grav_h0_p3")] <- grav_h0[,6] 
  df[i,c("grav_h2_p1", "grav_h2_p2", "grav_h2_p3")] <- grav_h2[,6] 
  df[i,1]<-as.character(list.filenames[i])
  df[i,c("z_sd2_")] <- as.numeric(sub("(?i).*z_sd2_*?(\\d+).*", "\\1", list.filenames[i]))
  df[i,c("z_sd1_")] <- as.numeric(sub("(?i).*z_sd1_*?(\\d+).*", "\\1", list.filenames[i]))
  df[i,c("z_sc1_")] <- as.numeric(sub("(?i).*z_sc1_*?(\\d+).*", "\\1", list.filenames[i]))
  df[i,c("x_")] <- as.numeric(sub("(?i).*x_sd2_*?(\\d+).*", "\\1", list.filenames[i]))
  df[i,c("y_")] <- as.numeric(sub("(?i).*y_sd2_*?(\\d+).*", "\\1", list.filenames[i]))
  df[i,c("x1_")] <- as.numeric(sub("(?i).*x_sd1_*?(\\d+).*", "\\1", list.filenames[i]))
  df[i,c("y1_")] <- as.numeric(sub("(?i).*y_sd1_*?(\\d+).*", "\\1", list.filenames[i]))
  df[i,c("x2_")] <- as.numeric(sub("(?i).*x_sc1_*?(\\d+).*", "\\1", list.filenames[i]))
  df[i,c("y2_")] <- as.numeric(sub("(?i).*y_sc1_*?(\\d+).*", "\\1", list.filenames[i]))
  
}

# df

# write.table(df, "/home/df.txt")

# VGG in Eotvoes
df$VGG_p1 <- df$VGG_p1*1000 
df$VGG_p2 <- df$VGG_p2*1000 
df$VGG_p3 <- df$VGG_p3*1000 

## Plot results (hsitograms)
par(mfrow=c(1,1))
boxplot(df[,c(2:4)], las=2, col=c("blue", "turquoise", "red"), names=c("p1", "p2", "p3"), main="15 % saturation difference")
grid(col="grey2")

par(mfrow=c(3,1))

hist(df$VGG_p1, breaks = 50, main= paste("historgram VGG pillar 1 \n mean =",round(mean(df$VGG_p1))), freq = F, xlab="vertical gravity gradients [Eoetvoes]" )
grid(col="darkgrey")
hist(df$VGG_p2, breaks = 50, main= paste("historgram VGG pillar 2 \n mean =",round(mean(df$VGG_p2))),freq = F , xlab="vertical gravity gradients [Eoetvoes]" )
grid(col="darkgrey")
hist(df$VGG_p3, breaks = 50, main= paste("historgram VGG pillar 3 \n mean =",round(mean(df$VGG_p3))),freq = F , xlab="vertical gravity gradients [Eoetvoes]" )
grid(col="darkgrey")


# df <- read.table("/run/media/acooke/Elements/Sim/borehole3/results/df.txt")

# Plot differences between VGG

diff_p1_p2 <- df$VGG_p1-df$VGG_p2
diff_p2_p3 <- df$VGG_p2-df$VGG_p3
diff_p1_p3 <- df$VGG_p1-df$VGG_p3

diff <- cbind(diff_p1_p2,diff_p2_p3 ,diff_p1_p3 )
diff <- diff*1000
colnames(diff) <- c("p1-p2", "p2-p3", "p1-p3")
boxplot(diff, col=c("grey"), ylab="VGG [E]", main="VGG differences between pillars \n borehole wet patches simulation")
grid(col="grey2")


