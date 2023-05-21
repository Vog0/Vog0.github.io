library(rgl)
library(gifski)
library(pdftools)  ## for converting PDF to PNG
library(png)       ## for reading png

setwd("~/My_Projects/Endless-Engines")
clear3d()

#~~~~~~~~~~~~~~~~~~~
#### Functions ####
#~~~~~~~~~~~~~~~~~~~

UFO = list()
{
  #Up and Down
  r <- function(y) (y-3)^2/2
  int <- c(0, 1.5)
  ny <- 20
  ntheta <- 36
  
  y <- seq(int[1], int[2], length.out = ny)
  cols <- colorRampPalette(c("#674019", "yellow"), space = "Lab")(ny)
  
  UFO[[1]] = translate3d(rotate3d(turn3d(y, r(y), n = ntheta,  smooth = TRUE, material = list(color = rep(cols, each = 4*ntheta))),pi/2,0,0,1),0,-0.3,0)
  UFO[[2]] = translate3d(rotate3d(turn3d(y, r(y), n = ntheta,  smooth = TRUE, material = list(color = rep(cols, each = 4*ntheta))),-pi/2,0,0,1),0,0.3,0)
  
  #Center
  UFO[[3]] = cylinder3d(center=matrix(c(0,-0.45,0,0,0.45,0),byrow = T,ncol=3),radius = 4,sides=100, color = "#CD7F32")
  
  #Ball top
  circ <- function(y) sqrt(1.3^2-y^2)
  int <- c(-1.3, 1.3)
  ny <- 20
  ntheta <- 36
  
  y <- seq(int[1], int[2], length.out = ny)
  
  UFO[[4]] = translate3d(turn3d(y, circ(y), n = ntheta,  smooth = TRUE, material3d(1,alpha=0.2), material = list(color = rep("cyan", each = 4*ntheta))),0,1.6,0)
  
  
  #Bottom
  UFO[[5]] = cylinder3d(center=matrix(c(0,-1.8,0,0,0,0),byrow = T,ncol=3),radius = 1.3,sides=100, closed = -2,color="#674019")
  
  #Circular things
  UFO[[6]] = cylinder3d(center=matrix(c(1.5,0,1.5,1.9,1.1,1.9),byrow = T,ncol=3),radius = 0.5,sides=50, closed = -2,color="#CD7F32")
  UFO[[7]] = cylinder3d(center=matrix(c(1.5,0,-1.5,1.9,1.1,-1.9),byrow = T,ncol=3),radius = 0.5,sides=50, closed = -2,color="#CD7F32")
  UFO[[8]] = cylinder3d(center=matrix(c(-1.5,0,-1.5,-1.9,1.1,-1.9),byrow = T,ncol=3),radius = 0.5,sides=50, closed = -2,color="#CD7F32")
  UFO[[9]] = cylinder3d(center=matrix(c(-1.5,0,1.5,-1.9,1.1,1.9),byrow = T,ncol=3),radius = 0.5,sides=50, closed = -2,color="#CD7F32")
  
  UFO[[10]] = cylinder3d(center=matrix(c(1.5,0,1.5,1.9,-1.1,1.9),byrow = T,ncol=3),radius = 0.5,sides=50, closed = -2,color="#CD7F32")
  UFO[[11]] = cylinder3d(center=matrix(c(1.5,0,-1.5,1.9,-1.1,-1.9),byrow = T,ncol=3),radius = 0.5,sides=50, closed = -2,color="#CD7F32")
  UFO[[12]] = cylinder3d(center=matrix(c(-1.5,0,-1.5,-1.9,-1.1,-1.9),byrow = T,ncol=3),radius = 0.5,sides=50, closed = -2,color="#CD7F32")
  UFO[[13]] = cylinder3d(center=matrix(c(-1.5,0,1.5,-1.9,-1.1,1.9),byrow = T,ncol=3),radius = 0.5,sides=50, closed = -2,color="#CD7F32")
}

drawVolcano = function(sx = -20, sy = -2, sz = -40){
  {
    y <- t(volcano)/100 + sy      # Exaggerate the relief
    z <- seq(sz,sz+80)/10  # 10 meter spacing (S to N)
    x <- seq(sx,sx+60)/10  # 10 meter spacing (E to W)
    
    ylim <- range(y)
    ylen <- ylim[2] - ylim[1] + 1
    colorlut <- terrain.colors(ylen) # height color lookup table
    col <- colorlut[ y - ylim[1] + 1 ] # assign colors to heights for each point
    #surface3d(x, y[1:61,1:81], -z, color = "gold2", back = "lines",texture = "sand.png")
    surface3d(x, y[1:61,1:81], -z, color = "gold2", back = "lines")
  } #Volcano
}

drawMyVolcanos = function(z){
  drawVolcano(sx = -8, sy= -2.2,sz = -60+z)
  drawVolcano(sx = -8, sy= -2.3,sz = 10+z)
  drawVolcano(sx = -63 ,sy=-2.224,sz = 10+z)
  drawVolcano(sx = -65 ,sy=-2.2,sz = -60+z)
  
  drawVolcano(sx = -8 ,sy=-2.2,sz = -110+z)
  drawVolcano(sx = -65 ,sy=-2.2,sz = -110+z)
}

sectionProgress <- function(i,start,finish){
  return(max(0,min(1,(i-start)/(finish-start))))
}

clearMyStuff = function(resview = T, axis = T){
  bg3d(sphere = F, texture = system.file("textures/vg.png", package = "rgl"), back = "filled" )
  clear3d()
  h_distance=50
  h1=cylinder3d(center=matrix(c(-h_distance,-h_distance-0.1,0,0,0,0),ncol=3),color="white",radius = 0.1,closed=-2)
  h2=cylinder3d(center=matrix(c( h_distance, h_distance+0.1,0,0,0,0),ncol=3),color="white",radius = 0.1,closed=-2)
  h3=cylinder3d(center=matrix(c(0,0,-h_distance,-h_distance-0.1,0,0),ncol=3),color="white",radius = 0.1,closed=-2)
  h4=cylinder3d(center=matrix(c(0,0, h_distance, h_distance+0.1,0,0),ncol=3),color="white",radius = 0.1,closed=-2)
  h5=cylinder3d(center=matrix(c(0,0,0,0,-h_distance,-h_distance-0.1),ncol=3),color="white",radius = 0.1,closed=-2)
  h6=cylinder3d(center=matrix(c(0,0,0,0, h_distance, h_distance+0.1),ncol=3),color="white",radius = 0.1,closed=-2)
  shade3d(h1)
  shade3d(h2)
  shade3d(h3)
  shade3d(h4)
  shade3d(h5)
  shade3d(h6)

  if(axis){
    xx = cylinder3d(matrix(c(-10,0,0,10,0,0),ncol=3,byrow=T),radius = 0.03, col='red',sides=10,closed = -2,lit=T)
    yy = cylinder3d(matrix(c(0,-10,0,0,10,0),ncol=3,byrow=T),radius = 0.03, col='blue',sides=10,closed = -2,lit=T)
    zz = cylinder3d(matrix(c(0,0,-10,0,0,10),ncol=3,byrow=T),radius = 0.03, col='green',sides=10,closed = -2,lit=T)
    shade3d(xx)
    shade3d(yy)
    shade3d(zz)
  }
  if(resview){
    view3d(-24,-4.8,fov=30,type="modelviewpoint")
    observer3d(0,0,3)
  }
}

drawUFO = function(ufo){
  n = length(ufo)
  for(i in 1:n){
    if(i==4){
        shade3d(ufo[[i]],alpha = 0.5)
    }else{
        shade3d(ufo[[i]])
    }
  }
}

rotateUFO = function(ufo,axis,theta){
  n = length(ufo)
  for(i in 1:n){
    ufo[[i]] = rotate3d(ufo[[i]],theta,axis[1],axis[2],axis[3])
  }
  return(ufo)
}

moveUFO = function(ufo,vec){
  n = length(ufo)
  for(i in 1:n){
    ufo[[i]] = translate3d(ufo[[i]],vec[1],vec[2],vec[3])
  }
  return(ufo)
}

scaleUFO = function(ufo,s){
  n = length(ufo)
  for(i in 1:n){
    ufo[[i]] = scale3d(ufo[[i]],s,s,s)
  }
  return(ufo)
}

#~~~~~~~~~~~~~~~~~~~
#### Draw Stuff ####
#~~~~~~~~~~~~~~~~~~~
clearMyStuff(axis=F)

drawMyVolcanos(0)

cUFO = c(0,0,0)
frameUFO = scaleUFO(UFO,0.1)
frameUFO = rotateUFO(frameUFO,c(0,1,0),2*pi*50/24)
frameUFO = rotateUFO(frameUFO,c(0,0,1),pi/10)
frameUFO = rotateUFO(frameUFO,c(0,1,0),-pi*3/2*1)
frameUFO = moveUFO(frameUFO,cUFO)

drawUFO(frameUFO)





        