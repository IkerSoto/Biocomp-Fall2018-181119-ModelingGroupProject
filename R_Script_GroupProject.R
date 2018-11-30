#Dynamic Modeling Group Project
#Team: Ellen Foley, Iker Soto

#Lotka-Volterra dynamic modeling

#Custom function for Lotka-Volterra model

#Parameters:
#b=prey birth rate, a=predator attack rate, e=conversion efficiency of prey to predators, s=predator death rate

ddLotkaVolt<-function(t,y,p){
  H=y[1]
  P=y[2]
  b=p[1]
  a=p[2]
  e=p[3]
  s=p[4]
  dHdt=(b*H)-(a*P*H)
  dPdt=(e*a*P*H)-(s*P)
  return(list(c(dHdt,dPdt)))
}

#Simulation for initial conditions: b=0.5, a=0.02, e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1
library(deSolve)
library(reshape2)
y1=c(25,5)
params1=c(0.5,0.02,0.1,0.2)
times1=seq(from =1, to=100, by=0.1)
modelSim=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params1)
modelSimOutput=data.frame(time=modelSim[,1],Herbivore=modelSim[,2],Predator=modelSim[,3])
modelSimOutput=melt(modelSimOutput,id.vars="time")
library(ggplot2)
ggplot(modelSimOutput,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when b is increased by a factor of 3: b=1.5, a=0.02, e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params2=c(1.5,0.02,0.1,0.2)
times1
modelSim2=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params2)
modelSimOutput2=data.frame(time=modelSim2[,1],Herbivore=modelSim2[,2],Predator=modelSim2[,3])
modelSimOutput2=melt(modelSimOutput2,id.vars="time")
ggplot(modelSimOutput2,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when b is decreased by a factor of 3: b=0.166,a=0.02, e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params3=c(0.166,0.02,0.1,0.2)
times1
modelSim3=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params3)
modelSimOutput3=data.frame(time=modelSim3[,1],Herbivore=modelSim3[,2],Predator=modelSim3[,3])
modelSimOutput3=melt(modelSimOutput3,id.vars="time")
ggplot(modelSimOutput3,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when a is increased by a factor of 3: b=0.5, a=0.06,e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params4=c(0.5,0.06,0.1,0.2)
times1
modelSim4=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params4)
modelSimOutput4=data.frame(time=modelSim4[,1],Herbivore=modelSim4[,2],Predator=modelSim4[,3])
modelSimOutput4=melt(modelSimOutput4,id.vars="time")
ggplot(modelSimOutput4,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when a is decreased by a factor of 3:b=0.5, a=0.0066,e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params5=c(0.5,0.0066,0.1,0.2)
times1
modelSim5=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params5)
modelSimOutput5=data.frame(time=modelSim5[,1],Herbivore=modelSim5[,2],Predator=modelSim5[,3])
modelSimOutput5=melt(modelSimOutput5,id.vars="time")
ggplot(modelSimOutput5,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when e is increased by a factor of 3:b=0.5, a=0.02,e=0.3, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params6=c(0.5,0.02,0.3,0.2)
times1
modelSim6=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params6)
modelSimOutput6=data.frame(time=modelSim6[,1],Herbivore=modelSim6[,2],Predator=modelSim6[,3])
modelSimOutput6=melt(modelSimOutput6,id.vars="time")
ggplot(modelSimOutput6,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when e is decreased by a factor of 3:b=0.5,a=0.02,e=0.033, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params7=c(0.5,0.02,0.033,0.2)
times1
modelSim7=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params7)
modelSimOutput7=data.frame(time=modelSim7[,1],Herbivore=modelSim7[,2],Predator=modelSim7[,3])
modelSimOutput7=melt(modelSimOutput7,id.vars="time")
ggplot(modelSimOutput7,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when s is increased by a factor of 3:b=0.5,a=0.02,e=0.1, s=0.6, H0=25, P0=5 and a time step of 0.1
y1
params8=c(0.5,0.02,0.1,0.6)
times1
modelSim8=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params8)
modelSimOutput8=data.frame(time=modelSim8[,1],Herbivore=modelSim8[,2],Predator=modelSim8[,3])
modelSimOutput8=melt(modelSimOutput8,id.vars="time")
ggplot(modelSimOutput8,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when s is decreased by a factor of 3:b=0.5,a=0.02,e=0.1, s=0.066, H0=25, P0=5 and a time step of 0.1
y1
params9=c(0.5,0.02,0.1,0.066)
times1
modelSim9=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params9)
modelSimOutput9=data.frame(time=modelSim9[,1],Herbivore=modelSim9[,2],Predator=modelSim9[,3])
modelSimOutput9=melt(modelSimOutput9,id.vars="time")
ggplot(modelSimOutput9,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

