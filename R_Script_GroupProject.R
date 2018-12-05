#Dynamic Modeling Group Project
#Team: Ellen Foley, Iker Soto

#Lotka-Volterra dynamic modeling

#Writing the custom function for Lotka-Volterra model

#In addition to the state variables H (herbivore) and P (predator),the following parameters are included in the model:
#b=prey birth rate, a=predator attack rate, e=conversion efficiency of prey to predators, s=predator death rate

#Creating the custom function: 
ddLotkaVolt<-function(t,y,p){
  H=y[1]
  P=y[2]
  b=p[1]
  a=p[2]
  e=p[3]
  s=p[4]
  dHdt=(b*H)-(a*P*H)  #Differential equation of the Lotka-Volterra model: change in herbivore population over time
  dPdt=(e*a*P*H)-(s*P) #Differential equation of the Lotka-Volterra model: change in predator population over time
  return(list(c(dHdt,dPdt)))
}

#Simulation for initial conditions using custom function ddLotkaVolt: 
#b=0.5, a=0.02, e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1 within a range of 1 to 200
library(deSolve)
library(reshape2)
y1=c(25,5)
params1=c(0.5,0.02,0.1,0.2)
times1=seq(from =1, to=200, by=0.1)
#Solving the system of diferential equations using the ode() function
modelSim=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params1)
#Redirecting the output of the equation system to a data frame with three columns in the following order:
#Time,Num. Herbivores,Num.Predators 
modelSimOutput=data.frame(time=modelSim[,1],Herbivore=modelSim[,2],Predator=modelSim[,3])
#Reshaping data frame using Reshape and melt() so that both variables (H,P) can be graphed
modelSimOutput=melt(modelSimOutput,id.vars="time")
#Graphing the results
library(ggplot2)
ggplot(modelSimOutput,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

###Additional simulations where initial parameters are changed.###

##Simulation of cases where prey birth rate (b) is either lower or higher than the initial condition.##
####Simulating an increase in prey birth rate (b)####
#Simulation when "b" is increased by a factor of 3: b=1.5, a=0.02, e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params2=c(1.5,0.02,0.1,0.2)
times1
modelSim2=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params2)
modelSimOutput2=data.frame(time=modelSim2[,1],Herbivore=modelSim2[,2],Predator=modelSim2[,3])
modelSimOutput2=melt(modelSimOutput2,id.vars="time")
ggplot(modelSimOutput2,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when "b" is increased by a factor of 2: b=1, a=0.02, e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params2.1=c(1,0.02,0.1,0.2)
times1
modelSim2.1=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params2)
modelSimOutput2.1=data.frame(time=modelSim2.1[,1],Herbivore=modelSim2.1[,2],Predator=modelSim2.1[,3])
modelSimOutput2.1=melt(modelSimOutput2.1,id.vars="time")
ggplot(modelSimOutput2.1,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when "b" is increased by a factor of 4: b=2, a=0.02, e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params2.2=c(2,0.02,0.1,0.2)
times1
modelSim2.2=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params2)
modelSimOutput2.2=data.frame(time=modelSim2.2[,1],Herbivore=modelSim2.2[,2],Predator=modelSim2.2[,3])
modelSimOutput2.2=melt(modelSimOutput2.2,id.vars="time")
ggplot(modelSimOutput2.2,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()


####Simulating a decrease in prey birth rate (b)####
#Simulation when "b" is decreased by a factor of 3: b=0.166,a=0.02, e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params3=c(0.166,0.02,0.1,0.2)
times1
modelSim3=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params3)
modelSimOutput3=data.frame(time=modelSim3[,1],Herbivore=modelSim3[,2],Predator=modelSim3[,3])
modelSimOutput3=melt(modelSimOutput3,id.vars="time")
ggplot(modelSimOutput3,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when "b" is decreased by a factor of 2: b=0.25,a=0.02, e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params3.1=c(0.25,0.02,0.1,0.2)
times1
modelSim3.1=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params3.1)
modelSimOutput3.1=data.frame(time=modelSim3.1[,1],Herbivore=modelSim3.1[,2],Predator=modelSim3.1[,3])
modelSimOutput3.1=melt(modelSimOutput3.1,id.vars="time")
ggplot(modelSimOutput3.1,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when "b" is decreased by a factor of 4: b=0.125,a=0.02, e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params3.2=c(0.125,0.02,0.1,0.2)
times1
modelSim3.2=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params3.2)
modelSimOutput3.2=data.frame(time=modelSim3.2[,1],Herbivore=modelSim3.2[,2],Predator=modelSim3.2[,3])
modelSimOutput3.2=melt(modelSimOutput3.2,id.vars="time")
ggplot(modelSimOutput3.2,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()


##Simulation of cases where predator attack rate (a) is either lower or higher than the initial condition.##
##Simulating an increase in predator attack rate (a)##
#Simulation when "a" is increased by a factor of 3: b=0.5, a=0.06,e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params4=c(0.5,0.06,0.1,0.2)
times1
modelSim4=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params4)
modelSimOutput4=data.frame(time=modelSim4[,1],Herbivore=modelSim4[,2],Predator=modelSim4[,3])
modelSimOutput4=melt(modelSimOutput4,id.vars="time")
ggplot(modelSimOutput4,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when "a" is increased by a factor of 2:b=0.5, a=0.04,e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params4.1=c(0.5,0.04,0.1,0.2)
times1
modelSim4.1=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params4.1)
modelSimOutput4.1=data.frame(time=modelSim4.1[,1],Herbivore=modelSim4.1[,2],Predator=modelSim4.1[,3])
modelSimOutput4.1=melt(modelSimOutput4.1,id.vars="time")
ggplot(modelSimOutput4.1,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when "a" is increased by a factor of 4:b=0.5, a=0.08,e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params4.2=c(0.5,0.08,0.1,0.2)
times1
modelSim4.2=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params4.2)
modelSimOutput4.2=data.frame(time=modelSim4.2[,1],Herbivore=modelSim4.2[,2],Predator=modelSim4.2[,3])
modelSimOutput4.2=melt(modelSimOutput4.2,id.vars="time")
ggplot(modelSimOutput4.2,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()


####Simulating a decrease in predator attack rate (a)####
#Simulation when "a" is decreased by a factor of 3:b=0.5, a=0.0066,e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params5=c(0.5,0.0066,0.1,0.2)
times1
modelSim5=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params5)
modelSimOutput5=data.frame(time=modelSim5[,1],Herbivore=modelSim5[,2],Predator=modelSim5[,3])
modelSimOutput5=melt(modelSimOutput5,id.vars="time")
ggplot(modelSimOutput5,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when "a" is decreased by a factor of 2:b=0.5, a=0.01,e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params5.1=c(0.5,0.01,0.1,0.2)
times1
modelSim5.1=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params5.1)
modelSimOutput5.1=data.frame(time=modelSim5.1[,1],Herbivore=modelSim5.1[,2],Predator=modelSim5.1[,3])
modelSimOutput5.1=melt(modelSimOutput5.1,id.vars="time")
ggplot(modelSimOutput5.1,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when "a" is decreased by a factor of 4:b=0.5, a=0.005,e=0.1, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params5.2=c(0.5,0.005,0.1,0.2)
times1
modelSim5.2=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params5.2)
modelSimOutput5.2=data.frame(time=modelSim5.2[,1],Herbivore=modelSim5.2[,2],Predator=modelSim5.2[,3])
modelSimOutput5.2=melt(modelSimOutput5.2,id.vars="time")
ggplot(modelSimOutput5.2,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

##Simulation of cases where the conversion efficiency of prey to predators (e) is either lower or higher than the initial condition.##
##Simulating an increase in conversion efficiency of prey to predators (e)##
#Simulation when "e" is increased by a factor of 3:b=0.5, a=0.02,e=0.3, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params6=c(0.5,0.02,0.3,0.2)
times1
modelSim6=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params6)
modelSimOutput6=data.frame(time=modelSim6[,1],Herbivore=modelSim6[,2],Predator=modelSim6[,3])
modelSimOutput6=melt(modelSimOutput6,id.vars="time")
ggplot(modelSimOutput6,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when "e" is increased by a factor of 2:b=0.5, a=0.02,e=0.2, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params6.1=c(0.5,0.02,0.2,0.2)
times1
modelSim6.1=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params6.1)
modelSimOutput6.1=data.frame(time=modelSim6.1[,1],Herbivore=modelSim6.1[,2],Predator=modelSim6.1[,3])
modelSimOutput6.1=melt(modelSimOutput6.1,id.vars="time")
ggplot(modelSimOutput6.1,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when "e" is increased by a factor of 4:b=0.5, a=0.02,e=0.4, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params6.2=c(0.5,0.02,0.4,0.2)
times1
modelSim6.2=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params6.2)
modelSimOutput6.2=data.frame(time=modelSim6.2[,1],Herbivore=modelSim6.2[,2],Predator=modelSim6.2[,3])
modelSimOutput6.2=melt(modelSimOutput6.2,id.vars="time")
ggplot(modelSimOutput6.2,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()


####Simulating a decrease in conversion efficiency of prey to predators (e)####
#Simulation when "e" is decreased by a factor of 3:b=0.5,a=0.02,e=0.033, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params7=c(0.5,0.02,0.033,0.2)
times1
modelSim7=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params7)
modelSimOutput7=data.frame(time=modelSim7[,1],Herbivore=modelSim7[,2],Predator=modelSim7[,3])
modelSimOutput7=melt(modelSimOutput7,id.vars="time")
ggplot(modelSimOutput7,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when "e" is decreased by a factor of 2:b=0.5,a=0.02,e=0.05, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params7.1=c(0.5,0.02,0.05,0.2)
times1
modelSim7.1=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params7.1)
modelSimOutput7.1=data.frame(time=modelSim7.1[,1],Herbivore=modelSim7.1[,2],Predator=modelSim7.1[,3])
modelSimOutput7.1=melt(modelSimOutput7.1,id.vars="time")
ggplot(modelSimOutput7.1,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when "e" is decreased by a factor of 4:b=0.5,a=0.02,e=0.025, s=0.2, H0=25, P0=5 and a time step of 0.1
y1
params7.2=c(0.5,0.02,0.025,0.2)
times1
modelSim7.2=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params7.2)
modelSimOutput7.2=data.frame(time=modelSim7.2[,1],Herbivore=modelSim7.2[,2],Predator=modelSim7.2[,3])
modelSimOutput7.2=melt(modelSimOutput7.2,id.vars="time")
ggplot(modelSimOutput7.2,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

##Simulation of cases where predator death rate (s) is either lower or higher than the initial condition.##
##Simulating an increase in predator death rate##
#Simulation when "s" is increased by a factor of 3:b=0.5,a=0.02,e=0.1, s=0.6, H0=25, P0=5 and a time step of 0.1
y1
params8=c(0.5,0.02,0.1,0.6)
times1
modelSim8=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params8)
modelSimOutput8=data.frame(time=modelSim8[,1],Herbivore=modelSim8[,2],Predator=modelSim8[,3])
modelSimOutput8=melt(modelSimOutput8,id.vars="time")
ggplot(modelSimOutput8,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when s is increased by a factor of 2:b=0.5,a=0.02,e=0.1, s=0.4, H0=25, P0=5 and a time step of 0.1
y1
params8.1=c(0.5,0.02,0.1,0.4)
times1
modelSim8.1=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params8.1)
modelSimOutput8.1=data.frame(time=modelSim8.1[,1],Herbivore=modelSim8.1[,2],Predator=modelSim8.1[,3])
modelSimOutput8.1=melt(modelSimOutput8.1,id.vars="time")
ggplot(modelSimOutput8.1,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when s is increased by a factor of 4:b=0.5,a=0.02,e=0.1, s=0.8, H0=25, P0=5 and a time step of 0.1
y1
params8.2=c(0.5,0.02,0.1,0.8)
times1
modelSim8.2=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params8.2)
modelSimOutput8.2=data.frame(time=modelSim8.2[,1],Herbivore=modelSim8.2[,2],Predator=modelSim8.2[,3])
modelSimOutput8.2=melt(modelSimOutput8.2,id.vars="time")
ggplot(modelSimOutput8.2,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()


####Simulating a decrease in predator death rate####
#Simulation when "s" is decreased by a factor of 3:b=0.5,a=0.02,e=0.1, s=0.066, H0=25, P0=5 and a time step of 0.1
y1
params9=c(0.5,0.02,0.1,0.066)
times1
modelSim9=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params9)
modelSimOutput9=data.frame(time=modelSim9[,1],Herbivore=modelSim9[,2],Predator=modelSim9[,3])
modelSimOutput9=melt(modelSimOutput9,id.vars="time")
ggplot(modelSimOutput9,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when "s" is decreased by a factor of 2:b=0.5,a=0.02,e=0.1, s=0.1, H0=25, P0=5 and a time step of 0.1
y1
params9.1=c(0.5,0.02,0.1,0.1)
times1
modelSim9.1=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params9.1)
modelSimOutput9.1=data.frame(time=modelSim9.1[,1],Herbivore=modelSim9.1[,2],Predator=modelSim9.1[,3])
modelSimOutput9.1=melt(modelSimOutput9.1,id.vars="time")
ggplot(modelSimOutput9.1,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when s is decreased by a factor of 4:b=0.5,a=0.02,e=0.1, s=0.05, H0=25, P0=5 and a time step of 0.1
y1
params9.2=c(0.5,0.02,0.1,0.05)
times1
modelSim9.2=ode(y=y1,times=times1,func=ddLotkaVolt,parms=params9.2)
modelSimOutput9.2=data.frame(time=modelSim9.2[,1],Herbivore=modelSim9.2[,2],Predator=modelSim9.2[,3])
modelSimOutput9.2=melt(modelSimOutput9.2,id.vars="time")
ggplot(modelSimOutput9.2,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()



=======
#Part II

### ROSENNZWEIG-MACARTHUR MODEL ### 

library(deSolve)
library(ggplot2)

#create a custom function 
RMmodel<- function(t,y,p){
  H=y[1] #first state variable 
  P=y[2] #second state variable 
  
  #parameters 
  b=p[1] 
  e=p[2]
  s=p[3]
  w=p[4]
  d=p[5]
  a=p[6]
  
  
  dHdt=(b*H*(1-a*H))-(w*(H/(d+H))*P)
  dPdt=(e*w*(H/(d+H))*P)-(s*P)
  return(list(c(dHdt,dPdt)))
}

#Define parameters, initial values for state variables, and time steps 
params1=c(0.8,0.07,0.2,5,400,0.001)
NO=c(500,120)
times= 1:100

# Simulate the model using ode()
modelSim1=ode(y=NO,times=times,func=RMmodel,parms=params1)

#model state variables in subsequent columns convert to a dataframe for plotting purposes
modelOutput1=data.frame(time=modelSim1[,1],N1=modelSim1[,2],N2=modelSim1[,3])

#plot output of simulation
ggplot(modelOutput1,aes(x=time,y=N1))+geom_line()+geom_line(data=modelOutput1, mapping=aes(x=time,y=N2),col='red')+theme_classic() 

###Model Simulation 2 with different parameters- Increase b 

#Define parameters, intitial values for state variables, and time steps 
params2=c(1.8,0.07,0.2,5,400,0.001)
NO=c(500,120)
times= 1:200

# Simulate the model using ode()
modelSim2=ode(y=NO,times=times,func=RMmodel,parms=params1)
#model state variables in subsequent columns convert to a dataframe for plotting purposes
modelOutput2=data.frame(time=modelSim2[,1],N1=modelSim2[,2],N2=modelSim2[,3])
#plot output of simulation
ggplot(modelOutput2,aes(x=time,y=N1))+geom_line()+geom_line(data=modelOutput2, mapping=aes(x=time,y=N2),col='red')+theme_classic() 

###Simulation 3- increase e 
#Define parameters, intitial values for state variables, and time steps 
params3=c(0.8,0.2,0.2,5,400,0.001)
NO=c(500,120)
times= 1:200

# Simulate the model using ode()
modelSim3=ode(y=NO,times=times,func=RMmodel,parms=params3)
#model state variables in subsequent columns convert to a dataframe for plotting purposes
modelOutput3=data.frame(time=modelSim3[,1],N1=modelSim3[,2],N2=modelSim3[,3])
#plot output of simulation
ggplot(modelOutput3,aes(x=time,y=N1))+geom_line()+geom_line(data=modelOutput3, mapping=aes(x=time,y=N2),col='red')+theme_classic() 

###Simulation 4- decrease e 
#Define parameters, intitial values for state variables, and time steps 
params4=c(0.8,0.01,0.2,5,400,0.001)
NO=c(500,120)
times= 1:200

# Simulate the model using ode()
modelSim4=ode(y=NO,times=times,func=RMmodel,parms=params4)
#model state variables in subsequent columns convert to a dataframe for plotting purposes
modelOutput4=data.frame(time=modelSim4[,1],N1=modelSim4[,2],N2=modelSim4[,3])
#plot output of simulation
ggplot(modelOutput4,aes(x=time,y=N1))+geom_line()+geom_line(data=modelOutput4, mapping=aes(x=time,y=N2),col='red')+theme_classic() 

###Simulation 5- increase s 
params5=c(0.8,0.07,0.8,5,400,0.001)
NO=c(500,120)
times= 1:200

# Simulate the model using ode()
modelSim5=ode(y=NO,times=times,func=RMmodel,parms=params5)
#model state variables in subsequent columns convert to a dataframe for plotting purposes
modelOutput5=data.frame(time=modelSim5[,1],N1=modelSim5[,2],N2=modelSim5[,3])
#plot output of simulation
ggplot(modelOutput5,aes(x=time,y=N1))+geom_line()+geom_line(data=modelOutput5, mapping=aes(x=time,y=N2),col='red')+theme_classic() 

###Simulation 6- decrease s 
params6=c(0.8,0.07,0.02,5,400,0.001)
NO=c(500,120)
times= 1:200

# Simulate the model using ode()
modelSim6=ode(y=NO,times=times,func=RMmodel,parms=params6)
#model state variables in subsequent columns convert to a dataframe for plotting purposes
modelOutput6=data.frame(time=modelSim6[,1],N1=modelSim6[,2],N2=modelSim6[,3])
#plot output of simulation
ggplot(modelOutput6,aes(x=time,y=N1))+geom_line()+geom_line(data=modelOutput6, mapping=aes(x=time,y=N2),col='red')+theme_classic() 

###Simulation 7- increase w 
params7=c(0.8,0.07,0.2,15,400,0.001)
NO=c(500,120)
times= 1:200

# Simulate the model using ode()
modelSim7=ode(y=NO,times=times,func=RMmodel,parms=params7)
#model state variables in subsequent columns convert to a dataframe for plotting purposes
modelOutput7=data.frame(time=modelSim7[,1],N1=modelSim7[,2],N2=modelSim7[,3])
#plot output of simulation
ggplot(modelOutput7,aes(x=time,y=N1))+geom_line()+geom_line(data=modelOutput7, mapping=aes(x=time,y=N2),col='red')+theme_classic() 


###Simulation 8- decrease w 
params8=c(0.8,0.07,0.2,1,400,0.001)
NO=c(500,120)
times= 1:200

# Simulate the model using ode()
modelSim8=ode(y=NO,times=times,func=RMmodel,parms=params8)
#model state variables in subsequent columns convert to a dataframe for plotting purposes
modelOutput8=data.frame(time=modelSim8[,1],N1=modelSim8[,2],N2=modelSim8[,3])
#plot output of simulation
ggplot(modelOutput8,aes(x=time,y=N1))+geom_line()+geom_line(data=modelOutput8, mapping=aes(x=time,y=N2),col='red')+theme_classic() 

###Simulation 9- increase d
params9=c(0.8,0.07,0.2,5,1200,0.001)
NO=c(500,120)
times= 1:200

# Simulate the model using ode()
modelSim9=ode(y=NO,times=times,func=RMmodel,parms=params9)
#model state variables in subsequent columns convert to a dataframe for plotting purposes
modelOutput9=data.frame(time=modelSim9[,1],N1=modelSim9[,2],N2=modelSim9[,3])
#plot output of simulation
ggplot(modelOutput9,aes(x=time,y=N1))+geom_line()+geom_line(data=modelOutput9, mapping=aes(x=time,y=N2),col='red')+theme_classic() 

###Simulation 10- decrease d
params10=c(0.8,0.07,0.2,5,150,0.001)
NO=c(500,120)
times= 1:200

# Simulate the model using ode()
modelSim10=ode(y=NO,times=times,func=RMmodel,parms=params10)
#model state variables in subsequent columns convert to a dataframe for plotting purposes
modelOutput10=data.frame(time=modelSim10[,1],N1=modelSim10[,2],N2=modelSim10[,3])
#plot output of simulation
ggplot(modelOutput10,aes(x=time,y=N1))+geom_line()+geom_line(data=modelOutput10, mapping=aes(x=time,y=N2),col='red')+theme_classic() 

###Simulation 11- increase alpha (a)
params11=c(0.8,0.07,0.2,5,400,0.004)
NO=c(500,120)
times= 1:200

# Simulate the model using ode()
modelSim11=ode(y=NO,times=times,func=RMmodel,parms=params11)
#model state variables in subsequent columns convert to a dataframe for plotting purposes
modelOutput11=data.frame(time=modelSim11[,1],N1=modelSim11[,2],N2=modelSim11[,3])
#plot output of simulation
ggplot(modelOutput11,aes(x=time,y=N1))+geom_line()+geom_line(data=modelOutput11, mapping=aes(x=time,y=N2),col='red')+theme_classic() 

###Simulation 12- decrease alpha (a)
params12=c(0.8,0.07,0.2,5,400,0.0005)
NO=c(500,120)
times= 1:200

# Simulate the model using ode()
modelSim12=ode(y=NO,times=times,func=RMmodel,parms=params12)
#model state variables in subsequent columns convert to a dataframe for plotting purposes
modelOutput12=data.frame(time=modelSim12[,1],N1=modelSim12[,2],N2=modelSim12[,3])
#plot output of simulation
ggplot(modelOutput12,aes(x=time,y=N1))+geom_line()+geom_line(data=modelOutput12, mapping=aes(x=time,y=N2),col='red')+theme_classic()

#Simulating the "Paradox of Enrichment for the Rosenzweig-MacArthur model by increasing the carrying capacity of the prey:

#Simulation when "a" is 0.00125
ParamsParadox=c(0.8,0.07,0.2,5,400,0.00125)
NO=c(500,120)
timesParadox=1:200
modelSimParadox=ode(y=NO, times=timesParadox,func=RMmodel,parms=ParamsParadox)
modelOutputParadox=data.frame(time=modelSimParadox[,1],Herbivore=modelSimParadox[,2],Predator=modelSimParadox[,3])
modelOutputParadox=melt(modelOutputParadox,id.vars="time")
ggplot(modelOutputParadox,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when "a" is 0.0009
ParamsParadox2=c(0.8,0.07,0.2,5,400,0.0009)
modelSimParadox2=ode(y=NO, times=timesParadox,func=RMmodel,parms=ParamsParadox2)
modelOutputParadox2=data.frame(time=modelSimParadox2[,1],Herbivore=modelSimParadox2[,2],Predator=modelSimParadox2[,3])
modelOutputParadox2=melt(modelOutputParadox2,id.vars="time")
ggplot(modelOutputParadox2,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when "a" is 0.0006
ParamsParadox3=c(0.8,0.07,0.2,5,400,0.0006)
modelSimParadox3=ode(y=NO, times=timesParadox,func=RMmodel,parms=ParamsParadox3)
modelOutputParadox3=data.frame(time=modelSimParadox3[,1],Herbivore=modelSimParadox3[,2],Predator=modelSimParadox3[,3])
modelOutputParadox3=melt(modelOutputParadox3,id.vars="time")
ggplot(modelOutputParadox3,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()

#Simulation when "a" is 0.0005
ParamsParadox4=c(0.8,0.07,0.2,5,400,0.0005)
modelSimParadox4=ode(y=NO, times=timesParadox,func=RMmodel,parms=ParamsParadox4)
modelOutputParadox4=data.frame(time=modelSimParadox4[,1],Herbivore=modelSimParadox4[,2],Predator=modelSimParadox4[,3])
modelOutputParadox4=melt(modelOutputParadox4,id.vars="time")
ggplot(modelOutputParadox4,aes(x=time,y=value))+geom_line(aes(color=variable))+theme_classic()






