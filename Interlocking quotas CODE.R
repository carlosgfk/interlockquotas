#TRANSFORMATION OF NON-INTERLOCKING OR SEMI-INTERLOCKING INTO INTERLOCKING QUOTA SETTINGS
#author: Carlos Ochoa
#date: 11/02/2024

#Set working directory (it only works in Rstudio) ##############################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Read functions and packages ###################################################
#(Adjust the path if needed)
source("quota_transformation_functions.R",echo = FALSE,verbose = FALSE)

#Parameters ####################################################################
num.sim.nonint = 100 #Number of similations for the non-interlocking quota setting. Keep this number small, as it is a slow simulation
num.sim.int = 1000 #Number of simulations for the interlocking quota settings. As the simulation is faster, we can use 1,000 simulations
code = "CODE" #This code has to match with the files used to upload quotas and panel availability

#Global vars ###################################################################
#Variables that must be inferred from data, do not modify them
global.target = NA #To be inferred later on from the original quota setting
quota.names = NA #Names of the variables involved in the quota settings, to be inferred later on from the original quota setting
num.vars = NA #Number of variables involved in the quota settings

#Data frame with results to be stored
df.simulation = data.frame(
  i = c(), #index of the quota setting, correlative
  scenario = c(), #description of the quota setting
  sim = c(), #number of simulations used to estimate performance
  sample = c(), #available sample
  completes = c(), #completes achieved
  quotafulls = c(), #quota fulls produced
  pending = c(), #pending completes to achieve the target sample size
  logs = c() #A recording of the types of participation produced during simulation: CO,CO,QF,CO,QF...
)

# Read panel data ##############################################################

file.name = paste0(code,"_panel.csv")
if (!file.exists(file.name)) file.name = paste0(code,"_panel.xlsx")
if (!file.exists(file.name)) stop("A file with the panel available has not been found")

read.results = read.panel(f=file.name)
  df.panel = read.results$df.panel #Data frame with the panel available
  num.vars = read.results$num.vars
  quota.names.original = read.results$quota.names.original
  quota.levels.original = read.results$quota.levels.original
  quota.names = read.results$quota.names
  quota.levels = read.results$quota.levels

# Read project quotas ##########################################################

#Read quotas using a general form to express quotas, valid for interlocking, non-interlocking and semi-interlocking
file.name = paste0(code,"_quotas.csv")
if (!file.exists(file.name)) file.name = paste0(code,"_quotas.xlsx")
if (!file.exists(file.name)) stop("A file with the non-interlocking or semi-interlocking quotas has not been found")
  
df.quotas.nonint = read.quotas(f = file.name, form="wide", q.names = quota.names, q.levels = quota.levels)

#Global target: sum targets of the first variable of the first quota setting
global.target = get.sample.size(df.quotas.nonint)

# SIMULATIONS ##################################################################

#1.Original scenario (non-interlocking) ###############################################

#Simulate fieldwork
df.simulation = simulate.fieldwork(
  scn = "Original non-interlocking",
  type = "general",
  s = num.sim.nonint,
  panel = df.panel,
  quotas = df.quotas.nonint,
  q.names = quota.names, 
  g.target = global.target,
  summary = df.simulation)

#2.Original scenario transformed to interlocking quotas ############################

#We try 3 potential starting points for the interlocking algorithm
starting.params = c("unit","ideal","proportional")
df.q.list = list()
prob.succ = c()

for (s in 1:length(starting.params)) {
  #Create crossing setting
  df.q.list[[s]] = interlock(
    df.q.nonint = df.quotas.nonint,
    q.names = quota.names,
    g.target = global.target,
    method = "solver", 
    panel = adjusted.availability(panel = df.panel),
    initial.values = starting.params[s], #try also ideal or panel.proportional
    verbose = "Partial")
  #Calculate prob succ
  prob.succ[s] = estimate.fieldwork(
    panel = df.panel,
    quotas = df.q.list[[s]],
    q.names = quota.names,
    g.target = global.target
  )
}

#Select the best one. In case of a tie, select the first one
best.starting = (1:length(starting.params))[prob.succ == max(prob.succ)][1]
print("Probabilities of success:")
print(prob.succ)
print(paste("We use the initialization method:",starting.params[best.starting]))
df.q.int = df.q.list[[best.starting]]

#Simulate fieldworks
df.simulation = simulate.fieldwork(
  scn = "Interlocking",
  type = "binomial", #faster than general, but quotafulls are not estimated
  s = num.sim.int, 
  panel = df.panel,
  quotas = df.q.int,
  q.names = quota.names,
  g.target = global.target,
  summary = df.simulation)

#3.Original scenario transformed to interlocking, flex +1 in 2 cells ############

#Average RR for the quota transformation
df.panel.available = adjusted.availability(
  panel = df.panel,
  cert = 0.6)

df.q.int.flex2 = add.flexibility(
  quotas = df.q.int,
  panel = df.panel.available,
  nflex = 2)

#Simulate fieldwork
df.simulation = simulate.fieldwork(
  scn = "Interlocking flex 2",
  type = "binomial",
  s = num.sim.int, 
  panel = df.panel,
  quotas = df.q.int.flex2,
  q.names = quota.names,
  g.target = global.target,
  summary = df.simulation)

#4.Original scenario transformed to interlocking, flex +1 in 5 cells ############

#Average RR for the quota transformation
df.panel.available = adjusted.availability(
  panel = df.panel,
  cert = 0.6)

df.q.int.flex5 = add.flexibility(
  quotas = df.q.int,
  panel = df.panel.available,
  nflex = 5)

#Simulate fieldwork
df.simulation = simulate.fieldwork(
  scn = "Interlocking flex 5",
  type = "binomial",
  s = num.sim.int, 
  panel = df.panel,
  quotas = df.q.int.flex5,
  q.names = quota.names,
  g.target = global.target,
  summary = df.simulation)

#5.Original scenario transformed to interlocking, flex +1 in 10 cells ############

#Average RR for the quota transformation
df.panel.available = adjusted.availability(
  panel = df.panel,
  cert = 0.6)

df.q.int.flex10 = add.flexibility(
  quotas = df.q.int,
  panel = df.panel.available,
  nflex = 10)

#Simulate fieldwork
df.simulation = simulate.fieldwork(
  scn = "Interlocking flex 10",
  type = "binomial",
  s = num.sim.int, 
  panel = df.panel,
  quotas = df.q.int.flex10,
  q.names = quota.names,
  g.target = global.target,
  summary = df.simulation)


#Global results ################################################################

#Summary of results per method if only one omnibus is tried
summary.scenarios = df.simulation %>% 
  mutate(completes.fail=ifelse(completes>=global.target,NA,completes)) %>% 
  group_by(i,scenario) %>%  
  summarise(
    `%P(success)`=100*mean(completes>=global.target),
    `Avg(size)`=mean(completes),
    `%Avg(size)`=100*mean(completes)/global.target,
    `Avg(size|fail)`=mean(completes.fail,na.rm=TRUE),
    `%Avg(size|fail)`=100*mean(completes.fail,na.rm=TRUE)/global.target
    )

summary.scenarios %>% print(n=Inf)

#Export results

#Export winning quota settings
write.table(format.output(df.q.int,quota.names.original,quota.levels.original),paste0(code,"_crossed.csv"),fileEncoding = "WINDOWS-1252",sep=";",row.names = FALSE)
write.table(format.output(df.q.int.flex2,quota.names.original,quota.levels.original),paste0(code,"_crossed_flex2.csv"),fileEncoding = "WINDOWS-1252",sep=";",row.names = FALSE)
write.table(format.output(df.q.int.flex5,quota.names.original,quota.levels.original),paste0(code,"_crossed_flex5.csv"),fileEncoding = "WINDOWS-1252",sep=";",row.names = FALSE)
write.table(format.output(df.q.int.flex10,quota.names.original,quota.levels.original),paste0(code,"_crossed_flex10.csv"),fileEncoding = "WINDOWS-1252",sep=";",row.names = FALSE)

save.image("finalSimulation.RData")
