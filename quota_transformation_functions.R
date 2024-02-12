#FUNCTIONS FOR QUOTA TANSFORMATIONS AND SIMULATIONS
#Author: Carlos Ochoa
#Version: 1.1.0

#Last changes: 
#Separate rounding functions from crossing functions
#Optimitzation of distance

#Backlog:
# Implementing other formats for the quota file
# Extract checks quota formats vs panel file from the read.quotas function
# Normalize.quotas is different from previous version, check that it works


#Packages ######################################################################
library(NlcOptim)
library(readxl)
library(tidyverse)
library(data.table)

#Quota formats #################################################################

#Quota settings can be defined using three different formats

#1. Long format:
# Pairs of columns quota-level + n
# If number of pairs == 1 -> non-interlocking
# If number of pairs > 1 -> interlocking or semi-interlocking
#     if the variable in each quota column is always the same -> interlocking

#Example (non-interlocking quota)
#quota    level     n
#gender   male      450
#gender   female    550
#age      18-24     150
#age      25-34     450
#age      35+       400
#region   North     600
#region   South     400

#Example (semi-interlocking quota)
#quota1   level1    quota2   level2     n
#gender   male      region    North     350
#gender   male      region    South     150
#gender   female    region    North     400
#gender   female    region    South     100
#age      18-24     region    North     150
#age      18-24     region    South     100
#age      25-34     region    North     350
#age      25-34     region    South     100
#...

#Example (interlocking quotas)
#quota1   level1    quota2   level2     quota3   level3     n
#gender   male      age       18-24     region    North     50
#gender   male      age       18-24     region    South     50
#gender   male      age       25-34     region    North     50
#gender   male      age       25-34     region    South     50
#gender   male      age       35+       region    North     50
#gender   male      age       35+       region    South     50
#gender   female    age       18-24     region    North     50
#gender   female    age       18-24     region    South     50
#gender   female    age       25-34     region    North     50
#gender   female    age       25-34     region    South     50
#gender   female    age       35+       region    North     50
#gender   female    age       35+       region    South     50

#2. Wide format
# A column for each variable + n
# If there are no * -> Interlocking
# If only 1 value different from * per row -> Non-interlocking
# If >1 value different from * per row and at least one * -> Semi-interlocking

#Example (non-interlocking quota)
#gender     age       region    n
#male       *         *         450
#female     *         *         550
#*          18-24     *         150
#*          25-34     *         450
#*          35+       *         400
#*          *         North     600
#*          *         South     400

#Example (semi-interlocking quota)
#gender     age       region    n
#male       *         North     350
#male       *         South     150
#female     *         North     400
#female     *         South     100
#*          18-24     North     150
#*          18-24     South     100
#*          25-34     North     350
#*          25-34     South     100
#...

#Example (interlocking quotas)
#gender     age       region     n
#male       18-24     North     50
#male       18-24     South     50
#male       25-34     North     50
#male       25-34     South     50
#male       35+       North     50
#male       35+       South     50
#female     18-24     North     50
#female     18-24     South     50
#female     25-34     North     50
#female     25-34     South     50
#female     35+       North     50
#female     35+       South     50

#Wide normalized
# Normalized wide format per groups of variables = one quota setting for each combination of variables
# If only one group -> interlocking
# If several groups with one variable per group -> non-interlocking
# If several groups with >1 variable per group -> semi-interlocking

#Example (non-interlocking quota)
#gender     n
#male       450
#female     550

#age        n
#18-24      150
#25-34      450
#35+        400

#region     n
#North      600
#South      400

#Example (semi-interlocking quota)
#gender     region    n
#male       North     350
#male       South     150
#female     North     400
#female     South     100

#age        #region   n
#18-24      North     150
#18-24      South     100
#25-34      North     350
#25-34      South     100
#...

#Example (interlocking quotas) = same as before, only one setting
#gender     age       region     n
#male       18-24     North     50
#male       18-24     South     50
#male       25-34     North     50
#male       25-34     South     50
#male       35+       North     50
#male       35+       South     50
#female     18-24     North     50
#female     18-24     South     50
#female     25-34     North     50
#female     25-34     South     50
#female     35+       North     50
#female     35+       South     50



#Functions #####################################################################
read.panel = function(
    #Read panel availability
  f #file in CSV format or Excel format
  ) {
  
  #Read file
  extension = tools::file_ext(f)
  #Verificar si es un archivo CSV
  if (tolower(extension) == "csv") {
    df.panel = read.csv(f,sep=";",dec=",")
  } else if (tolower(extension) %in% c("xls", "xlsx")) {
    df.panel = read_excel(f)
  } 
  
  #Clean spaces
  df.panel = df.panel %>% 
    mutate(across(everything(),~ trimws(.x)))
  #read meta data
  num.vars = ncol(df.panel) - 2 #All but participants and RR
  #Preserve the original order of variables and levels
  quota.names.original = names(df.panel)[1:num.vars] #Variable names in original order
  quota.levels.original = lapply(df.panel[,1:num.vars], function(x) {unique(x)})
  #Reorder names and levels in alphabetical order
  quota.names = quota.names.original[order(quota.names.original)] 
  quota.levels = quota.levels.original[quota.names]
  quota.levels = lapply(quota.levels, function(x) {x[order(x)]})
  #Reorder dataset
  df.panel = df.panel %>% 
    select(all_of(quota.names),everything()) %>%  #Put var names in alphabetical order
    arrange_at(quota.names) #Order levels, in the right order
  #Convert to numeric
  df.panel = df.panel %>% mutate(across(all_of(c("participants","RR")),as.numeric))
  return(list(
    df.panel=df.panel,
    num.vars=num.vars,
    quota.names.original=quota.names.original,
    quota.levels.original=quota.levels.original,
    quota.names=quota.names,
    quota.levels=quota.levels))
}

read.quotas = function(
  #Read quota setting
  #out: return a normalized quota setting (format wide normalized = list with one data frame per crossed setting)
  f, #file name of an excel file
  form, #format of the file -> wide, long... (so far, only wide format is supported)
  q.names, #variable names expected, according to the panel file) 
  q.levels #variable with the expected levels
) {

  #Read file
  extension = tools::file_ext(f)
  #Verificar si es un archivo CSV
  if (tolower(extension) == "csv") {
    data = read.csv(f,sep=";",dec=",")
  } else if (tolower(extension) %in% c("xls", "xlsx")) {
    data = read_excel(f)
  } 
  
  #Clean spaces
  data = data %>% 
    mutate(across(everything(),~ trimws(.x)))
  
  #Depending on the format
  if (form == "wide") {
    #Check: names must be the same in the data and in the quota setting
    var.names = names(data)[-length(names(data))]
    var.names = var.names[order(var.names)]
    if (length(var.names) != length(q.names)) {
      stop("Number of variables in the panel and the quota file are not of the same")
    }
    if (sum(var.names != q.names) != 0) {
      stop("Variables in the panel and the quota file are different")
    }
    #Reorder columns and reorder rows, return result
    data = data %>%
      select(all_of(q.names), everything()) %>%
      arrange_at(q.names)
    #Check levels
    levels.quotas = lapply(data[,-length(names(data))], function(x) {unique(x)[unique(x)!="*"]})
    levels.quotas = levels.quotas[q.names]
    levels.quotas = lapply(levels.quotas, function(x) {x[order(x)]})
    levels.match = sapply(1:length(q.levels), function(x) {sum(levels.quotas[[x]] != q.levels[[x]])})
    if (sum(levels.match) != 0) {
      stop(paste(
        "The levels of these quota variables in the panel file do not match the levels in the quota file:",
        q.names[levels.match!=0]
        ))
    }
    #Return a normalized form
    data = normalize.quotas(data)
  } else if (form == "long") {
    stop("Format not supported (yet)")
  } else {
    stop("Format not supported")
  }
  return(data)
}

normalize.quotas = function(
    #Divide in different quota settings (a list of quota settings)
  #If there is only one quota setting, this manipulation works as well, but
  #the quota setting is always save into a list (with 1 element only, in this case)
  df.q #a quota setting in wide not-normalized format
  #out: return a LIST with a normalized quota setting
) {
  # In: df.q is 
  # Out: a list of wide normalized quota settings
  out = df.q %>%
    rowwise() %>% 
    mutate(across(-c("n"),~ ifelse(.x=="*",0,1),.names = "new_{.col}")) %>%
    mutate(n=as.numeric(n)) %>% 
    unite(key,contains("new"),sep="_") %>% 
    group_split(key) %>% 
    lapply(function(x) {x %>% select(-where(~all(. == "*")),-key) %>% as.data.frame()})  
  q.names = sapply(out, function(x) {names(x)[1]})
  out = out[ (1:length(out))[order(q.names)] ]
  return(out)
}

get.sample.size = function(
  #Returns the total sample size requested by a quota setting
  df.q #a normalized quota setting
  #out: the sample size, obtained as the sum of n of the first subquota setting
) {
  return(df.q[[1]] %>% 
           group_by_at(1) %>% 
           summarise(n=sum(n)) %>% 
           summarise(n=sum(n)) %>% pull()
  )
}

quota.tolong = function(
    #Convert a wide and normalized quota setting into a long format
  df.q #a normalized quota setting
  #out:long quota setting
) {
  #Create an empty long version
  out = data.frame(quota=c(),level=c(),n=c())
  #Fill the information for each subquota inside the normalized quota setting
  for (i in 1:length(df.q)) {
    out = rbind(
      out,
      df.q[[i]] %>% 
        mutate(quota=names(df.q[[i]])[1],.before=1) %>% 
        rename(level=!!sym(names(df.q[[i]])[1]))
    )
  }  
  return(out)
}

interlock = function(
  #Transform a non-interlocking quota setting into an equivalent interlocking setting
  df.q.nonint, #Original non-interlocking quota setting, using a normalized format
  q.names, #Names of the variables used in the quotas
  g.target, #Desired number of completes, could not match the information in the quota setting if using flexible quotas
  method = "solver", #Method used to transform non-interlocking into interlocking quotas
  panel = NA, #A data frame with the number of available panelists for each combination of quota variables
  rounded = TRUE, #Round cells targets keeping the global target
  initial.values = "ideal", #Ideal = product of proportions of the quotas, #Unit = all cells equals 1, #proportional = cells proportional to panel availability
  verbose = "No" #"No"=No feedback but faster, "Partial" returns how the distance evolves and "Full" return also the quota values
) {
  
  #Extract variables and levels from the quota setting...
  names.vect = c()
  levels.list = list()
  for (qs in 1:length(df.q.nonint)) {
    names = names(df.q.nonint[[qs]])[-length(names(df.q.nonint[[qs]]))]
    for (n in names) {
      if (!(n %in% names.vect)) {
        names.vect = c(names.vect,n)
        levels.list[[length(levels.list)+1]] = unique(df.q.nonint[[qs]][,n])
      }
    }
  }
  names(levels.list) = names.vect
  
  #... and build the interlocking setting
  
  #If the original quota setting is non-interlocking (but NOT semi-interlocking)
  if (initial.values=="ideal") { #All sub-quotas have 2 columns, that is, one variable
    
    if (mean(sapply(df.q.nonint,function(l) {ncol(l)==2})) != 1) stop("The ideal initialization of quotas is only possible for non-semi-interlocking quota settings")
    
    # set the initial values to the result of calculating the product of the individual frequencies
    df.q.nonint.freq = lapply(df.q.nonint,function(x) {x %>% mutate(n=n/sum(n))})
    df.cross.n = cbind(
      expand.grid(lapply(df.q.nonint.freq,function(x) {x[,-2]})),
      expand.grid(lapply(df.q.nonint.freq,function(x) {x[,2]}))
    )
    names(df.cross.n) = c(q.names,paste0("f",1:length(q.names)))
    df.cross.n = df.cross.n %>% 
      rowwise() %>% 
      mutate(f = prod(c_across(all_of(paste0("f",1:length(q.names)))))) %>% 
      ungroup() %>% 
      mutate(n=f*g.target) %>% 
      select(-all_of(contains("f"))) %>% 
      as.data.frame()
  }
  
  
  if (initial.values=="proportional") { 
    #Set the initial values to the panel available, to promote having more panelists tha are easy
    df.cross.n = expand.grid(levels.list) %>% 
      left_join(panel %>% rename(n=participants)) %>% 
      ungroup() %>% 
      mutate(n = n*global.target/sum(n))
  }
  
  if (initial.values=="unit") { 
    #Put the targets to 1
    df.cross.n = expand.grid(levels.list)
    df.cross.n = df.cross.n %>% 
      select(all_of(names.vect[order(names.vect)])) %>% 
      arrange_at(names.vect[order(names.vect)]) %>% 
      mutate(n=1) %>% #Initial values 
      as.data.frame()
  }
  
  #For each method
  if (method=="solver") {
    
    #Transformation matrix: transform de individual cells into the non-crossed quotas efficiently
    quota.trans = matrix(NA,nrow = 0,ncol = nrow(df.cross.n))
    for (i in 1:length(levels.list)) {
      for (j in 1:length(levels.list[[i]])) {
        quota.trans = rbind(quota.trans, as.numeric(df.cross.n[,i] == levels.list[[i]][j]))
      }
    }
    
    #Add to the interlocking setting a column with available panelists + the ratio "required panelists"/"available panelists" (called IR)
    df.cross.n = df.cross.n %>%
      left_join(panel) %>% 
      mutate(participants=replace_na(participants,0))

    #Initialize the solver. The goal is to find "x", that is the "n" column of the interlocking quota setting
    
    #Initial coefficients
    x = df.cross.n$n
    
    #Non-crossed quotas linealized
    noncrossed.targets = c()
    for (i in 1:length(df.q.nonint)) noncrossed.targets = c(noncrossed.targets,df.q.nonint[[i]]$n)
    noncrossed.targets = matrix(noncrossed.targets)    
    
    if (verbose == "Full") {
      #Target function to be minimized by the solver
      obj = function(x) {
        #Transform into non-interlocking quotas and calculate rmse
        dist = mean((quota.trans %*% matrix(x) - noncrossed.targets)^2)
        #Minimize distance
        print(paste("dist",dist))
        print(as.vector(x))
        return(dist)
      }
    }
    else if (verbose == "Partial") {
      #Target function to be minimized by the solver
      obj = function(x) {
        #Transform into non-interlocking quotas and calculate rmse
        dist = mean((quota.trans %*% matrix(x) - noncrossed.targets)^2)
        #Minimize distance
        print(paste("dist",dist))
        return(dist)
      }
    } else {
      #Target function to be minimize by the solver
      obj = function(x) {
        #Transform into non-interlocking quotas and calculate rmse
        dist = mean((quota.trans %*% matrix(x) - noncrossed.targets)^2)
        return(dist)
      }     
    }

    #Constraints for the solver
    con = function(x) {
      f = NULL
      f = rbind( #I use this strange way to set up the constraints to allow for more than one constraint, see documentation of the solver
        f,
        sum(x) - g.target #The sum of the cells cannot be larger than the required sample size
      )
      return(list(ceq=NULL,c=f)) 
    }
    
    #Limits of the cells
    lower_bounds <- rep(0,length(x)) #Cells must be >= 0 ...
    upper_bounds <- df.cross.n$participants #--- and <= available panelists.
    
    #Execute the solver and save the result as the target of the interlocking quota setting
    out = solnl(x, objfun = obj, confun = con, lb = lower_bounds, ub = upper_bounds)
    df.cross.n = df.cross.n %>% mutate(n=as.vector(out$par))
    
    #Round the solution considering the availability of panelists
    if (rounded) {
      print("Done! Rounding cells...")
      df.cross.n = round.quotas(
        df.q.nonint = df.q.nonint,
        df.q.int = list(df.cross.n %>% select(-participants)),
        df.panel = df.cross.n %>% select(-n),
        target.size = g.target,
        verbose = (verbose != "No")
      )
    } else {
      #Clean columns and inserto into a list...
      df.cross.n = list(df.cross.n %>% 
        select(all_of(q.names),n) %>% 
        as.data.frame())
    }
  }
  if (method=="direct_proportions") {
    #Do nothing except inserting the setting into a list
    df.cross.n = list(df.cross.n)
  }
  if (method=="representativity-optimal") {
    
    #Add to the interlocking setting a column with available panelists + the ratio "required panelists"/"available panelists" (called IR)
    df.cross.n = df.cross.n %>%
      left_join(panel) %>% 
      mutate(participants=replace_na(participants,0))
    
    #Round the solution considering the availability of panelists
    if (rounded) {
      print("Done! Rounding cells...")
      df.cross.n = round.quotas(
        df.q.nonint = df.q.nonint,
        df.q.int = list(df.cross.n %>% select(-participants)),
        df.panel = df.cross.n %>% select(-n),
        target.size = g.target,
        verbose = (verbose != "No")
      )
    } else {
      df.cross.n = list(df.cross.n)
    }
  }
  #Return the result
  return(df.cross.n)
}

round.quotas = function(
    df.q.nonint,
    df.q.int, #An interlocking quota setting not rounded  (within a list)
    df.panel, #Number of expected participants per interlocking cell
    target.size, #The sample size to be reached in the rounding process
    verbose = FALSE
    #out: a rounded quota setting
) {
  
  #Extract variables and levels from the quota setting...
  names.vect = c()
  levels.list = list()
  for (qs in 1:length(df.q.nonint)) {
    names = names(df.q.nonint[[qs]])[-length(names(df.q.nonint[[qs]]))]
    for (n in names) {
      if (!(n %in% names.vect)) {
        names.vect = c(names.vect,n)
        levels.list[[length(levels.list)+1]] = unique(df.q.nonint
                                                      [[qs]][,n])
      }
    }
  }
  names(levels.list) = names.vect
  
  #Non-crossed quotas linealized
  noncrossed.targets = c()
  for (i in 1:length(df.q.nonint)) noncrossed.targets = c(noncrossed.targets,df.q.nonint[[i]]$n)
  noncrossed.targets = matrix(noncrossed.targets)  
  
  #Merge the interlocking quota setting and the panel
  df.q = df.q.int[[1]] %>%
    left_join(df.panel) %>% 
    mutate(participants=replace_na(participants,0))
  
  #Separate integer part from decimal part and arrange by decimal part (closer to the upper number first)
  df.q = df.q %>%
    mutate(pos=1:nrow(df.q)) %>%  #Create a position variable to preserve the original order
    mutate(integ=floor(n),deci=round(n,1)-integ) %>% #Split the integer part from the decimal part, with one decimal
    arrange(-deci) %>%  #Order by the decimal part: larger decimal parts first
    mutate(IR=integ/participants) #IR indicated the difficulty to get participants, IR large is bad
  
  #Current size of the quota setting considering the integer parts
  current.size = df.q %>% summarise(sum(integ)) %>% pull()
  
  #Initialize values for an iterative process
  row = 1
  num.rows = nrow(df.q)
  
  #Logic value that indicates which rows in the quota setting can be increased (initially, all rows can be modified)
  #I do this to avoid checking rows that are no longer available
  valid.rows = rep(TRUE,nrow(df.q))
  
  #Initialy we don not allow to exceed the number of panelists available, but if we complete a loop over all the rows
  #we change this flag and we allow asking for more panelists than availabe. In this cases, flexibility could make this setting
  #useful
  exceed.participants = FALSE
  
  #Iterative process...
  #Loop while the sample size is less than required
  while(current.size < target.size) {
    #We record the current size to compare with the size after each iteration, to detect if we have completed a loop without increasing the sample size
    last.size = current.size
    #For each row in the quota setting
    for (row in (1:num.rows)[valid.rows]) {
      #Can we increase this row? Initially, NO
      increase.row = FALSE
      #If we don't need to exceed available panelists, increase the row if it does not exceed participants
      if (!exceed.participants) {
        if ( df.q[row,"integ"] + 1 <= df.q[row,"participants"] ) {
          increase.row = TRUE #Indicate that we can increase
        }
      } else { #If there is no other option, increase participants even if we exceed available panelists
        print("Exceeding")
        increase.row = TRUE #Indicate that we can increase
      }
      if (increase.row) { #We can increase this row
        df.q[row,"integ"] = df.q[row,"integ"] + 1 #Increase
        
        #Check if we are within the quota limits
        
        #Transformation matrix: transform the individual cells into the non-crossed quotas efficiently
        quota.trans = matrix(NA,nrow = 0,ncol = nrow(df.q))
        for (i in 1:length(levels.list)) {
          for (j in 1:length(levels.list[[i]])) {
            quota.trans = rbind(quota.trans, as.numeric(df.q[,i] == levels.list[[i]][j]))
          }
        }
        
        #Check if the quotas are ok (equal or below the limits)
        quota.passed = sum((noncrossed.targets - quota.trans %*% matrix(df.q$integ)) < 0) == 0
        
        #If we have over the quotas, roll back
        if (!quota.passed) {
          #Roll back
          df.q[row,"integ"] = df.q[row,"integ"] - 1
          #Do not check this row any more
          valid.rows[row] = FALSE
        } else {
          #Update size
          current.size = current.size + 1
          #If we have reached the required sample size, break the loop
          if (current.size == target.size) row=num.rows #End
        }
      }
      #Debug info
      if (verbose) {
        print(paste("sample size",current.size))
        print(paste("row",row))
      }
    }
    #The first time we complete an iteration withaout increase the sample size, allow exceeding participants
    if (!exceed.participants & (current.size == last.size)) {
      print("The quotas must exceed the participants, sorry!")
      exceed.participants = TRUE
    }
  }
  #Clean the quota setting, removing not needed columns and insert into a list
  df.q = list(df.q %>% 
    arrange(pos) %>%
    select(-n,-pos,-deci,-IR,-participants) %>% 
    rename(n=integ) %>% 
    as.data.frame())
  
  return(df.q)
}


simulate.fieldwork = function(
  #Simulate the execution of the fieldwork
  scn,  #scn: description of the scenario
  type, #type of simulation: general will work for all quota settings, crossed only for strict crossed settings, prioritzed
  s=10, #number of simulations
  panel, #panel available for the simulation
  quotas, #quota setting
  q.names, #names of the quota variables
  g.target, #targeted number of completes
  summary, #data frame where the results will be saves
  prioritized.quotas, #targets to be prioritized in the execution of the fieldwork
  simulation.mode = "efficient", #if efficient, consider that after the 1st quota-full, future quota fulls are discarded
  save.logs = FALSE #Save traces about the simulation
) {
  
  #Assign index number to the simulation
  ind = ifelse(nrow(summary)==0,1,max(summary$i) + 1)
  
  #Remove factors and ensure the panel data is a regular data frame to improve speed
  panel = panel %>% mutate(across(-participants,as.character)) %>% as.data.frame()
  
  if (type=="binomial") {
    
    if (length(quotas)>1) stop ("The binomial simulation is only available for interlocking quota settings")
    
    df.sim = quotas[[1]] %>% 
      left_join(panel)
    
    #Simulate n times
    for (sim in 1:s) {
      
      #Simulate how many panelists will participate using a binomial distribution
      df.sim = df.sim %>%
        rowwise() %>%
        mutate(respondents = rbinom(1, participants, as.numeric(RR))) %>% 
        mutate(sample = min(n,respondents)) %>% 
        ungroup() %>% 
        as.data.frame()
      
      co = df.sim %>% summarise(min(sum(sample),g.target)) %>% pull()
      
      #Calculate the sample available
      sample.size = df.sim %>% summarise(sum(participants)) %>% pull()

      #Final quota composition (for reporting)
      df.q.final = list(df.sim %>% select(all_of(q.names),n.final=sample))

      #Log
      summary = rbind(
        summary,
        data.frame(
          i=ind,
          scenario=scn,
          sim=sim,
          sample=sample.size,
          completes=co,
          quotafulls=NA,          
          pending=g.target-co,
          logs=NA
        )
      )
      #Message
      print(summary[nrow(summary),1:6])      
    } #end sim
    
  } #End of type =="binomial"
  if (type=="general") {
    
    #Simulate n times
    for (sim in 1:s) {
      
      #We record the sequence of events co and qf
      if (save.logs) logs = c()
      
      #Copy the panel available in a new variable for this simulation, ensuring the order of the columns
      df.panel = panel %>% select(all_of(q.names),participants,RR)
      
      #Simulate how many panelists will participate using a binomial distribution
      df.panel = df.panel %>%
        rowwise() %>%
        mutate(participants = rbinom(1, participants, as.numeric(RR))) %>% 
        select(-RR) %>% #Remove RR, no longer needed 
        ungroup() %>% 
        as.data.frame()
      
      #Calculate the sample available
      sample.size = df.panel %>% summarise(sum(participants)) %>% pull()
      
      #Initialize quota fulls and completes
      co = 0
      qf = 0
      
      #Initialize quotas for this simulation
      df.q = quotas
      
      #Initialize available sample with probabilities of inclusion
      df.sim = df.panel %>% mutate(prop=cumsum(participants)/sum(participants))
      
      #While there are participants and there are open quotas
      while(co<g.target & nrow(df.sim)>0 & sum(df.sim$participants)>0 & sum(df.q[[1]]$n)>0) { 
        
        #Select randomly an individual using probabilities of inclusion
        selection = sum(df.sim$prop<=runif(1))+1
        individual = df.sim[selection,q.names,drop=FALSE] 
        
        #Are all the quotas open for this individual?
        open.quota = TRUE
        
        #Find which quota/s row/s are affected by this panelist
        #For each quota set
        quota.selection = list() #list of quota-rows affected by the panelist, for each quota set
        for (qs in 1:length(df.q)) {
          quota.selection[[qs]] = rep(TRUE,nrow(df.q[[qs]]))  
          #For each quota variable (removing "n")
          for (q.namesame in names(df.q[[qs]])[-length(names(df.q[[qs]]))]) {
            quota.selection[[qs]] = quota.selection[[qs]] & (df.q[[qs]][,q.namesame] == individual[,q.namesame] %>% as.character())
          }
          #Participant is valid if its quota is open for all the quota settings
          open.quota = open.quota & (df.q[[qs]][quota.selection[[qs]],"n"])>0
          if (!open.quota) qs=length(df.q) #Stop checking quotas for this individual
        }
        #If quota for this individual is open
        if (open.quota) {
          #Adjust quotas
          for (qs in 1:length(df.q)) {
            df.q[[qs]][quota.selection[[qs]],"n"] = df.q[[qs]][quota.selection[[qs]],"n"] - 1
          }
          #Reduce sample availability
          df.sim[selection,"participants"] = df.sim[selection,"participants"] - 1
          #Update completes
          co = co + 1
          #Update logs
          if(save.logs) logs = c(logs,"co")          
          #If quota full
        } else {
          #Update quota full
          qf = qf + 1
          #Update logs
          if(save.logs) logs = c(logs,"qf")
          
          if (simulation.mode == "efficient") {
            #In the efficent mode, remove panelists like this from the available panel          
            df.sim = df.sim[-selection,]
          } else {
            #In the slow mode, reduce participants by 1
            df.sim[selection,"participants"] = df.sim[selection,"participants"] - 1
          }
        } #If CO vs QF
        #Recalculate inclusion probabilities
        df.sim = df.sim %>% mutate(prop=cumsum(participants)/sum(participants))
      } #End while
      
      #Final quota composition (for reporting)
      df.q.final = quotas
      for (qs in 1:length(df.q.final)) {
        df.q.final[[qs]] = suppressMessages(df.q.final[[qs]] %>% 
                                              rename(n.target = n) %>% 
                                              left_join(
                                                df.q[[qs]] %>% 
                                                  rename(n.missing = n)
                                              ) %>% mutate(
                                                n.final = n.target - n.missing
                                              ) %>% select(-n.target,-n.missing))
      } 
      #Log
      summary = rbind(
        summary,
        data.frame(
          i=ind,
          scenario=scn,
          sim=sim,
          sample=sample.size,
          completes=co,
          quotafulls=qf,          
          pending=g.target-co,
          logs=ifelse(save.logs , paste0(logs,collapse="_"), "")
        )
      )
      #Message
      print(summary[nrow(summary),1:6])      
    } #end sim
    
  } #End of type ==" general"
  if (type=="prioritized") {
    
    #Simulate n times
    for (sim in 1:s) {
      
      #Select data
      df.panel = panel %>% select(all_of(q.names),participants,RR)
      
      #Simulate how many panelists will participate using a binomial distribution
      df.panel = df.panel %>%
        rowwise() %>%
        mutate(participants = rbinom(1, participants, as.numeric(RR))) %>% 
        select(-RR) %>% 
        ungroup() %>% 
        as.data.frame()
      
      #Calculate the sample available
      sample.size = df.panel %>% summarise(sum(participants)) %>% pull()
      
      #Sample size available in this try
      sample = df.panel %>% summarise(sum(participants)) %>% pull()
      
      #Initialize quota fulls and completes
      co = 0
      
      #Initialize quotas for this simulation
      df.q = quotas
      
      #Mark which panelists are prioritized
      priorities = prioritized.quotas %>% filter(priority==TRUE)
      selection = rep(FALSE,nrow(df.panel))
      for (pr in 1:nrow(priorities)) {
        selection = selection | (df.panel[,priorities[pr,"quota"]] == priorities[pr,"level"])   
      }
      df.panel = df.panel %>% mutate(priority=selection)
      
      #Initialize available sample with probabilities of inclusion
      df.sim = df.panel %>% mutate(prop=cumsum(participants*priority)/sum(participants*priority))
      
      #While there are participants and there are open quotas
      while(co<g.target & nrow(df.sim)>0 & sum(df.sim$participants)>0 & sum(df.q[[1]]$n)>0) { 
        
        #Select randomly an individual using probabilities of inclusion
        selection = sum(df.sim$prop<=runif(1))+1
        individual = df.sim[selection,q.names] 
        
        #Are all the quotas open for this individual?
        open.quota = TRUE
        
        #Find which quota/s row/s are affected by this panelist
        #For each quota set
        quota.selection = list() #list of quota-rows affected by the panelist, for each quota set
        for (qs in 1:length(df.q)) {
          quota.selection[[qs]] = rep(TRUE,nrow(df.q[[qs]]))  
          #For each quota variable (removing "n")
          for (q.namesame in names(df.q[[qs]])[-length(names(df.q[[qs]]))]) {
            quota.selection[[qs]] = quota.selection[[qs]] & (df.q[[qs]][,q.namesame] == individual[,q.namesame] %>% as.character())
          }
          #Participant is valid if its quota is open for all the quota settings
          open.quota = open.quota & (df.q[[qs]][quota.selection[[qs]],"n"])>0
          if (!open.quota) qs=length(df.q) #Stop checking quotas for this individual
        }
        #If quota for this individual is open
        if (open.quota) {
          #Adjust quotas
          for (qs in 1:length(df.q)) {
            df.q[[qs]][quota.selection[[qs]],"n"] = df.q[[qs]][quota.selection[[qs]],"n"] - 1
          }
          #Reduce sample availability
          df.sim[selection,"participants"] = df.sim[selection,"participants"] - 1
          #Update completes
          co = co + 1
          #If quota full
        } else {
          #Quota full
          #Remove panelists like this from the available panel
          df.sim = df.sim[-selection,]
          #Reverse priorities if prioritzed sample is over
          if (df.sim %>% filter(priority) %>% summarise(sum(participants)) %>% pull()==0) {
            df.sim = df.sim %>% mutate(priority = !priority)
          }
          #Recalculate inclusion probabilities
          df.sim = df.sim %>% mutate(prop=cumsum(participants)/sum(participants))
        } #If pending
      } #End while
      
      #Final quota composition (for reporting)
      df.q.final = quotas
      for (qs in 1:length(df.q.final)) {
        df.q.final[[qs]] = suppressMessages(df.q.final[[qs]] %>% 
                                              rename(n.target = n) %>% 
                                              left_join(
                                                df.q[[qs]] %>% 
                                                  rename(n.missing = n)
                                              ) %>% mutate(
                                                n.final = n.target - n.missing
                                              ) %>% select(-n.target,-n.missing))
      } 
      #Log
      summary = rbind(
        summary,
        data.frame(
          i=ind,
          scenario=scn,
          sim=sim,
          sample=sample.size,
          completes=co,
          pending=g.target-co,
          logs = NA
        )
      )
      #Message
      print(summary[nrow(summary),1:6])       
    } #end sim
    
  }  #End type = "prioritized" 
  return(summary)  
}

estimate.fieldwork = function(
  #Estimate the execution of the fieldwork, for crossed settings only
  panel, #panel available for the simulation
  quotas, #quota setting
  q.names, #names of the quota variables
  g.target #targeted number of completes
) {
  
  #Remove factors and ensure the panel data is a regular data frame to improve speed
  panel = panel %>% mutate(across(-participants,as.character)) %>% as.data.frame()
  
  #Detect flexibility
  if (sum(quotas[[1]]$n) - global.target > 0) stop ("The estimation is not available when there is flexibility")
   
  #Combine quotas and panel availability
  df.sim = quotas[[1]] %>% 
    left_join(panel) 
  
  #Calculate the join probability of completing all the quotas at the same time
  #prob.succ = 
  df.sim = df.sim %>% 
    mutate(
      prob = 1 - pbinom(n-1,participants,as.numeric(RR)),
      prob.log = log(prob)) %>% 
    summarise(
      prob.succ = exp(sum(prob.log))
    )
  
  return(100 * df.sim %>% pull(prob.succ))
}

adjusted.availability = function(
    #Estimate the sample availability per cell for a given certainty. The result must be read
  #as "give me the MINIMUM number of panelists per cell that will participate with certainty "cert"
  #The highest is "cert", the lower the number of participants.
  #participants == 0 are replaced by 0.01 to avoid convergence problems with the solver
  panel, #a data frame with the available panelists: one column per quota + two columns: participants and RR
  cert = 0.8,   #certainty.
  min.availability = 0.01 #If availability is 0 for a cell, it will be replaced by this value
) {
  if ( !(("participants" %in% names(panel)) & ("RR" %in% names(panel)))) stop("The data frame panel must include the columns participants and RR")
  return(
    panel %>%
      rowwise() %>%
      mutate(participants = qbinom(1 - cert, participants, as.numeric(RR))) %>% #1 - cert = low tail
      mutate(participants = ifelse(participants==0,0.01,participants)) %>% 
      select(-RR) %>%
      ungroup() %>% 
      mutate(participants=ifelse(participants==0,min.availability,participants)) %>% 
      as.data.frame()
  )
}

add.flexibility = function(
    #Add flexibility to an interlocking quota setting
  #that means adding +1 to the "nflex" easier cells (=more sample availability)
  quotas, #quota setting (one column per quota + column n)
  panel,  #same structure + column participants
  nflex   #number of cells to increase by +1
) {
  
  if (length(quotas)>1) stop("Only crossed quotas can be flexibilized")
  
  #Order by IR (difificulty) and increase the easier cells
  quotas=quotas[[1]] %>% 
    mutate(pos = 1:nrow(quotas[[1]])) %>% #Initial position
    left_join(panel) %>% 
    mutate(IR=round(n/participants,1)) %>% #IR = % of available panels that are required
    arrange(participants<10,IR) %>%  #Prioritize cells with more than 10 available panelists and low IR
    mutate(priority = 1:nrow(quotas[[1]])) %>% #Priority
    mutate(n=ifelse(priority<=nflex,n+1,n)) %>% 
    arrange(pos) %>%  #Restore thi initial order
    select(-pos,-participants,-IR,-priority)
  
  return(list(quotas))
}

format.output = function(
  #Restore the order of the quota variables and the levels of the variables as they
  #were in the original feasibility file.
  #The input is a one-element list, but the output is a data frame
  df, #a data frame with the crossed quotas
  quota.names, #the quota variables in the original order
  levels.order) { #the levels of each quota variable in the original order
  
  df = df[[1]]
  for (q.namesame in quota.names) {
    df[, q.namesame] = factor(df[, q.namesame], levels = levels.order[[q.namesame]])
  }
  df = df %>% select(all_of(quota.names),everything()) %>% arrange_at(quota.names)
  return(df)
}

create.proportional.invitation = function(
  #Create a sample to be invited that is proportional to the quota targets
  df.q, #Non-interlocking quota setting
  q.names, #Names of the quota variables
  g.target, #Target sample size
  RR, #Response Rate to consider for all panelists groups
  times=4 #A factor that multiplies the available panelists
) {
  
  #Transform quota setting into an optimal unrounded interlocking quota
  df.q.crossed = crossquotas(
    df.q.nonint = df.q, 
    q.names = q.names,
    g.target = g.target,
    method = "unrounded")
  
  #Calculate participants for each cell
  df.part = df.q.crossed[[1]] %>%
    mutate(RR=RR.avg) %>% 
    mutate(participants=ceiling(times*ceiling(n)/RR)) %>% #Two times ceiling to avoid sample 1 with RR<1 in some cells
    select(-n)
}

adjust.size = function(
    #Transform a non-interlocking quota setting to a new size
    quotas, #noninterlocking quota setting
    size #desired size
    ) {
  for (i in 1:length(quotas)) {
    #Keep the order
    quotas[[i]]$pos = 1:nrow(quotas[[i]])
    #Adjust
    quotas[[i]]$n = quotas[[i]]$n*size/sum(quotas[[i]]$n)
    #Separate integers and decimals
    quotas[[i]] = quotas[[i]] %>% mutate(int = floor(n), dec = round(n,3)-int)
    #Order by decimal part
    quotas[[i]] = quotas[[i]] %>% arrange(-dec)
    #Convert to dataframe again to increase speed
    quotas[[i]] = quotas[[i]] %>% as.data.frame()
    #Round up until...
    row = 1
    while(sum(quotas[[i]]$int) < size) {
      quotas[[i]][row,"int"] = quotas[[i]][row,"int"] + 1
      row = row + 1
    }
    #Restore order
    quotas[[i]] = quotas[[i]] %>% arrange(pos)
    #Remove vars
    quotas[[i]] = quotas[[i]] %>% 
      select(-n,-dec,-pos) %>% 
      rename(n=int) %>% 
      as.data.frame()
  } 
  return(quotas)
}
