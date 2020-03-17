# This script peforms the data analysis for the paper on the effect of editorial boards resigning on journal quality

# Loading required packages

library(Synth)

# Reading in the data and building a single dataframe

years <- c('2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009',
           '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018')

main_df <- read.csv('C:/Users/User/Documents/Data/resignations/scimagojr 1999.csv', sep = ';')
colnames(main_df)[9] <- 'Total.Docs.1year'
df_year <- rep(1999,dim(main_df)[1])
main_df$Year <- df_year

for (yr in years) { # Read in additional years and rbind to bottom of main_df
  
  data <- read.csv(paste('C:/Users/User/Documents/Data/resignations/scimagojr ', yr, '.csv', sep = ''), sep = ';')
  colnames(data)[9] <- 'Total.Docs.1year'
  df_year <- rep(as.numeric(yr),dim(data)[1])
  data$Year <- df_year
  print(dim(data))
  main_df <- rbind(main_df, data)
  
}

# Convering ,-type floats to standard decimals

main_df$SJR <- as.numeric(gsub(',', '.', main_df$SJR))
main_df$Cites...Doc...2years. <- as.numeric(gsub(',', '.', main_df$Cites...Doc...2years.))
main_df$Ref....Doc. <- as.numeric(gsub(',', '.', main_df$Ref....Doc.))

# Subset the data set to include only journals

main_df <- main_df[which(main_df$Type == 'journal'),]

# Defining a function to be used for cleaning up the category data

cat_cleaner <- function(cat_name) {
  
  ifelse(substr(cat_name,nchar(cat_name),nchar(cat_name)) == ')', clean_cat <- substr(cat_name,1,nchar(cat_name)-5), clean_cat <- substr(cat_name,1,nchar(cat_name)-4))
  
  return(clean_cat)
  
}

# Using Synth to develop and run the synthetic controls model

# A helper function used to create useful IDs

synth_ID_maker <- function(df) {
  
  uniq <- unique(df$Sourceid)
  new_col <- c()
  
  for (i in 1:dim(df)[1]) {
    
    new_col <- c(new_col, which(matrix(unlist(uniq)) == df$Sourceid[i]))
    
  }
  
  df$ID <- new_col
  
  return(df)
  
}

# A helper function used to balance panels

balanced_panel <- function(df,nyrs,idtop,idlin) {
  
  if (idtop == TRUE) { # truncate data set for Topology because it dies
    
    df <- df[which(df$Year < 2013),]
    
  }
  
  if (idlin == TRUE) { # truncate data set for J K-Theory because it dies
    
    df <- df[which(df$Year < 2018),]
    df <- df[which(df$Year > 2008),]
    
  }
  
  new_df <- df[FALSE,]
  
  for (i in 1:max(df$ID)) {
    
    if (length(which(df$ID == i)) == nyrs) {
      
      new_df <- rbind(new_df, df[which(df$ID == i),])
      
    }
    
  }
  
  new_df <- new_df[c('Title', 'Year', 'SJR', 'H.index', 'Total.Docs.1year', 'Total.Docs...3years.', 'Total.Refs.',
                     'Total.Cites..3years.', 'Citable.Docs...3years.', 'Cites...Doc...2years.', 'Ref....Doc.', 'ID')]
  uniq <- unique(new_df$ID)
  new_col <- c()
  
  for (i in 1:dim(new_df)[1]) {
    
    new_col <- c(new_col, which(matrix(unlist(uniq)) == new_df$ID[i]))
    
  }
  
  new_df$ID <- new_col
  new_df$Title <- as.character(new_df$Title)
  new_df$Year <- as.numeric(new_df$Year)
  new_df$SJR <- as.numeric(new_df$SJR)
  new_df$H.index <- as.numeric(new_df$H.index)
  new_df$Total.Docs.1year <- as.numeric(new_df$Total.Docs.1year)
  new_df$Total.Docs...3years. <- as.numeric(new_df$Total.Docs...3years.)
  new_df$Total.Refs. <- as.numeric(new_df$Total.Refs.)
  new_df$Total.Cites..3years. <- as.numeric(new_df$Total.Cites..3years.)
  new_df$Citable.Docs...3years. <- as.numeric(new_df$Citable.Docs...3years.)
  new_df$Cites...Doc...2years. <- as.numeric(new_df$Cites...Doc...2years.)
  new_df$Ref....Doc. <- as.numeric(new_df$Ref....Doc.)
  new_df$ID <- as.numeric(new_df$ID)
  
  return(new_df)
  
}

# Subset data by categories for each analysis

# International Journal of Solids and Structures

IJSS <- main_df[grepl('Applied Mathematics', main_df$Categories)
                | grepl('Condensed Matter Physics', main_df$Categories)
                | grepl('Materials Science \\(miscellaneous\\)', main_df$Categories)
                | grepl('Mechanical Engineering', main_df$Categories)
                | grepl('Mechanics of Materials', main_df$Categories)
                | grepl('Modeling and Simulation', main_df$Categories),]
IJSS <- synth_ID_maker(IJSS)
IJSS <- balanced_panel(IJSS,20,FALSE,FALSE)

# Canadian Medical Association Journal

CMAJ <- main_df[grepl('Medicine \\(miscellaneous\\)', main_df$Categories),]
CMAJ <- synth_ID_maker(CMAJ)
CMAJ <- balanced_panel(CMAJ,20,FALSE,FALSE)

# Topology

TOP <- main_df[grepl('Geometry and Topology', main_df$Categories),]
TOP <- synth_ID_maker(TOP)
TOP <- balanced_panel(TOP,14,TRUE,FALSE)

# Organization and Environment

OE <- main_df[grepl('Environmental Science \\(miscellaneous\\)', main_df$Categories)
              | grepl('Organizational Behavior and Human Resource Management', main_df$Categories),]
OE <- synth_ID_maker(OE)
OE <- balanced_panel(OE,20,FALSE,FALSE)

# Journal of Library Administration

JLA <- main_df[grepl('Library and Information Sciences', main_df$Categories)
               | grepl('Public Administration', main_df$Categories),]
JLA <- synth_ID_maker(JLA)

# It failed to properly read in JLA data for 2008 so we manually append it here

jla2008 <- c(11184, 4700152795, 'Journal of Library Administration', 'journal',
             '01930826', 0.236, 'Q3', 26, 90, 137, 1854, 50, 133, 0.426, 20.60,
             'United States', 'Haworth Press Inc.', '1980-ongoing',
             'Library and Information Sciences (Q3); Public Administration (Q3)', 2008, 98)
JLA <- rbind(JLA, setNames(jla2008, names(JLA)))
JLA <- balanced_panel(JLA,20,FALSE,FALSE)

# Lingua

LIN <- main_df[grepl('Language and Linguistics', main_df$Categories)
               | grepl('Linguistics and Language', main_df$Categories),]
LIN <- synth_ID_maker(LIN)
LIN <- balanced_panel(LIN,20,FALSE,FALSE)

# Journal of K-Theory

JK <- main_df[grepl('Algebra and Number Theory', main_df$Categories)
              | grepl('Geometry and Topology', main_df$Categories),]
JK <- synth_ID_maker(JK)
JK <- balanced_panel(JK,9,FALSE,TRUE)

# Predictors for Synth

preds.SJR <- c('Total.Docs.1year', 'Total.Docs...3years.', 'Total.Refs.', 'Total.Cites..3years.',
           'Citable.Docs...3years.', 'Cites...Doc...2years.', 'Ref....Doc.')

preds.cites <- c('Total.Docs.1year', 'Total.Docs...3years.', 'Total.Refs.', 'Total.Cites..3years.',
               'Citable.Docs...3years.', 'Ref....Doc.')

# Creating the input data objects for Synth using dataprep() and citaitons predictors

IJSS.synth.data.cites <- dataprep(foo = IJSS, predictors = preds.cites, predictors.op = c('mean'),
                                dependent = c('Cites...Doc...2years.'), unit.variable = c('ID'), time.variable = c('Year'),
                                treatment.identifier = 30, controls.identifier = c(1:29, 31:408), time.predictors.prior = c(1999:2005),
                                time.optimize.ssr = c(1999:2005), unit.names.variable = 'Title', time.plot = c(1999:2018))

CMAJ.synth.data.cites <- dataprep(foo = CMAJ, predictors = preds.cites, predictors.op = c('mean'), dependent = c('Cites...Doc...2years.'),
                                unit.variable = c('ID'), time.variable = c('Year'), treatment.identifier = 309,
                                controls.identifier = c(1:308, 310:699), time.predictors.prior = c(1999:2006),
                                time.optimize.ssr = c(1999:2006), unit.names.variable = 'Title', time.plot = c(1999:2018))

TOP.synth.data.cites <- dataprep(foo = TOP, predictors = preds.cites, predictors.op = c('mean'), dependent = c('Cites...Doc...2years.'),
                              unit.variable = c('ID'), time.variable = c('Year'), treatment.identifier = 4,
                              controls.identifier = c(1:3, 5:27), time.predictors.prior = c(1999:2006),
                              time.optimize.ssr = c(1999:2006), unit.names.variable = 'Title', time.plot = c(1999:2012))

OE.synth.data.cites <- dataprep(foo = OE, predictors = preds.cites, predictors.op = c('mean'), dependent = c('Cites...Doc...2years.'),
                              unit.variable = c('ID'), time.variable = c('Year'), treatment.identifier = 106,
                              controls.identifier = c(1:105, 107:119), time.predictors.prior = c(1999:2012),
                              time.optimize.ssr = c(1999:2012), unit.names.variable = 'Title', time.plot = c(1999:2018))

JLA.synth.data.cites <- dataprep(foo = JLA, predictors = preds.cites, predictors.op = c('mean'), dependent = c('Cites...Doc...2years.'),
                              unit.variable = c('ID'), time.variable = c('Year'), treatment.identifier = 57,
                              controls.identifier = c(1:56), time.predictors.prior = c(1999:2013),
                              time.optimize.ssr = c(1999:2013), unit.names.variable = 'Title', time.plot = c(1999:2018))

LIN.synth.data.cites <- dataprep(foo = LIN, predictors = preds.cites, predictors.op = c('mean'), dependent = c('Cites...Doc...2years.'),
                                 unit.variable = c('ID'), time.variable = c('Year'), treatment.identifier = 59,
                                 controls.identifier = c(1:58, 60:75), time.predictors.prior = c(1999:2015),
                                 time.optimize.ssr = c(1999:2015), unit.names.variable = 'Title', time.plot = c(1999:2018))

JK.synth.data.cites <- dataprep(foo = JK, predictors = preds.cites, predictors.op = c('mean'), dependent = c('Cites...Doc...2years.'),
                                 unit.variable = c('ID'), time.variable = c('Year'), treatment.identifier = 69,
                                 controls.identifier = c(1:68, 70:73), time.predictors.prior = c(2009:2014),
                                 time.optimize.ssr = c(2009:2014), unit.names.variable = 'Title', time.plot = c(2009:2017))

# Creating the input data objects for Synth using dataprep() and SJR predictors

IJSS.synth.data.SJR <- dataprep(foo = IJSS, predictors = preds.SJR, predictors.op = c('mean'), dependent = c('SJR'),
                                unit.variable = c('ID'), time.variable = c('Year'), treatment.identifier = 30,
                                controls.identifier = c(1:29, 31:408), time.predictors.prior = c(1999:2005),
                                time.optimize.ssr = c(1999:2005), unit.names.variable = 'Title', time.plot = c(1999:2018))

CMAJ.synth.data.SJR <- dataprep(foo = CMAJ, predictors = preds.SJR, predictors.op = c('mean'), dependent = c('SJR'),
                                unit.variable = c('ID'), time.variable = c('Year'), treatment.identifier = 309,
                                controls.identifier = c(1:308, 310:699), time.predictors.prior = c(1999:2006),
                                time.optimize.ssr = c(1999:2006), unit.names.variable = 'Title', time.plot = c(1999:2018))

TOP.synth.data.SJR <- dataprep(foo = TOP, predictors = preds.SJR, predictors.op = c('mean'), dependent = c('SJR'),
                               unit.variable = c('ID'), time.variable = c('Year'), treatment.identifier = 4,
                               controls.identifier = c(1:3, 5:27), time.predictors.prior = c(1999:2006),
                               time.optimize.ssr = c(1999:2006), unit.names.variable = 'Title', time.plot = c(1999:2012))

OE.synth.data.SJR <- dataprep(foo = OE, predictors = preds.SJR, predictors.op = c('mean'), dependent = c('SJR'),
                              unit.variable = c('ID'), time.variable = c('Year'), treatment.identifier = 106,
                              controls.identifier = c(1:105, 107:119), time.predictors.prior = c(1999:2012),
                              time.optimize.ssr = c(1999:2012), unit.names.variable = 'Title', time.plot = c(1999:2018))

JLA.synth.data.SJR <- dataprep(foo = JLA, predictors = preds.SJR, predictors.op = c('mean'), dependent = c('SJR'),
                               unit.variable = c('ID'), time.variable = c('Year'), treatment.identifier = 57,
                               controls.identifier = c(1:56), time.predictors.prior = c(1999:2013),
                               time.optimize.ssr = c(1999:2013), unit.names.variable = 'Title', time.plot = c(1999:2018))

LIN.synth.data.SJR <- dataprep(foo = LIN, predictors = preds.SJR, predictors.op = c('mean'), dependent = c('SJR'),
                                 unit.variable = c('ID'), time.variable = c('Year'), treatment.identifier = 59,
                                 controls.identifier = c(1:16, 18:34, 36:58, 60:74), time.predictors.prior = c(1999:2015),
                                 time.optimize.ssr = c(1999:2015), unit.names.variable = 'Title', time.plot = c(1999:2018))

JK.synth.data.SJR <- dataprep(foo = JK, predictors = preds.SJR, predictors.op = c('mean'), dependent = c('SJR'),
                                unit.variable = c('ID'), time.variable = c('Year'), treatment.identifier = 69,
                                controls.identifier = c(1:68), time.predictors.prior = c(2009:2014),
                                time.optimize.ssr = c(2009:2014), unit.names.variable = 'Title', time.plot = c(2009:2017))

# Using Synth to create synthetic controls for each treatment group for citations

IJSS.synth.out.cites <- synth(data.prep.obj = IJSS.synth.data.cites)

CMAJ.synth.out.cites <- synth(data.prep.obj = CMAJ.synth.data.cites)

TOP.synth.out.cites <- synth(data.prep.obj = TOP.synth.data.cites)

OE.synth.out.cites <- synth(data.prep.obj = OE.synth.data.cites)

JLA.synth.out.cites <- synth(data.prep.obj = JLA.synth.data.cites)

LIN.synth.out.cites <- synth(data.prep.obj = LIN.synth.data.cites)

JK.synth.out.cites <- synth(data.prep.obj = JK.synth.data.cites)

# Using Synth to create synthetic controls for each treatment group for citations

IJSS.synth.out.SJR <- synth(data.prep.obj = IJSS.synth.data.SJR)

CMAJ.synth.out.SJR <- synth(data.prep.obj = CMAJ.synth.data.SJR)

TOP.synth.out.SJR <- synth(data.prep.obj = TOP.synth.data.SJR)

OE.synth.out.SJR <- synth(data.prep.obj = OE.synth.data.SJR)

JLA.synth.out.SJR <- synth(data.prep.obj = JLA.synth.data.SJR)

LIN.synth.out.SJR <- synth(data.prep.obj = LIN.synth.data.SJR)

JK.synth.out.SJR <- synth(data.prep.obj = JK.synth.data.SJR)

# Plotting the Synth results for citations

postscript('C:/Users/User/Documents/Data/resignations/IJSS.cites.path.eps')
path.plot(synth.res = IJSS.synth.out.cites, dataprep.res = IJSS.synth.data.cites, Ylab = 'Citations per Document',
          Xlab = 'Year', Legend = c('Intl. J. of Solids and Structures', 'Synthetic Control'),
          Legend.position = 'topright', Ylim = c(0,4))
abline(v = 2005)
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/CMAJ.cites.path.eps')
path.plot(synth.res = CMAJ.synth.out.cites, dataprep.res = CMAJ.synth.data.cites, Ylab = 'Citations per Document',
          Xlab = 'Year', Legend = c('CMAJ', 'Synthetic Control'),
          Legend.position = 'topright', Ylim = c(0,10))
abline(v = 2006)
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/TOP.cites.path.eps')
path.plot(synth.res = TOP.synth.out.cites, dataprep.res = TOP.synth.data.cites, Ylab = 'Citations per Document',
          Xlab = 'Year', Legend = c('Topology', 'Synthetic Control'),
          Legend.position = 'topright', Ylim = c(0,4))
abline(v = 2006)
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/OE.cites.path.eps')
path.plot(synth.res =OE.synth.out.cites, dataprep.res = OE.synth.data.cites, Ylab = 'Citations per Document',
          Xlab = 'Year', Legend = c('Organization & Environment', 'Synthetic Control'),
          Legend.position = 'topleft', Ylim = c(0,10))
abline(v = 2012)
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/JLA.cites.path.eps')
path.plot(synth.res = JLA.synth.out.cites, dataprep.res = JLA.synth.data.cites, Ylab = 'Citations per Document',
          Xlab = 'Year', Legend = c('J. Library Administration', 'Synthetic Control'),
          Legend.position = 'top', Ylim = c(0,2))
abline(v = 2013)
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/LIN.cites.path.eps')
path.plot(synth.res = LIN.synth.out.cites, dataprep.res = LIN.synth.data.cites, Ylab = 'Citations per Document',
          Xlab = 'Year', Legend = c('Lingua', 'Synthetic Control'),
          Legend.position = 'top', Ylim = c(0,2))
abline(v = 2015)
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/JK.cites.path.eps')
path.plot(synth.res = JK.synth.out.cites, dataprep.res = JK.synth.data.cites, Ylab = 'Citations per Document',
          Xlab = 'Year', Legend = c('J. of K-Theory', 'Synthetic Control'),
          Legend.position = 'topright', Ylim = c(0,2))
abline(v = 2014)
dev.off()

# Plotting the Synth results for SJR

postscript('C:/Users/User/Documents/Data/resignations/IJSS.SJR.path.eps')
path.plot(synth.res = IJSS.synth.out.SJR, dataprep.res = IJSS.synth.data.SJR, Ylab = 'SJR Journal Quality Index',
          Xlab = 'Year', Legend = c('Intl. J. of Solids and Structures', 'Synthetic Control'),
          Legend.position = 'topright', Ylim = c(0,4))
abline(v = 2005)
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/CMAJ.SJR.path.eps')
path.plot(synth.res = CMAJ.synth.out.SJR, dataprep.res = CMAJ.synth.data.SJR, Ylab = 'SJR Journal Quality Index',
          Xlab = 'Year', Legend = c('CMAJ', 'Synthetic Control'),
          Legend.position = 'topright', Ylim = c(0,3))
abline(v = 2006)
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/TOP.SJR.path.eps')
path.plot(synth.res = TOP.synth.out.SJR, dataprep.res = TOP.synth.data.SJR, Ylab = 'SJR Journal Quality Index',
          Xlab = 'Year', Legend = c('Topology', 'Synthetic Control'),
          Legend.position = 'topright', Ylim = c(0,4))
abline(v = 2006)
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/OE.SJRs.path.eps')
path.plot(synth.res =OE.synth.out.SJR, dataprep.res = OE.synth.data.SJR, Ylab = 'SJR Journal Quality Index',
          Xlab = 'Year', Legend = c('Organization & Environment', 'Synthetic Control'),
          Legend.position = 'topleft', Ylim = c(0,10))
abline(v = 2012)
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/JLA.SJR.path.eps')
path.plot(synth.res = JLA.synth.out.SJR, dataprep.res = JLA.synth.data.SJR, Ylab = 'SJR Journal Quality Index',
          Xlab = 'Year', Legend = c('J. Library Administration', 'Synthetic Control'),
          Legend.position = 'top', Ylim = c(0,2))
abline(v = 2013)
dev.off()


postscript('C:/Users/User/Documents/Data/resignations/LIN.SJR.path.eps')
path.plot(synth.res = LIN.synth.out.SJR, dataprep.res = LIN.synth.data.SJR, Ylab = 'SJR Journal Quality Index',
          Xlab = 'Year', Legend = c('Lingua', 'Synthetic Control'),
          Legend.position = 'top', Ylim = c(0,2))
abline(v = 2015)
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/JK.SJR.path.eps')
path.plot(synth.res = JK.synth.out.SJR, dataprep.res = JK.synth.data.SJR, Ylab = 'SJR Journal Quality Index',
          Xlab = 'Year', Legend = c('J. of K-Theory', 'Synthetic Control'),
          Legend.position = 'topright', Ylim = c(0,2))
abline(v = 2014)
dev.off()

# Plotting the gaps for citations

postscript('C:/Users/User/Documents/Data/resignations/IJSS.cites.gaps.eps')
gaps.plot(synth.res = IJSS.synth.out.cites, dataprep.res = IJSS.synth.data.cites, Ylab = 'IJSS')
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/CMAJ.cites.gaps.eps')
gaps.plot(synth.res = CMAJ.synth.out.cites, dataprep.res = CMAJ.synth.data.cites, Ylab = 'CMAJ')
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/TOP.cites.gaps.eps')
gaps.plot(synth.res = TOP.synth.out.cites, dataprep.res = TOP.synth.data.cites, Ylab = 'Topology')
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/OE.cites.gaps.eps')
gaps.plot(synth.res = OE.synth.out.cites, dataprep.res = OE.synth.data.cites, Ylab = 'Org. & Environ.')
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/JLA.cites.gaps.eps')
gaps.plot(synth.res = JLA.synth.out.cites, dataprep.res = JLA.synth.data.cites, Ylab = 'J. Library Admin.')
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/LIN.cites.gaps.eps')
gaps.plot(synth.res = LIN.synth.out.cites, dataprep.res = LIN.synth.data.cites, Ylab = 'Lingua')
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/JK.cites.gaps.eps')
gaps.plot(synth.res = JK.synth.out.cites, dataprep.res = JK.synth.data.cites, Ylab = 'J. K-Theory')
dev.off()

# Plotting the gaps for SJR

postscript('C:/Users/User/Documents/Data/resignations/IJSS.SJR.gaps.eps')
gaps.plot(synth.res = IJSS.synth.out.SJR, dataprep.res = IJSS.synth.data.SJR, Ylab = 'IJSS')
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/CMAJ.SJR.gaps.eps')
gaps.plot(synth.res = CMAJ.synth.out.SJR, dataprep.res = CMAJ.synth.data.SJR, Ylab = 'CMAJ')
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/TOP.SJR.gaps.eps')
gaps.plot(synth.res = TOP.synth.out.SJR, dataprep.res = TOP.synth.data.SJR, Ylab = 'Topology')
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/OE.SJR.gaps.eps')
gaps.plot(synth.res = OE.synth.out.SJR, dataprep.res = OE.synth.data.SJR, Ylab = 'Org. & Environ.')
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/JLA.SJR.gaps.eps')
gaps.plot(synth.res = JLA.synth.out.SJR, dataprep.res = JLA.synth.data.SJR, Ylab = 'J. Library Admin.')
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/LIN.SJR.gaps.eps')
gaps.plot(synth.res = LIN.synth.out.SJR, dataprep.res = LIN.synth.data.SJR, Ylab = 'Lingua')
dev.off()

postscript('C:/Users/User/Documents/Data/resignations/JK.SJR.gaps.eps')
gaps.plot(synth.res = JK.synth.out.SJR, dataprep.res = JK.synth.data.SJR, Ylab = 'J. K-Theory')
dev.off()

# Placebo testing


#### note to self : see the regional paper from week 5e(?) for formatting results


























