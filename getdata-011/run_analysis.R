# Code for Getting and Cleaning Data - coursera

####
# Read datasets from training and test sets, merging into one
####

# Setup

library(dplyr)

## source directory information
rootdir <- 'UCI HAR Dataset'
datadirs <- c('train' , 'test')
varfile <- 'features.txt'
varfile <- paste(c(rootdir,varfile),collapse = '/')

# FUNCTIONS #
readvar <- function(varfile) {
    # extracts names from "varfile" and selects attributes for mean and std
    # outputs a list with variable names and index of required attributes
    # variable name is 2nd attribute for each record
    names <- c('id','var')
    varlist <- read.table(varfile,col.names = names)
    varlist <- varlist[,'var']
    pick <- grep('mean',varlist,ignore.case = 'T')
    pick <- c(pick,grep('std',varlist,ignore.case = 'T'))
    list(varlist,sort(pick)) 
}

readfiles <- function(root=character(), t=character(), varlist = list()) {
    # Combine files from each location 't' located under 'root'
    # datafiles are the first item of varlist
    # the second item provides index of required variables

    #####
    # data, activity, and subject files are combined into one datafram
    # file information:
    # data: X_<datadir>.txt
    # activity: y_<datadir>.txt
    # subject: subject_<datadir>.txt

    ##### perform activity ####

    # READ datafile
    # read.table elapsed: train data set: 34.380  test data set: 6.489
    root = paste(root,t,sep = '/')
    fbase = paste('X_',t,'.txt',sep = '')
    f = paste(root,fbase,sep = '/')
    Tdat <- read.table(f,col.names = varlist[[1]])
    Tdat <- select(Tdat,varlist[[2]])

    # READ subject file
    fbase <- paste('subject_',t,'.txt',sep='')
    f <- paste(root,fbase,sep = '/')
    subjects = read.table(f,col.names = c('subject'))

    # READ activity file
    fbase <- paste('y_',t,'.txt',sep='')
    f <- paste(root,fbase,sep = '/')
    activity = read.table(f,col.names = c('activity'))

    # COMBINE three files by column
    # insert subject and activity as first two columns of returned dataframe
    cbind(subjects,activity,Tdat)
}



# Would be nice to check for directory/file existence to provide smooth bailout if error

# get variable names (varlist[[1]]) and index of required attributes (varlist[[2]]) 
 varlist <- readvar(varfile)

# Read files and extract mean and standard deviation for each measurement
#    loop through datadirs and combine in single data.frame
dat <- data.frame()
for (x in datadirs) {
    dat <- rbind(dat,readfiles(rootdir,x,varlist))
}

# sort by subject and activity for cleanliness
dat <- arrange(dat,subject,activity)

# Rename measurements to descriptive variables

# create a second independent tidy data set with the average for each activity and subject

