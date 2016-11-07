
###########################################################################################################################
###
### Importing data into R
### 
###
### 1. Delimited files
### 2. Fixed width text files 
### 3. Variable width text files (e.g. logs )
### 4. SPSS, SAS, STATA formats ('foreign' library)
### 5. SQL statements using database calls

###########################################################################################################################
###
### 1. Delimited files
###
###
### read.csv
### read.delim
### read.table (CAUTION)
###

# setwd("C:\\Users\\Ian\\Dropbox\\ISDS\\ISDS 2015 workshop")

# rm(list=ls(all=TRUE))
# (I NEVER save on exit from R - versioning issues creep in)
# I may save specific R objects which take a long time to create and load these as part of the script

datain<- read.csv(file = "Data\\GI_line_level_data.csv")

### can also use read.delim or read.table, but read.table has some poor default parameter settings (comment.char for one)

names(datain)

datain[1:10, "chief.complaint"]
# why not 
# datain[1:10, 7] or
# datain$chief.complaint[1:10] ??

# 1. If order of columns changes then datain[1:10, 7] may pull wrong column but
# datain[1:10, "chief.complaint"] will not


# 2. Partial name matching with '$'
#    can lead to errors at later stage
# e.g.
cc1<- datain$chief
datain$chief<- 1:nrow(datain)
cc2<- datain$chief

# 3. No errors when matches not found
cc1<- datain$cheif.complaint
# NO ERROR - may be hard to trace 

# compare with
cc2<- datain[, "cheif.complaint"]

### (Using "select" in dplyr solves many of these issues)


### back to read.csv

# Default behavior is to take field names from first row, but names may be altered
names(datain)
# "chief complaint" was changed to "chief.complaint"


# Occasionally this is undesirable. To prevent this use 'check.names' paramter

datain<- read.csv(file = "Data\\GI_line_level_data.csv", check.names = FALSE)
names(datain)
datain[1:10, "chief complaint"]


# Autocoding of fields
str(datain)

# Default behavior is to try and auto code fields
# This can at times be problematic - 
# - for example, numbers which are id's with leading 0's, or ICD9 codes
# - text gets converted into factors regardless of common sense
#
# Alternative is to specify the class of each column ahead of time
# Easiest is to specify them all as 'character' (which does not conversion)
# and then convert them after reading


datain<- read.csv(file = "Data\\GI_line_level_data.csv", 
		  check.names = FALSE,
		  colClasses = "character")

str(datain)
 
 datain[,"age"]<-  as.numeric(datain[,"age"])
 datain[,"date"]<- as.Date(datain[,"date"], format = "%m/%d/%Y")
 datain[,"facility"]<-  factor(datain[,"facility"])
 datain[,"syndrome"]<-  factor(datain[,"syndrome"])
 datain[,"ICD9"]<- 	factor(datain[,"ICD9"])

 ### Note, sometimes it is good practice to explicitly specify the levels for factors, e.g.
 table(datain[,"facility"])

 facilities<- c("123",  "255" , "256",  "259",  "309",   "37",  "390",  "413",  "420",  "522", "6200", "6201",   "66",   "67",  "703", "7298", "667")
 datain[,"facility"]<-  factor(datain[,"facility"], levels = facilities)
 table(datain[,"facility"])
# Note facility "667" now appears as a category in table(datain[,"facility"])


 
###########################################################################################################################
###
### 2. Fixed width text files 
###
###
### read.fwf
### 
### Never had the need to use


 
###########################################################################################################################
###
### 3. Variable width text files (e.g. log files )
### 
###
### scan
###

### The file "Session22 output.txt" contains data output from an external program used to calculate the 
### fractal dimension of a path
 

 scanin<- scan(file = "Data\\Session22 output.txt", 
			sep = "\n", 
			what = character(),
			na.string = NULL)
	
str(scanin)

scanin
 

###########################################################################################################################
###
### 4. SPSS, SAS, STATA formats 
### 
###
### 'foreign' library
###

library('foreign')

datain.spss<- read.spss("Data\\sv08f_hijos.sav",  to.data.frame = TRUE)
datain.stata<- read.dta("Data\\sv08f_hijos.dta")

datain.spss[1:10,1:10]
datain.stata[1:10,1:10]
# Note differences!





###########################################################################################################################
###
### 5. SQL statements using database calls
### 
###
### Example only (from Distribute project)
###

##### DO NOT RUN

##### Function to connect to a particular distribute database																			
##### Notes - requires that the groups distributeRemote and distributeLocal are defined in $HOME/.my.conf 								
#####       - for remote connections requires a ssh tunnel to the mysql server. See "daily_updates.R" for where this is done			

distribute.ConnectToDataBase<- function(dbname, local = DATABASE_LOCAL)
{
	
		
	# access to mysql on boron 
	# requires ssh tunnel to boron to be open and port forwarding
	drv <- dbDriver("MySQL")
	if (!local)
		con<- dbConnect(drv, group = "distributeRemote", dbname = dbname)
	else
		con<- dbConnect(drv, group = "distributeLocal",  dbname = dbname)
	
	
	return(con)
	
}







distribute.getData<- function(con, sql.statement)
{
	
	
	if (exists("rs"))
		dbClearResult(rs)
	


	rs<- dbSendQuery(con, sql.statement)				
	data <- fetch(rs, n = -1)   # extract all rows
	dbClearResult(rs)

	return(data)
}




distribute.getSiteInformation<- function(local = DATABASE_LOCAL, drop.sites = DROP_SITES)
{
			# Get site keys and names
	
			con<- distribute.ConnectToDataBase("isds-flu", local = local)
	
			sql.statement<- paste(		
	 						  "SELECT ID, ShortName, LongName ",
							  "FROM `Sites`"   )
							  	  
			data<- distribute.getData(con, sql.statement)
			dbDisconnect(con)
	
			sel.drop<-  data["ShortName"] %in% drop.sites
			site.info<- data[!sel.drop,]
							  
			return(site.info)
}
	
