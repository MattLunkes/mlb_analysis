############################################################
###                  mlb data analysis                   ###
############################################################

# By Matt Lunkes

#### Overview: ######################################## ####

# Step 1: Setup and prep
# Step 2: Create and name data-frames
# Step 3: Perform operations on select tables
# Step 4: Export Data

# Step 1: Setup and prep  ############################# ####
############################################################

# Set WD
setwd("~/Google Drive/mlb_analysis")

# download data from from http://www.baseball-databank.org/
# unzip file (creates "adminDB" folder), place in WD

# Use "http://www.baseball-databank.org/files/BDB-def-2011-03-28.txt" to create "table_definitions.csv"
# Place "table_definitions.csv" file in WD and read in
table_defs <- read.csv("table_definitions.csv", sep = ",")

# Transform columns 2 & 3 into a list of vectors; to be used later to name column headers of ALL tables
field_names <- setNames(split(table_defs$field_name,table_defs$table),unique(table_defs[,2]))
field_names <- sapply(field_names,as.character)

# Create a separate vector with ".csv" appended to each table name (will be used to access files)
table_file_names <- paste(unique(table_defs[,2]), "txt", sep = ".")


# Step 2: Create and name data-frames  ################ ####
############################################################

# Set WD to "adminDB"
setwd("~/Google Drive/mlb_analysis/adminDB")

# Loop through and create data frames, save all to a list ("mlb")
mlb <- lapply(table_file_names,function(x){
  read.csv(x, header=FALSE)
})

# Reset WD
setwd("~/Google Drive/mlb_analysis")

# Use table_defs to name columns in each data.frame
mlb <- lapply(1:length(mlb), function(x){
  setNames(mlb[[x]],field_names[[x]])
})

# Pass back table names to data.frames
names(mlb) <- unique(table_defs[,2])


# Step 3: Perform operations on select tables  ######## ####
############################################################

# For some reason, none of the data includes city or state information
# To fix, we'll create our own df of team/franchise info and manually edit
# Create data.frame
#team_city <- as.data.frame(mlb$Teams[,c("teamID","franchID","name")])
#team_city <- subset(team_city,!duplicated(team_city$teamID))
#row.names(team_city) <- NULL  # Reset index

# Dump-out list
# write.csv(team_city, "team_city.csv")

# Edit as csv file (adding city and state values)
# Re-ingest
#team_city <- read.csv("team_city.csv", header = TRUE)

# Merge city and state info back into "Teams"
mlb$Teams <- merge(mlb$Teams, team_city[,c("teamID","city","state","primary_franchise")], by = "teamID", all.x =  TRUE)

# Add a few columns with clarifying information
mlb$mlb$Teams$yr_tm <- paste(mlb$mlb$Teams$yearID, mlb$mlb$Teams$teamIDBR, sep = " - ")
mlb$mlb$Teams$record <- mlb$mlb$Teams$W / (mlb$mlb$Teams$W + mlb$mlb$Teams$L)
mlb$mlb$Teams$RunDiff <- mlb$mlb$Teams$R - mlb$mlb$Teams$RA

# Make new columns for Pythagorean Analysis
mlb$Teams$RSt <- mlb$Teams$R / mlb$Teams$G
mlb$Teams$RAt <- mlb$Teams$RA / mlb$Teams$G
mlb$Teams$Rt <- mlb$Teams$RSt / mlb$Teams$RAt

# Calculate each team's pythagorean win percentage
mlb$Teams$Win_Pt <- mlb$Teams$Rt^1.83 / (1 + mlb$Teams$Rt^1.83)  

# Calculate the difference btwn actual and predicted win percentage
# mlb$Teams$Actual_vs_Projected <- mlb$Teams$record - mlb$Teams$Win_Pt
# SAVE this for Data Studio

# Create new columns to replace factor "Y"/"N" values for titles
mlb$Teams$Won_Division <- ifelse(mlb$Teams$DivWin == "Y",1,0)
mlb$Teams$Won_Wild_Card <- ifelse(mlb$Teams$WCWin == "Y",1,0)
mlb$Teams$Won_Penant <- ifelse(mlb$Teams$LgWin == "Y",1,0)
mlb$Teams$Won_World_Series <- ifelse(mlb$Teams$WSWin == "Y",1,0)

# Create a "Champions" column to group pre-1903 title-winners with modern-era World Series Champions
mlb$Teams$Champions <- ifelse((mlb$Teams$yearID < 1903 & mlb$Teams$Won_Penant == 1) | mlb$Teams$Won_World_Series == 1,1,0)

# Merge back critical information to each table that we're going to dump out
mlb$Pitching <- merge(mlb$Pitching, mlb$Master[,c("playerID","nameFirst","nameLast","nameNote","nameGiven","nameNick")], by = "playerID", all.x = TRUE)
mlb$Pitching <- merge(mlb$Pitching, team_city[,c("teamID","name","city","state","primary_franchise")], by = "teamID", all.x = TRUE)

mlb$Batting <- merge(mlb$Batting, mlb$Master[,c("playerID","nameFirst","nameLast","nameNote","nameGiven","nameNick")], by = "playerID", all.x = TRUE)
mlb$Batting <- merge(mlb$Batting, team_city[,c("teamID","name","city","state","primary_franchise")], by = "teamID", all.x = TRUE)

mlb$Salaries <- merge(mlb$Salaries, mlb$Master[,c("playerID","nameFirst","nameLast","nameNote","nameGiven","nameNick")], by = "playerID", all.x = TRUE)
mlb$Salaries <- merge(mlb$Salaries, team_city[,c("teamID","name","city","state","primary_franchise")], by = "teamID", all.x = TRUE)

mlb$HallOfFame <- merge(mlb$HallOfFame, mlb$Master[,c("hofID","nameFirst","nameLast","nameNote","nameGiven","nameNick","birthCity","birthState","birthCountry","birthYear","deathYear")], by = "hofID", all.x = TRUE)

# Subset the HoF table to only include inductees
hof_members <- mlb$HallOfFame[mlb$HallOfFame$inducted == "Y",]



# Step 4: Export Data  ################################ ####
############################################################
write.csv(mlb$Teams, "mlb_Teams.csv")

write.csv(mlb$Pitching, "mlb_Pitching.csv")
write.csv(mlb$Batting, "mlb_Batting.csv")
write.csv(mlb$Salaries, "mlb_Salaries.csv")
write.csv(hof_members,"mlb_hof_members.csv")


############################################################
###                       FIN                            ###
############################################################