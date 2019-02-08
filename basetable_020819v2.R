
if (!require("readtext")) install.packages("readtext", quiet=TRUE) ; library(readtext)

install.packages("data.table")
library(data.table)
library(readtext)
library(readxl)

## import all files in the directory
description <- readtext("C:\\Users\\dkewon\\Documents\\GitHub\\SMA\\SMAGroup1\\Data\\concept_extraction_cvs\\concept_extraction_cvs\\*.txt")

stateabbreviation <- read_excel("~/GitHub/SMA/SMAGroup1/stateabbreviation.xlsx")

#################Location

#getting rid of punctuation and trailing space 
description$text<-gsub("[[:punct:][:blank:]]+", " ", description$text)
description$text<-gsub("\\n", " ", description$text)

#creating a city column
description$city <- NA

#using for loop to extract the cities/states
for (i in 1:length(description$text)){

#split the text by space
split <- strsplit(description$text[i]," ")[[1]] 

# # Find the state abbreviation in the split
location <- match(split, stateabbreviation$Abbreviation)

# # Get the position of the word following the state
location <- which(!is.na(location))-1

# # Get the word from the position found
 location_split <- split[location]
 
 description$city[i] <- location_split[1]
 
 # adding states (state abbreviation)
 #comparing split with state abbreviation 
 state <- match(split, stateabbreviation$Abbreviation)
 #Get the position of each state
 state <- which(!is.na(state))
 #extract state based on the position
 state_split <- split[state]
 #adding states to the new column 
 description$state[i] <- state_split[1]
 
}

#visualization
if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; library(ggplot2)
if (!require("maps")) install.packages("maps", quiet=TRUE) ; library(maps)

#load US map data
states_location <- read_excel("~/GitHub/SMA/SMAGroup1/states_location.xlsx")


#Adding lantitude and longitude for the map
description$lat <- states_location$latitude[match(description$state, states_location$state)]
description$long <- states_location$longitude[match(description$state, states_location$state)]

install.packages("rworldmap")
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-110, -100), ylim = c(10, 50), asp = 1)
points(description$long, description$lat, col = "red", cex = 0.5)


#table-which state is looking for data analysts/engineers the most?
state_count<-table(description$state)
state_count<-as.data.frame(state_count) 
library(ggplot2)
ggplot(state_count, aes(x = as.character(state_count$Var1), y = state_count$Freq)) + geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 60, vjust = )) + labs(y = "Frequency", x= "States")

#################Positions/Titles

dictionary <- read_excel("~/GitHub/SMA/SMAGroup1/Dictionary.xlsx")

install.packages("stringr")
library(stringr)
install.packages("dplyr")
library(dplyr)

#reloading the data + cleaning for "position" column
descriptionpos <- readtext("C:\\Users\\dkewon\\Documents\\GitHub\\SMA\\SMAGroup1\\Data\\concept_extraction_cvs\\concept_extraction_cvs\\*.txt")
descriptionpos$text<-gsub("\\n", " ", descriptionpos$text)
descriptionpos$text <- tolower(descriptionpos$text)
descriptionpos$text<-gsub("[[:punct:][:blank:]]+", " ", descriptionpos$text)

jobdictionary <- dictionary$Position

# creating df before inserting in the description basetable 
df <- data.frame(text = as.character(descriptionpos$text))

#copying the text column to the job column
df$job <- df$text

#function to detect a word from the job dictionary and when found to take this word
#plus all what is on the left of this word in order to get the whole jobtitle

jobdescriber <- function(desctext){
  if (str_detect(desctext, paste(jobdictionary, collapse = "|"))){
    split <- strsplit(as.character(desctext),split=" ")[[1]] 
    matches <- unique (grep(paste(jobdictionary,collapse="|"), 
                            split, value=TRUE))
    
    desctext <- str_replace(desctext, matches, paste0(matches, "&"))
    
    desctext <- strsplit(desctext, "&")
    
    desctext <- desctext[[1]][1]
  }else{
    desctext <- "missing"
  }
  
}
df[[2]] <- lapply(df[[2]], jobdescriber)
description$position<-df$job

#################Language

# detecting languages other than English
install.packages("textcat")
library(textcat)
install.packages("cld2")
library(cld2)
r <- textcat(description$text)
x <- cld2::detect_language(description$text)
df2 <- data.frame(description = as.character(description$text))
description$language <- cld2::detect_language(as.character(df2[[1]]))

#################Organization
#re-loading/cleaning text files for organizations
descriptionorg <- readtext("C:\\Users\\dkewon\\Documents\\GitHub\\SMA\\SMAGroup1\\Data\\concept_extraction_cvs\\concept_extraction_cvs\\*.txt")
descriptionorg$text<-gsub("\\n", " ", descriptionorg$text)
descriptionorg$text <- tolower(descriptionorg$text)
descriptionorg$text<-gsub(c("/"), "", descriptionorg$text)

#creating df2 before inserting in the description basetable 
df2 <- data.frame(description = descriptionorg$text)


df2$organisation <- as.character(df2$description)
jobdictionary <- dictionary$Position

df2 <- data.frame(description = descriptionorg$text)

df2$organisation <- as.character(df2$description)
jobdictionary <- dictionary$Position

#using function to extract companies after job titles 
orgdescriber1 <- function(org){
   if(split <- strsplit(as.character(org),split=" ")[[1]]) {
     match <- unique (grep(paste(jobdictionary,collapse="|"),
                           split, value=TRUE))

     org <- strsplit(org, match)

     org <- org[[1]][2]
   } else {
     org <- "missing"
   }

 }

 df2[[2]] <- lapply(df2[[2]], orgdescriber1)

# since company comes before "-", new function "orgdescriber2" was added to extract what is before "-"

 orgdescriber2 <- function(org2){
   org2 <- strsplit(org2, "-")
   org2 <- org2[[1]][1]
 }

 df2[[2]] <- lapply(df2[[2]], orgdescriber2)

 # adding companies in the description basetable
# df2$organisation2<-unlist(lapply(strsplit(as.character(df2$organisation), c("engineer","analyst","developer")), function(x) x[2]))
df2$organisation2<-regmatches(df2$organisation,gregexpr("(?<=developer).*",df2$organisation,perl=TRUE)) 
description$company<-df2$organisation2

 