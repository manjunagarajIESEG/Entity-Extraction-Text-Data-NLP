
if (!require("readtext")) install.packages("readtext", quiet=TRUE) ; library(readtext)

install.packages("data.table")
library(data.table)
library(readtext)
library(readxl)

## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()

## import all files in the directory
description <- readtext("C:\\Users\\dkewon\\Documents\\GitHub\\SMA\\SMAGroup1\\Data\\concept_extraction_cvs\\concept_extraction_cvs\\*.txt")

stateabbreviation <- read_excel("~/GitHub/SMA/SMAGroup1/stateabbreviation.xlsx")
View(stateabbreviation)
#################

#getting rid of punctuation and trailing space 
description$text<-gsub("[[:punct:][:blank:]]+", " ", description$text)
description$text<-gsub("\\n", " ", description$text)

#creating a city column
description$city <- NA

for (i in 1:length(description$text)){
#description$text <- tolower(description$text)

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
 states <- match(split, stateabbreviation$Abbreviation)
 states <- which(!is.na(states))
 states_split <- split[states]
 description$states[i] <- states_split[1]
 
}

description$state<- NULL

############ still struggling to get the first part of the city#########


