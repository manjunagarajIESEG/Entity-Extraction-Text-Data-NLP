
if (!require("readtext")) install.packages("readtext", quiet=TRUE) ; library(readtext)

install.packages("data.table")
library(data.table)
library(readtext)

## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()

## import all files in the directory
description <- readtext("C:\\Users\\dkewon\\Documents\\GitHub\\SMA\\SMAGroup1\\Data\\concept_extraction_cvs\\concept_extraction_cvs\\*.txt")

#################

description$text<-gsub("[[:punct:][:blank:]]+", " ", description$text)
description$text<-gsub("\\n", " ", description$text)

description$city <- NA

for (i in 1:length(description$text)){
#description$text <- tolower(description$text)
split <- strsplit(description$text[i]," ")[[1]] 

# # Find the negative words in the split

location <- match(split, c("CA","FL","IL","MD","MI","NJ","NY","OR","PA","WA","DC", "MA","TN"))

# # Get the position of the word following the negative word
location <- which(!is.na(location))-1
# 
# # Get the word from the position found
 location_split <- split[location]
 
 description$city[i] <- location_split[1]
}
split





