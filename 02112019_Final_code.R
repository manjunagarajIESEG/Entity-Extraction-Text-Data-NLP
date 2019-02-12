if (!require("readtext")) install.packages("readtext", quiet=TRUE) ; library(readtext)

install.packages("data.table")
library(data.table)
library(readtext)
library(readxl)

for (i in c('SnowballC','slam','tm','RWeka','Matrix')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

# Stringr package to match the kewords
if (!require("stringr")) install.packages("stringr", quiet=TRUE) ; library(stringr)


if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; library(ggplot2)
if (!require("dplyr")) install.packages("dplyr", quiet=TRUE) ; library(dplyr)

if (!require("openNLP")) install.packages("openNLP", quiet=TRUE) ; library(openNLP)
if (!require("NLP")) install.packages("NLP", quiet=TRUE) ; library(NLP)
install.packages("openNLPmodels.en", dependencies=TRUE, repos = "http://datacube.wu.ac.at/")
library(openNLPmodels.en)


########## initial test with openNLP to extract organizations and locations

## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()

## import all files in the directory
test_openNLP <- readtext("C:\\Users\\dkewon\\Documents\\GitHub\\SMA\\SMAGroup1\\Data\\concept_extraction_cvs\\concept_extraction_cvs\\*.txt")

## Entity recognition for organizations and locations.
entity_org <- Maxent_Entity_Annotator(kind="organization")
entity_location <- Maxent_Entity_Annotator(kind="location", probs = TRUE)


##function to check all job offers on organizations and location

anno <- function(x){
  
  # x is the tweet or text
  s <- as.String(x)
  
  a2 <- NLP::annotate(s, list(sent_token_annotator, word_token_annotator))
  
  organizations = list(s[entity_org(s,a2)])
  location = list(s[entity_location(s,a2)])
  
  return(list(organizations,location))
}

organizations_locations <- rbindlist(lapply(test_openNLP[,2],anno))


## import all files in the directory
path <- "C:\\Users\\dkewon\\Documents\\GitHub\\SMA\\SMAGroup1\\Data\\concept_extraction_cvs\\concept_extraction_cvs\\*.txt"

description <- readtext(path)

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

#making naconverter a comment to visualize the data. If there is "missing", we can't visualize easily
# naconverter <- function(col){
#   ifelse(is.na(col), "missing", col)
# }
# 
# description$city <- lapply(description$city, naconverter)
# description$state <- lapply(description$state, naconverter)

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


#reloading the data + cleaning for "position" column
descriptionpos <- readtext(path)
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

#counting positions
fLength <- function(str1, pat){
  lengths(regmatches(str1, gregexpr(paste0("\\b", pat), str1, ignore.case = TRUE)))
}

sum(fLength(final_table$position, "developer"))
sum(fLength(final_table$position, "engineer"))
sum(fLength(final_table$position, "analyst"))

#################Language

# detecting languages testing textcat and cld2 and finally using cld2
install.packages("textcat")
library(textcat)
install.packages("cld2")
library(cld2)

df2$language <- textcat(as.character(df2$description))
df2 <- data.frame(description = as.character(description$text))
description$language <- cld2::detect_language(as.character(df2[[1]]))

table(df2$language, exclude = NULL)
table(description$language, exclude = NULL)
checkset <- description[is.na(description$language),]
checkset2 <- df2[df2$language =="catalan", ]

description$language <- lapply(description$language, naconverter)

#counting languages
length(grep("fr", final_table$language))
length(grep("en", final_table$language))


#################Organization
#re-loading/cleaning text files for organizations
descriptionorg <- readtext(path)
descriptionorg$text<-gsub("\\n", " ", descriptionorg$text)
descriptionorg$text <- tolower(descriptionorg$text)
descriptionorg$text<-gsub(c("/"), "", descriptionorg$text)
descriptionorg$text<-gsub("\\("," ", descriptionorg$text)

#creating df2 before inserting in the description basetable 
df2 <- data.frame(description = descriptionorg$text)


df2$organisation <- as.character(df2$description)
jobdictionary <- dictionary$Position

df2 <- data.frame(description = descriptionorg$text)

df2$organisation <- as.character(df2$description)
jobdictionary <- dictionary$Position



#using function to extract companies after job titles 
orgdescriber1 <- function(org){
  if (str_detect(org, paste(jobdictionary, collapse = "|"))){
    split <- strsplit(as.character(org),split=" ")[[1]]
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


# change the results that are longer than 7 words (there are almost no company names with
# more than 7 words) to missing
wordcounter <- function(org3){
  wordcount <- sapply(strsplit(org3, " "), length)
  if (wordcount > 7){
    org3 <- "missing"
  }else{
    org3 <- org3
  }
}


df2[[2]] <- lapply(df2[[2]], wordcounter)


description$organisation <- df2$organisation

################SKILLS
data <- readtext(path)

# First we will delete non-recognizable characters, otherwise the tm package will get in trouble later. 
# Note that this can also delete some emoticons
description_txt <- sapply(data[,2],function(x) iconv(x, 'utf8', 'ascii',""))


data <- VCorpus(VectorSource(description_txt))

# Was the corpus loaded succesfully? Print a random description
data.length <- length(data)
writeLines(as.character(data[[sample(1:data.length, 1)]]))


# Remove Punctuation,Numbers, Whitespace and convert text to lowercase
data <- tm_map(data, removePunctuation)
data <- tm_map(data, removeNumbers)
data <- tm_map(data, stripWhitespace)
data <- tm_map(data,content_transformer(tolower))



#remove stopwords
data <- tm_map(data, removeWords, stopwords("english"))


# convert the corpus to dataframes

data <- data.frame(text = sapply(data, as.character), stringsAsFactors = FALSE)


# function to match the skills in job description

skills <- function(keywords) {
  ifelse(str_detect(data$text, keywords)==TRUE,1,0)
}

# key words to extract the information from description

keywords <- dictionary$skills


for (i in keywords) {
  data[i] <- skills(i)
}

final_table <- cbind(description,data[,2:19])

r <- barplot(colSums(final_table[,10:27]),main = "Technical Skills",ylab = "count",las=2,ylim = c(0,1600))

#################test

# prepare comparison set 
comparison_set <- read_excel("C:\\Users\\dkewon\\Documents\\GitHub\\SMA\\SMAGroup1\\comparison_set.xlsx")
comparison_set <- comparison_set[, 2:24]

# prepare model_results
comparison_docs <- c("100.txt", "101.txt", "102.txt", "103.txt", "104.txt", "105.txt", "106.txt", "107.txt",
  "108.txt", "109.txt", "110.txt", "111.txt", "112.txt", "113.txt", "114.txt", "115.txt",
  "116.txt", "117.txt", "118.txt", "119.txt")


comparison_docs2 <- c("1100.txt", "1101.txt", "1102.txt", "1103.txt", "1104.txt", "1105.txt", "1106.txt", "1107.txt",
                     "1108.txt", "1109.txt", "1110.txt", "1111.txt", "1112.txt", "1113.txt", "1114.txt", "1115.txt",
                     "1116.txt", "1117.txt", "1118.txt", "1119.txt")


model_result <- final_table[str_detect(final_table$doc_id, paste(comparison_docs, collapse = "|")),]
model_result <- model_result[!str_detect(model_result$doc_id, paste(comparison_docs2, collapse = "|")),]
model_result <- model_result[, -c(1,2, 5, 6)]

# trimming trailing and leading blanks (would lead to problems in string comparison)
model_result[[5]] <- lapply(model_result[[5]], trimws)

# check whether these dataframes are equal
result <- table(model_result==comparison_set)
model_result==comparison_set



# take the non-identical cells
False <- as.numeric(result[1])

# take the identical cells
True <- as.numeric(result[2])

# calculate the accuracy 
Accuracy <- True / (True+False)



# test without skills section
comparison_set2 <- comparison_set[, 1:5]
model_result2 <- model_result[, 1:5]


# check whether these dataframes are equal
result2 <- table(model_result2==comparison_set2)
model_result2==comparison_set2

# take the non-identical cells
False2 <- as.numeric(result2[1])

# take the identical cells
True2 <- as.numeric(result2[2])

# calculate the accuracy 
Accuracy2 <- True / (True+False)


