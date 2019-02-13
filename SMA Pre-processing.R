# load text packages
# The tm package expects the SnowballC and slam packages to be installed.

for (i in c('SnowballC','slam','tm','RWeka','Matrix')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

# readtext package to import the data

if (!require("readtext")) install.packages("readtext", quiet=TRUE) ; library(readtext)


# import all files in the directory
description <- readtext("C:/Users/Remo/Documents/GitHub/SMAGroup1/Data/concept_extraction_cvs/concept_extraction_cvs/*.txt")
description$language <- cld2::detect_language(as.character(description[[2]]))


# First we will delete non-recognizable characters, otherwise the tm package will get in trouble later. 
# Note that this can also delete some emoticons
description_txt <- sapply(description[,2],function(x) iconv(x, 'utf8', 'ascii',""))


description <- VCorpus(VectorSource(description_txt))

# inspect the corpus

description[[1]]
as.character(description[[1]])

# Which transformer functions are available? 
getTransformations()

# Remove Punctuation,Numbers, Whitespace and convert text to lowercase
description <- tm_map(description, removePunctuation)
description <- tm_map(description, removeNumbers)
description <- tm_map(description, stripWhitespace)
description <- tm_map(description,content_transformer(tolower))

# key words tpo extract the information from description

KEYWORDS <- c('hadoop','python', 'nosql' ,'html', 'spark' , 'sas', 'excel', 'aws', 'azure', 'java', 'tableau', 'matlab')

description$job <- paste(strsplit(description[,2], " ")[[1]][1:4], colapse=" ")

string1 <- "c developer in the house"

install.packages("stringr")
library(stringr)


  

string2 <- str_replace(string1, "developer", "developer,")

solution <- strsplit(string2, ",")
solution2 <- solution[[1]][1]

