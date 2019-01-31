if (!require("openNLP")) install.packages("openNLP", quiet=TRUE) ; library(openNLP)
if (!require("NLP")) install.packages("NLP", quiet=TRUE) ; library(NLP)
if (!require("readtext")) install.packages("readtext", quiet=TRUE) ; library(readtext)
install.packages("openNLPmodels.en", dependencies=TRUE, repos = "http://datacube.wu.ac.at/")
library(openNLPmodels.en)
install.packages("data.table")
library(data.table)
library(readtext)

## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()

## import all files in the directory
description <- readtext("C:/Users/Remo/Documents/GitHub/SMAGroup1/Data/concept_extraction_cvs/concept_extraction_cvs/*.txt")

testdescription <- description[1,2]
s <- as.String(testdescription)

a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))


## Entity recognition for persons, organizations and locations.
entity_person <- Maxent_Entity_Annotator(kind="person")
entity_org <- Maxent_Entity_Annotator(kind="organization")
entity_location <- Maxent_Entity_Annotator(kind="location", probs = TRUE)

persons = s[entity_person(s,a2)]
organizations = s[entity_org(s,a2)]
location = s[entity_location(s,a2)]


##function to check all job offers on organizations and location

anno <- function(x){
  
  # x is the tweet or text
  s <- as.String(x)
  
  a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
  
  organizations = list(s[entity_org(s,a2)])
  location = list(s[entity_location(s,a2)])
  
  return(list(organizations,location))
}

annotations <- rbindlist(lapply(description[,2],anno))


## test with spaCyr
install.packages("spacyr")
library(spacyr)
spacy_install()

spacy_initialize()

txt <- c(d1 = "spaCy is great at fast natural language processing.",
         d2 = "Mr. Smith spent two years in North Carolina.")

parsedtxt <- spacy_parse(txt)
