# install.packages("readr")
# install.packages("party")
# #Installation of caret failed for some missing packages, so here we install them:
# install.packages("iterators")
# install.packages("scales")
# install.packages("pbkrtest")
# install.packages("SnowballC")
# #for older R versions, an older pbkrtest should be installed:
# install.packages("lme4")
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz" 
# install.packages(packageurl, repos=NULL, type="source")
# #finally we can install caret:
# install.packages("caret", dependencies = TRUE)
# install.packages("Metrics")
# install.packages("Matrix")
# #install.packages('randomForest')
# install.packages("tm")
# install.packages("NLP")
#install.packages("RWeka")
# 
# unzip("test.csv.zip")
# unzip("train.csv.zip")

rm(list = ls(all = TRUE))
setwd("C:\\Users\\USER\\Dropbox\\Raz&Livnat\\lastYear\\b\\ofrit\\ass4\\data")

library(RWeka)
library(readr)
library(SnowballC)
library(Matrix)
library(NLP)
library(tm)   
library(stringr)
library(e1071)

#library(randomForest)
test  <- read_csv("test.csv")
train <- read_csv("train.csv")

median_relevance <- factor(train$median_relevance)
relevance_variance <- train$relevance_variance

train$median_relevance = NULL
train$relevance_variance = NULL


remove_stop_words = function(string){
  stop_words = c("\n"," if "," is "," it ","are" ,"http", " www "," the ", " they "," a "," in "," with "," this ",
              " while ", " still "," and ", " more "," to "," your "," for ", " its "," that "," by "," on"," each ",
              " you "," or "," be ", " can "," all "," also "," at "," you'll ", " like ", " just "," there ",
              " has "," have ", " does "," not "," as "," may "," due "," up "," while "," where "," percent ",
              "text-decoration:.*?;","text-align:.*?;","<br/>","<tbody>","</tbody>"," st ",
              "font-family:.*?;","font-size:.*?;","color:.*?;","margin:.*?;","&nbsp;",
              "<li>","</li>","<ul>","</ul>","<th>","</th>","<td>","</td>","<tr>","</tr>","<br>","</br>","<p>",
              "</p>","<table>","</table>","padding:.*?;","width:.*?;","height:.*?;","display:.*?;","float:.*?;",
              "font-weight:.*?;","list-style:.*?;","list-style-type:.*?;","<strong>","</strong>","border:.*?;",
              "<.*?>","img","style","px","margin","left", "right","font","solid","This translation tool is for your convenience only.*?Note: The accuracy and accessibility of the resulting translation is not guaranteed"
              ,"[.#@][a-zA-Z0-9_,. \\-\\s#,:>]*[\\s]*\\{.*?\\}","[a-zA-Z0-9_, \\s]*[\\s]*\\{.*?\\}",
              "Seller assumes all responsibility for this listing.","Last updated on",
              "html, body, div, span, applet, object,.*?HTML5 display-role reset for older browsers"
  )
  ##need to handel /\().
  signes = c("\t","!","#","$",",","'s","%","^","&","*","-",";",":","-","_","+","=","|")
  ft = c(" foot "," feet "," ft ")
  inc = c(" inches "," inch ")
  lb = c(" pounds "," pound "," lbs "," lb ")
  gal = c(" gallons "," gallon "," gals "," gal ")
  oz = c(" ounces "," ounce "," oz ")
  mm = c(" millimeters "," milimeters "," mm ")
  amp = c(" amperes "," ampere "," amps "," amp ")
  deg = c(" degrees "," degree "," deg ")
   
  for (i in 1:length(stop_words)){
    string = gsub(stop_words[i], " ", string,T)
  }
  
  for (i in 1:length(signes)){
    string = gsub(signes[i], "", string,T)
  }
  for (i in 1:length(ft)){
    string = gsub(ft[i], "ft.", string,T)
  }
  for (i in 1:length(inc)){
    string = gsub(inc[i], "inc.", string,T)
  } 
  for (i in 1:length(lb)){
    string = gsub(lb[i], "lb.", string,T)
  }
  for (i in 1:length(gal)){
    string = gsub(gal[i], "gal.", string,T)
  }
  for (i in 1:length(oz)){
    string = gsub(oz[i], "oz.", string,T)
  }
  for (i in 1:length(mm)){
    string = gsub(mm[i], "mm.", string,T)
  }
  for (i in 1:length(amp)){
    string = gsub(amp[i], "amp.", string,T)
  }
  for (i in 1:length(deg)){
    string = gsub(deg[i], "deg.", string,T)
  }
  string = gsub("toliet", "toilet", string,T)
  string = gsub("airconditioner", "air conditioner", string,T)
  string = gsub("condtion", "condition", string,T)
  string = gsub("vinal", "vinyl", string,T)
  string = gsub("vynal", "vinyl", string,T)
  string = gsub("skill", "skil", string,T)
  string = gsub("snowbl", "snow bl", string,T)
  string = gsub("plexigla", "plexi gla", string,T)
  string = gsub("rustoleum", "rust-oleum", string,T)
  string = gsub("whirpool", "whirlpool", string,T)
  string = gsub("whirlpoolga", "whirlpool ga", string,T)
  string = gsub("whirlpoolstainless", "whirlpool stainless", string,T)

  string = str_replace(gsub("\\s+", " ", str_trim(string)), "B", "b")
  string=iconv(string, "latin1", "ASCII", sub="")
  return (string)
}

ids = test$id
rtrain = nrow(train)
#union categories
levels(train$query) <- union(levels(train$query), levels(test$query))
levels(train$product_title) <- union(levels(train$product_title), levels(test$product_title))
levels(train$product_description) <- union(levels(train$product_description), levels(test$product_description))
levels(test$query) <- union(levels(train$query), levels(test$query))
levels(test$product_title) <- union(levels(train$product_title), levels(test$product_title))
levels(test$product_description) <- union(levels(train$product_description), levels(test$product_description))

all_data<-rbind(train,test)

all_data$product_description = lapply(all_data$product_description,tolower)
all_data$product_description = lapply(all_data$product_description,removeNumbers)
all_data$product_description = lapply(all_data$product_description,remove_stop_words)
all_data$product_description = lapply(all_data$product_description,removeWords, stopwords("english"))
all_data$product_description = lapply(all_data$product_description,stemDocument)
all_data$product_description = lapply(all_data$product_description,removePunctuation)
all_data$product_description = lapply(all_data$product_description,stripWhitespace)

#train
q_terms <- Corpus(VectorSource(all_data$query))
dtm <-
  DocumentTermMatrix(q_terms, control = list(
    weighting = function(x)
      weightTfIdf(x, normalize = T)))
dfQ <- as.data.frame(as.matrix(dtm))

pt_terms <- Corpus(VectorSource(all_data$product_title))
dtm <-
  DocumentTermMatrix(pt_terms, control = list(
    weighting = function(x)
      weightTfIdf(x, normalize = T)))
dfPt <- as.data.frame(as.matrix(dtm))

pd_terms <- Corpus(VectorSource(all_data$product_description))
dtm <-
  DocumentTermMatrix(pd_terms, control = list(
    weighting = function(x)
      weightTfIdf(x, normalize = T)))
dfPd <- as.data.frame(as.matrix(dtm))

colnames(dfPt) = paste("pt_", colnames(dfPt), sep = "")
colnames(dfPd) = paste("pd_", colnames(dfPd), sep = "")
colnames(dfQ) = paste("q_", colnames(dfQ), sep = "")

all_data=cbind(dfQ,dfPt,dfPd)
all_data<-as.matrix(all_data)

train = all_data[1:rtrain,]
rtest = rtrain+1
test = all_data[rtest:nrow(all_data),]

model <- svm(train,median_relevance, kernel="linear", cost=.75)

pred <- predict(model,test)
result <- cbind(ids,pred)
colnames(result)=c("id","prediction")
write.csv(result,"ass4_model.csv",row.names=F)


