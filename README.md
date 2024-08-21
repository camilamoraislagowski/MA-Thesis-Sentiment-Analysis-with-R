# Assessing western media’s sentiment on artificial intelligence

## ℹ️ Information

This project is a part of the MA thesis "Assessing western media’s sentiment on artificial intelligence: a reduced-bias method of analysis", done by student Camila Morais, for the course International Legal Communication at the University of Warsaw.


## 📩 Installation

This project was created using R programming language and Rstudio development environment (Version 2024.04.2+764 (2024.04.2+764)). Therefore, in order to run this project, it is recommended you have an up-to-date version of Rstudio, which should be available to download from their website: https://cran.r-project.org/

The code itself can be found under the main file named “Sentiment Analysis 2”, where you will find three files which start with the name “MA_SentimentAnalysisTest_”. There is little difference between these files, as the majority of the code is the same for the most part, however, given the dataset used for each selected news outlet is different and thus requires different data to be linked to the project in order for the analysis to work properly, the decision was made to separate the code into three different files, one for each of the news outlets. You can find the file for the analysis of the BBC News outlet under the name “MA_SentimentAnalysisTest_BBC.R”, the file for the Financial Times outlet under the name “MA_SentimentAnalysisTest_FinancialTimes.R”, and the file for the Guardian outlet under the name “MA_SentimentAnalysisTest_Guardian.R”. 

The main file named “Sentiment Analysis 2” also contains Microsoft Excel files, in which you can find the dataset of news articles related to the subject of AI from each of the selected news outlets. Please download them and attach each Excel file according to their matching news outlet (i.e. the Microsoft Excel BBC News dataset will be attached to the R file “MA_SentimentAnalysisTest_BBC.R”).


## 💻 Usage 

You will find explanations and commentary for every step already “built-in” within the code, however, here is an alert to beginners:  When running the code, be mindful that all commentary has to be done with a # in front, otherwise Rstudio will attempt to run your commentary as a part of the code, which will not work. 

In order to link the Excel files to their corresponding Rstudio project file, follow the steps below: 

First, open the Rstudio project file, then proceed to the following: **File > Import Dataset > From Excel > Browse >** then link to the corresponding news outlet Excel file. After this, you will be able to run the code normally, but be mindful that there are sections of the code which must be changed according to the news outlet you intend to use, therefore, follow as indicated below: 

For BBC News data, use: BBCnewsdata 

For Financial Times data, use: FinancialTimesData 

For The Guardian data, use: TheGuardianData 

----------
Sections of the code which must be changed according to the news outlets dataset:

### OUTPUT CHECK -> CHANGE THE DATA FILE TO CHANGE THE NEWS PROVIDER 
head(**BBCnewsData**)

tail(**BBCnewsData**)


### TOKENIZATION -> CHANGE THE DATA FILE TO CHANGE THE NEWS PROVIDER
news_df <- **BBCnewsData** %>% select(headline_text)

news_tokens <- news_df %>% unnest_tokens(word, headline_text)


### Ensure the data has a column 'headline_text' -> CHANGE THE DATA FILE TO CHANGE THE NEWS PROVIDER
news_df <- **BBCnewsData** %>% select(headline_text)

