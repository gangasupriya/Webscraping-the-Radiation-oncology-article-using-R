#Step 1 installing required packages

install.packages("rvest")
install.packages("httr")
install.packages("xml2")

#installing additional requried libraries
install.packages("dplyr")
install.packages("stringr")
install.packages("ggplot2")

#loading the libraries
library(rvest)
library(httr)
library(xml2)
library(dplyr)
library(stringr)
library(ggplot2)



#Step 2 : Scraping the Article Data

#Create a empty data set
RadOnc = data.frame()

# Take input from the user through the console
# After running this line, we entered the year

year = readline()
#Example: 2023
#After entering the year run the rest of the  code 

year = as.integer(year)

volume =  year - 2005 # volume is the keyword present in the filtering section of year in the webpage
volume

link1= paste0("https://ro-journal.biomedcentral.com/articles?tab=keyword&searchType=journalSearch&sort=PubDate&volume=",volume,"&page=1")
page1 = read_html(link1)

# Getting the number of pages for the selected year by scraping the website

entries = page1 %>% html_nodes(".c-listing-title strong") %>% html_text()
entries = as.integer(entries)
entries
if(entries<=50){
  n<-1
}else if(entries>=50 && entries<=100){
  n<-2
}else if(entries>=100 && entries<=150){
  n<-3
}else if(entries>=150 && entries<=200){
  n<-4
}else if(entries>=200 && entries<=250){
  n<-5
}else if(entries>=250 && entries<=300){
  n<-6
}
print(n)
#here n refers to number of pages


# Iterating through 'n' pages using a for-loop to extract data from the webpage
# To scrape the data, we run the entire for-loop once
# Before running the for-loop each time, we cleared the 'RadOnc' dataset by running the code on line 25
# Otherwise, existing data will cause duplicates

for(page_result in seq(from =1, to = n, by=1)){
  link= paste0("https://ro-journal.biomedcentral.com/articles?tab=keyword&searchType=journalSearch&sort=PubDate&volume=",volume,"&page=",page_result,"")
  print(link)
  page = read_html(link)
  
  #extracting titles
  name = page %>% html_nodes(".c-listing__title a") %>% html_text()
  
  #extracting published date
  published_date = page %>% html_nodes(".c-listing__metadata span+ span") %>% html_text()
  
  #extracting authors
  authors = page %>% html_nodes(".c-listing__authors-list") %>% html_text()
  article_links = page %>% html_nodes(".c-listing__title a") %>% html_attr("href") %>% paste("https://ro-journal.biomedcentral.com", .,sep="")
  
  #function for extracting abstract by entering into each article
  get_abstract <- function(article_link){
    article_page = read_html(article_link)
    article_abstract = article_page %>% html_node("#Abs1-content") %>% html_text() %>% paste(collapse = ",")
    return(article_abstract)  
  }
  #extracting abstract
  abstract = sapply(article_links,FUN = get_abstract)
  
  #function for extracting keywords by entering into each article
  get_keywords <- function(article_link){
    article_page = read_html(article_link)
    div <-article_page %>% html_node(".c-article-subject-list")
    text<-  div %>% html_nodes("a") %>% html_text()
    article_keywords<-paste(text,collapse = ", ")  # Concatenate with comma
    return(article_keywords)
  }
  #extracting keywords
  keywords = lapply(article_links,FUN = get_keywords)
  keywords <- unlist(keywords)
  
  
  #function for extracting Corresponding by entering into each article
  get_corresponding_authors <- function(article_link){
    article_page = read_html(article_link)
    article_corresponding_authors = article_page %>% html_node("#corresp-c1")%>% html_text()
    return(article_corresponding_authors)
  }
  #extracting corresponding_authors
  corresponding_authors = sapply(article_links,FUN = get_corresponding_authors)
  
  #function for extracting Corresponding author's email by entering into each article
  get_corresponding_authors_email <- function(article_link){
    article_page = read_html(article_link)
    article_corresponding_authors_email = article_page %>% html_node("#corresp-c1")%>% html_attr("href")
   return(article_corresponding_authors_email)
  }
  #extracting corresponding author's email
   corresponding_authors_email = sapply(article_links,FUN = get_corresponding_authors_email)
  
  #Inserting all the data obtained from the Web scraping into a data set named " RadOnc " which is initialized in the beginning of the code 
  RadOnc = rbind(RadOnc,data.frame(name,authors,corresponding_authors,corresponding_authors_email,published_date,abstract,keywords))
}

# The scraped data is stored in the dataset called "RadOnc"
# The line below displays the raw data in "RadOnc"
# To verify if the dataset is correct,  we compared its entries with the output from line 45
# If the number of entries matches, than data is fine; if not, there may be duplicate entries
# To fix duplicates,we run line 25 to clear the dataset and then re-run the rest of the code 
# At this point, "RadOnc" contains raw data ready for cleaning


View(RadOnc)







#Step 3 : Data Cleaning and Pre Processing

# Once we had started step  3, do not run any code from step 1 or 2 except for loading libraries
# Running that code may shuffle the data and lead to errors or incorrect results
# Only run step 1 and 2 again if you want to reset your data and start over from the beginning

#Here we need to clean these columns since they are not in proper format
#viewing the columns
print(RadOnc$corresponding_authors_email)
print(RadOnc$published_date)

#Removing unwanted things from these columns
RadOnc$corresponding_authors_email<-str_remove(RadOnc$corresponding_authors_email,"mailto:")
RadOnc$published_date<-str_remove(RadOnc$published_date,"Published on: ")
RadOnc<-na.omit(RadOnc)

#The dataset after cleaning
View(RadOnc)

write.csv(RadOnc, file = "Radiation_Oncology_Data.csv", row.names = FALSE)



#Step 4 : Data Analysis and Visualization


#a. Finding number of articles published in each month using Data Visualisation

#Converting our Data into the format which "R" can understand
newdates<-as.Date(RadOnc$published_date,format = "%d %B %Y")

# Extract the month from the dates
month_counts <- table(format(newdates, "%B"))

# Sort the months in chronological order
sorted_months <- factor(names(month_counts), levels = month.name)

# Plot the graph with sorted months
ggplot(data.frame(month = sorted_months, count = as.numeric(month_counts)), aes(x = month, y = count)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  labs(title = 'Number of Articles Published by Month', x = 'Month', y = 'Number of Articles')

#Here you can see the plot which represents number of articles published in every month in that specific year
#This can help us to find in month more articles are getting published


#b.Finding Top Authors in that based on number of articles they have published in that year


print(RadOnc$authors)
#Here you can authors are in the combined format.

# Split the combined authors into individual authors
all_authors <- unlist(strsplit(RadOnc$authors, ", "))

# Create a list of unique authors
unique_authors <- unique(all_authors)

#Finding number of articles involved by each unique author
articles_count <- sapply(unique_authors, function(author) sum(grepl(author, RadOnc$authors)))

# Create a data frame with unique authors and their article counts
author_articles <- data.frame(author = unique_authors,
                              articles_involved = articles_count)

# Aggregate articles counts for each author (in case there are duplicates)
author_articles <- aggregate(articles_involved ~ author, data = author_articles, FUN = sum)

# Get the top 5 authors with the most articles involved
top_authors <- head(author_articles[order(author_articles$articles_involved, decreasing = TRUE), ],5)
print(top_authors)
# Plot the top authors involved in articles
library(ggplot2)
ggplot(top_authors, aes(x = articles_involved, y = reorder(author, articles_involved))) +
  geom_bar(stat = "identity", fill = c("blue","lightgreen","red","purple","orange")) +
  labs(title = "Top Authors and Articles Involved",
       x = "Number of Articles Involved",
       y = "Author Name") +
  theme_minimal()

#Here you can see the top 5 authors in that year along with number of publications they were involved in that specific year


#c. Finding Most repeated Keywords

print(RadOnc$keywords)
#Here you can authors are in the combined format.

#splitting the keywords
all_keywords <- unlist(strsplit(RadOnc$keywords, ", "))

# Create a list of unique keywords
unique_keywords <- unique(all_keywords)

articles_count <- sapply(unique_keywords, function(keyword) sum(grepl(keyword, RadOnc$keywords)))

# Create a data frame with unique keywords and their article counts
keyword_articles <- data.frame(keyword = unique_keywords,
                              times_repeated = articles_count)

# Aggregate articles counts for each keyword (in case there are duplicates)
keyword_articles <- aggregate(times_repeated ~ keyword, data = keyword_articles, FUN = sum)

# Get the top 5 repeated keywords
repeated_keywords <- head(keyword_articles[order(keyword_articles$times_repeated, decreasing = TRUE), ],5)
print(repeated_keywords)

# Plot the top Repeated keywords
library(ggplot2)
ggplot(repeated_keywords, aes(x = times_repeated, y = reorder(keyword, times_repeated))) +
  geom_bar(stat = "identity", fill = c("blue","lightgreen","red","purple","orange")) +
  labs(title = "Frequent Keywords and Occurence",
       x = "Number of times Keywords Occured",
       y = "Keywords") +
  theme_minimal()+coord_flip()
# Here you can see the most repeated keywords


frequent_keywords <- head(keyword_articles[order(keyword_articles$times_repeated, decreasing = TRUE), ],30)
print(frequent_keywords)


#d. Creating a scatter plot using the above values to show Frequent Keywords
ggplot(frequent_keywords, aes(x = times_repeated, y = reorder(keyword, times_repeated))) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Frequent Keywords and Occurrence",
       x = "Number of times Keywords Occurred",
       y = "Keywords") +
  theme_minimal()
#Here you can see the scatterolot of most frequent keywords



