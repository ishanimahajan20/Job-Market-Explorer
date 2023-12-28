# Job-Market-Explorer

##### SCRAPPING USING R programming
Proficiently utilizing web scraping techniques, I extract internship data to get interesting analysis. This project is self created from idea to implementing on R studio, This project showcases my expertise in mining valuable information, conducting thorough analysis, and statistical rigor to extract meaningful conclusions from intricate data.

## Set Up Your Environment

install some packages that will be handy. Open your R console or script and run:

install.packages("rvest")
install.packages("dplyr")

rvest is a great package for web scraping, and dplyr will help us manipulate and analyze the data.

## Write Code to Scrape Job Listings

library(rvest)
library(dplyr)

Link = "https://internshala.com/internships/analytics,data-analysis-internship/"

#### Create an html document from a url

page = read_html(Link)

#html_nodes : selects parts of a document using CSS selectors

#### html_text will extract text from selected nodes

name = page %>% html_nodes(".profile .view_detail_button") %>% html_text()
View(name)

Company = page %>% html_nodes(".link_display_like_text") %>% html_text()

install.packages("stringr")
library(stringr)
Place = page %>% html_nodes("#location_names .view_detail_button") %>% html_text() %>% head(40) %>%  str_sub(1, 40)

Stipend = page %>% html_nodes(".stipend") %>% html_text()

Duration = page %>% html_nodes(".other_detail_item+ .other_detail_item .item_body") %>% html_text()

### Create a dataframe 

Internship.data <- data.frame(name , Company , Stipend , Duration, Place)

#print the data frame 
print(Internship.data)

## ANALYSIS OF STIPEND

### Summary of stipend distribution

summary(Internship.data$Stipend)

#It seems like the Stipend column is being treated as a character instead of a numeric variable. To perform numerical analysis such as calculating mean, median, min, and max, we need to convert it to a numeric format.

#### Convert Stipend column to numeric

#Internship.data$Stipend <- as.numeric(Internship.data$Stipend)

#### Check the data types
str(Internship.data)

#### Replace commas in Stipend column with an empty string

Internship.data$Stipend <- gsub("/month", "", Internship.data$Stipend)

Internship.data$Stipend <- gsub("/week", "", Internship.data$Stipend)

Internship.data$Stipend <- gsub("+  Incentives", "", Internship.data$Stipend)

Internship.data$Stipend <- gsub("Unpaid", "0", Internship.data$Stipend)

library(ggplot2)

## Create a bar chart for top companies

ggplot(Internship.data, aes(x = Company)) +
  geom_bar(stat = "count", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Top Companies Offering Internships", x = "Company", y = "Count")

  #error as x asis is not that visible
  ![x axis error](https://github.com/ishanimahajan20/Job-Market-Explorer/assets/134215344/a77047cb-d804-433c-a973-ef990790ab7e)




  ggplot(Internship.data, aes(x = Company)) +
  geom_bar(stat = "count", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Top Companies Offering Internships", x = "Company", y = "Count")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

![image](https://github.com/ishanimahajan20/Job-Market-Explorer/assets/134215344/f8bdd86f-f8c6-4bcc-afa5-b90202be7589)

## Create a bar chart for popular locations
ggplot(Internship.data, aes(x = Place)) +
  geom_bar(stat = "count", fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Popular Internship Locations", x = "Location", y = "Count")

#ERROR :  Aesthetics must be either length 1 or the same as the data (40)
##### SO, manually creating table

## Create a table of location counts
location_counts <- table(Place)

## Convert the table to a data frame
location_data <- as.data.frame(location_counts)

#### Rename the columns for better clarity
colnames(location_data) <- c("Place", "Count")

#### Order the data frame by count
location_data <- location_data[order(location_data$Count, decreasing = TRUE), ]

### Print the resulting data frame
print(location_data)

![image](https://github.com/ishanimahajan20/Job-Market-Explorer/assets/134215344/edadcf69-3b31-47c0-b1c5-c9e11452e193)


Internship.data$Duration <- gsub("Months", "", Internship.data$Duration)

## Create a histogram for internship durations
ggplot(Internship.data, aes(x = Duration)) +
  geom_histogram(fill = "coral", binwidth = 1, boundary = 0) +
  theme_minimal() +
  labs(title = "Duration Trends", x = "Duration", y = "Count")
Internship.data$Duration <- as.numeric(Internship.data$Duration)

![image](https://github.com/ishanimahajan20/Job-Market-Explorer/assets/134215344/8a6040b4-4dea-4946-bb19-10a68cf3e249)

## Create a boxplot for stipend analysis
ggplot(Internship.data, aes(y = Stipend)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Stipend Analysis", x = "", y = "Stipend")

  (THIS CODE IS NOT SUCCESSFUL AS STIPEND IS IN RANGE ), BUT THE OUTPUT IS

  ![image](https://github.com/ishanimahajan20/Job-Market-Explorer/assets/134215344/e0b5810f-2ddc-4c89-80c3-2b1a5a114ce1)


  ## Create a scatter plot for stipend vs. duration
ggplot(Internship.data, aes(x = Duration, y = Stipend)) +
  geom_point(color = "darkorange") +
  theme_minimal() +
  labs(title = "Stipend vs. Duration Analysis", x = "Duration", y = "Stipend")

  ![image](https://github.com/ishanimahajan20/Job-Market-Explorer/assets/134215344/19d45458-3573-4c3a-9b1f-05d0a4bf3497)

  ggplot(Internship.data, aes(x = Duration, y = Stipend)) +
  geom_col(color = "darkorange") +
  theme_minimal() +
  labs(title = "Stipend vs. Duration Analysis", x = "Duration", y = "Stipend")

  ![image](https://github.com/ishanimahajan20/Job-Market-Explorer/assets/134215344/eab38c6f-0e93-4da3-8173-a2bbe8e580b1)

install.packages("tm")
installed.packages("wordcloud")
library(tm)


### Text analysis on internship titles involves extracting insights from the text data in the internship title column.
'name' is the column with internship titles

corpus <- Corpus(VectorSource(Internship.data$name))
dtm <- DocumentTermMatrix(corpus)
word_freq <- colSums(as.matrix(dtm))

## Create a bar chart for word frequency
barplot(sort(word_freq, decreasing = TRUE)[1:10], col = "purple", main = "Top 10 Words in Internship Titles")

![image](https://github.com/ishanimahajan20/Job-Market-Explorer/assets/134215344/8e181c2c-fa53-44be-a053-80abc588d34f)

STEP TO STEP GUIDE : FOR THIS CODE 
Preprocessing:

Tokenization: Break down the titles into individual words or tokens.
Lowercasing: Convert all words to lowercase to ensure consistency.
Removing Stop Words: Eliminate common words (e.g., "and," "the") that don't carry significant meaning.
Stemming/Lemmatization: Reduce words to their root form for consistency.

Creating a Document-Term Matrix (DTM):

Convert the tokenized and preprocessed titles into a matrix format, where rows represent documents (internship titles) and columns represent unique words.

Word Frequency Analysis:

Calculate the frequency of each word in the document-term matrix. Identify the most common words.

Visualization:

Create visualizations such as bar charts or word clouds to represent the frequency of words. This provides a quick overview of the most prevalent terms.

### hypothesis testing 
(just my assumptions)

Hypothesis: Internships in Certain Locations Have Higher Stipends

Null Hypothesis (H0): The average stipend for internships is the same across all locations.

Alternative Hypothesis (H1): The average stipend for internships varies by location.

location_groups <- split(Internship.data$Stipend, Internship.data$Place)
result <- aov(location_groups)
summary(result)

my code has stipend in range so can't perform it


Hypothesis: Internships in Specific Categories Have Different Durations

Null Hypothesis (H0): The average duration for internships is the same across all place .

Alternative Hypothesis (H1): The average duration for internships varies by place .


Check the levels and counts of 'Place'

table(Internship.data$Place)         ----> This will display the counts for each level. If any level has very few observations, it might be a good idea to either combine categories or ensure that you have a reasonable amount of data for each level before conducting the ANOVA test.

Convert 'Place' to a factor with specified contrasts

Internship.data$Place <- factor(Internship.data$Place, levels = unique_levels)

Specify contrasts directly

contrasts(Internship.data$Place) <- contr.treatment(length(unique_levels))

Perform ANOVA test

result <- aov(Duration ~ Place, data = Internship.data)
summary(result)

Result: ![image](https://github.com/ishanimahajan20/Job-Market-Explorer/assets/134215344/702a0835-e289-4018-a71e-d5b40aa5a18f)

Based on this output, it does not appear that the 'Place' variable has a significant effect on the 'Duration' variable. The high p-value (0.589) suggests that there is not enough evidence to reject the null hypothesis, which posits that there are no differences in mean 'Duration' across different levels of 'Place'.




  





