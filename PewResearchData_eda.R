library(RSQLite)
library(dplyr)
library('XML')
library("methods")
library(ggplot2)


gaming_csv <- read.csv("June 10-July 12, 2015 - Gaming, Jobs and Broadband - CSV.csv")
print(gaming_csv)

homeinternet_dialupuser_df <- select(gaming_csv, state, bbhome1, bbhome2)
homeinternet_dialupuser_df1 <- na.omit(homeinternet_dialupuser_df)
homeinternet_state_dialup <- arrange(homeinternet_dialupuser_df1, desc(state))
#isolate the states and their homeinternet by dialup usage

homeinternet_state_dialup[!duplicated(homeinternet_state_dialup$state), ]
homeinternet_state_dialup
#remove duplicated states

smartphoneuser_gamer <- select(gaming_csv, date2a, game4)
smartphoneuser_gamer_df1 <- na.omit(smartphoneuser_gamer)
smartphoneuser_gamer_likely <- arrange(smartphoneuser_gamer_df1, date2a)
#if you are a smartphone user, are you likely to play video games

colnames(smartphoneuser_gamer_likely)[1] <- "smartphoneuser"
colnames(smartphoneuser_gamer_likely)[2] <- "gamer"


ggplot(data=smartphoneuser_gamer_likely, aes(x=smartphoneuser, y=gamer, group=1)) +
  geom_line()

student_marital_df <- select(gaming_csv, stud, marital)
student_marital_df1 <- na.omit(student_marital_df)
student_married <- arrange(student_marital_df1, marital)
#based on your marital status, are you a student or not?

colnames(student_married)[1] <- "studentornot"
colnames(student_married)[2] <- "maritalstatus"

ggplot(data=student_married, aes(x=maritalstatus, y=student ornot, )) +
  geom_bar(colour="black", stat="identity")

hispanic_political <- select(gaming_csv, hisp, ideo)
hispanic_political_df1 <- na.omit(hispanic_political)
hispanic_politicalaffiliation <- arrange(hispanic_political, hisp)
#if they are of hispanic or latino origin, what are their political views

colnames(hispanic_politicalaffiliation)[1] <- "hispanic_or_not"
colnames(hispanic_politicalaffiliation)[2] <- "what_are_your_political views"
#change the name of the columns

source_table = table(hispanic_politicalaffiliation)
pie(source_table[source_table < 2])
#based on if theyre hispanic or not, pie graph of what their political views are

birth_race_df <- select(gaming_csv, birth_hisp, race)
birth_race_df1 <- na.omit(birth_race_df)
puertoOrUSA_race <- arrange(birth_race_df1, birth_hisp)
#whether born in USA or puerto Rico, what race are they?

colnames(puertoOrUSA_race)[1] <- "born_in_USA"
colnames(puertoOrUSA_race)[2] <- "Whatraceareyou"
#change name of columns

plot <- ggplot(puertoOrUSA_race, aes(born_in_USA, Whatraceareyou))
plot + geom_boxplot()
plot + geom_boxplot() + geom_jitter(width = 0.2)
#box plot of those born in USA or not and what race they are

job <- select(gaming_csv, job2, age)
jobandage <- na.omit(job)
jobandage1 <- arrange(job, age)
colnames(jobandage1)[1] <- "doyouwork"
colnames(jobandage1)[2] <- "howoldareyou"
ggplot(data=jobandage1, aes(x=doyouwork, y=howoldareyou, group=1)) +
  geom_line()
#how old are you and do you work or not
