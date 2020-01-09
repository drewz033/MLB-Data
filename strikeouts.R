#import the necessary packages

library(dplyr)
library(Lahman)
library(ggplot2)
library(stringr)


#extract teams data from Lahman database
teams<-data('Teams')



#create vector of recent years to analyze
years<-c('2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018')



#filter team dataframe for recent years, rename to batting
batting<-Teams[is.element(Teams$yearID, years),]


#We can view strikeouts by years and sum the total
strikeout_view<-batting%>% 
  group_by(yearID) %>% 
  summarise(TotalSO = sum(SO))

#visualize blox plot for strikeout statistics
boxplot(TotalSO~yearID,data=strikeout_view, main="Strikeouts",
        xlab="Year", ylab="SO")


#upload swings data from fangraphs
swings<-read.csv('swings.csv')


#summarise plate discipline metric including in zone and outside zone swings by season
swing_avg<-swings%>% 
  group_by(swings$ï..Season) %>% 
  summarise(Swing.Outside.Zone = mean(O.Swing.),Swing.In.Zone=mean(Z.Swing.),Outside.Zone.Contact=mean(O.Contact.),
            Inside.Zone.Contact=mean(Z.Contact.),Swing.Pct=mean(Swing.),Contact.Pct=mean(Contact.),
            Pitches.In.Zone=mean(Zone.))



#Filter the batting df for just strikeouts, year and team name

strikeouts<-batting[,c('yearID','SO','name')]

# to match team description names in both strikeout and swings data, need to strip out the second workd in strikeouts df

strikeouts$name<-word(strikeouts$name, -1)


#renaming columns
swings<-swings%>%rename(Swing.Outside.Zone=O.Swing.,Swing.In.Zone=Z.Swing.,Outside.Zone.Contact=O.Contact.,
                        Inside.Zone.Contact=Z.Contact.,Swing.Pct=Swing.,Contact.Contact.Pct=Contact.,
                        Pitches.In.Zone=Zone.)


#renaming columns
colnames(swings)[1] <- "Season"
colnames(swings)[2]<-'Team'



#filter swing data for only the needed data as well as seaon and team name
swing_data<-swings[,c('Season','Team','Swing.Outside.Zone','Swing.In.Zone','Outside.Zone.Contact','Inside.Zone.Contact','Pitches.In.Zone','Swing.Pct')]

#rename columns to match swing data dataframe
colnames(strikeouts)[3]<-'Team'
colnames(strikeouts)[1]<-'Season'




#create a comparison dataframe by merging swing data and strikeouts
comparison_df<-merge(swing_data,strikeouts,by=c('Season','Team'))



#we want to see data in percents, not decimal form to better understand regression
comparison_df$Pitches.In.Zone<-comparison_df$Pitches.In.Zone*100
comparison_df$Inside.Zone.Contact<-comparison_df$Inside.Zone.Contact*100
comparison_df$Swing.Outside.Zone<-comparison_df$Swing.Outside.Zone*100
comparison_df$Swing.Pct<-comparison_df$Swing.Pct*100
comparison_df$Outside.Zone.Contact<-comparison_df$Outside.Zone.Contact*100


#view the correlations of swing type, of course we are expecting to see some highly correlated variables here

cor(comparison_df[,3:7])

#run the regression with pitches in the zone and swings in the zone as independent variables
regression<-lm(formula=SO ~ Pitches.In.Zone+Swing.In.Zone, data=comparison_df)


#view regression out
summary(regression)


#run prediction model by splitting the data into train and test set, split on dependent variables strikeouts
library(caTools)
set.seed(123)
split = sample.split(comparison_df$SO, SplitRatio = 0.8)
training_set = subset(comparison_df, split == TRUE)
test_set = subset(comparison_df, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula=SO ~ Pitches.In.Zone+Swing.In.Zone,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

summary(regressor)


y_true=test_set[,'SO']

results<-cbind(y_true,y_pred)


#run brusch pagan test to ensure homoskedacticity
lmtest::bptest(regressor) 


