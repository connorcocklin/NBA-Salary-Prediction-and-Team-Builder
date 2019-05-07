library(tidyverse)
library(data.table)
library(GGally)
library(PerformanceAnalytics)
library(plotly)
library(rvest)
library(stringr)
library(magrittr)
library(corrplot)
library(vif)

#reading in Data
seasonstats <- read.csv(file = "C:\\Users\\conno\\Desktop\\Data Science Class\\Final Project\\Seasons_Stats.csv", header = TRUE, sep = ",")

# Get salary dataset 
#Reading in salary dataset for year end 2017.
salary.table <- read.csv(file = "C:\\Users\\conno\\Desktop\\Data Science Class\\Final Project\\Salary1617.csv")

#cleaning data a tad, getting stats for year ending 2018.
stats17 <- 
  seasonstats %>% filter(Year >= 2017) %>% 
  select(Year:G, MP, PER, FG:PTS) %>% 
  distinct(Player, .keep_all = TRUE) %>% 
  mutate(MPG = MP/G, PPG = PTS/G, APG = AST/G, 
         RPG = TRB/G, TOPG = TOV/G, BPG = BLK/G, 
         SPG = STL/G,x3PaG = X3PA/G) 

#merging salary and stats into one table

Salary_Stats_2017 <- merge(stats17, salary.table,
                           by.x = "Player", by.y = "Player")
names(Salary_Stats_2017)[41] <- "Salary17"
Salary_Stats_2017<- Salary_Stats_2017[-39]
Salary_Stats_2017<- Salary_Stats_2017[-39]

#Reading in Advanced Statistics to join the party
#Get Advanced stats dataset
page <- read_html("https://www.basketball-reference.com/leagues/NBA_2017_advanced.html")
AdvancedStats.Table <- page %>% html_table(header = FALSE) %>% extract2(1)


#fixing headers and columns
names(AdvancedStats.Table) <- AdvancedStats.Table[1,]
AdvancedStats.Table <- AdvancedStats.Table[-1,]
AdvancedStats.Table <- AdvancedStats.Table[ , !names(AdvancedStats.Table) 
                                            %in% c("NA", "Tm", "PER", "MP", "Age")]

AdvancedStats.Table <- AdvancedStats.Table %>% filter(Rk!="Rk")


#changing column type to Numeric instead of Character
AdvancedStats.Table <- as.data.frame(AdvancedStats.Table)
AdvancedStats.Table <- as_tibble(AdvancedStats.Table)

AS1 <- AdvancedStats.Table %>% select(Rk, Player, Pos)
As2 <- AdvancedStats.Table %>% select(-Rk, -Player, -Pos)


As2 <- As2 %>% mutate_if(is.character,as.numeric)
AdvancedStats.Table <- bind_cols(AS1,As2)

AdvancedStats.Table = AdvancedStats.Table[!duplicated(AdvancedStats.Table$Player),]


#Merging data into Master data set

Master_Salary_Stats_2017 <- merge(Salary_Stats_2017, AdvancedStats.Table,
                           by.x = "Player", by.y = "Player")


#correlation 
library(corrplot)
correlations <- cor(Master_Salary_Stats_2017%>%
                      select(Salary17, MPG:SPG,
                             Age, PER, x3PaG,eFG.,contains("%")),
                    use = "complete.obs", method = "pearson")
corrplot(correlations, method = "number", type = "upper")
        
#Correlation hierarchy to Salary is PPG>MPG>TOPG>RPG=PER>APG=SPG>BPG>X3P>eFG
#The above is the first set of variables, because of level of correlation of X3pag and eFG I decided to read in more advanced statistics.
#These measures include value over replacement player and usage rate

correlations1 <- cor(Master_Salary_Stats_2017 %>%
                      select(Salary17, MPG:SPG,
                             Age, PER, WS, VORP,`USG%`,contains("%")),
                    use = "complete.obs", method = "pearson")
corrplot(correlations1, method = "number", type = "upper")

#We see that the advanced stats add some explaination to the bigger picture of what drives salary.
#Here TOV% corrects for where TOPG seemed to moderately positively correlate with pay while widly being considered a negative play
#If we take all factors of with correlation higher than .3 we have a hierarchy that looks like:
# PPG>MPG>WS>VORP>RPG>APG>SPG>PER>USG%>AGE

#Scatter Plot with regression line
Master_Salary_Stats_2017 %>%
  ggplot(aes(x = PPG, y = Salary17)) + 
           geom_point() +
           geom_smooth(method = "lm")

Master_Salary_Stats_2017 %>%
  ggplot(aes(x = MPG, y = Salary17)) +
    geom_point() +
  geom_smooth(method = "lm")

Master_Salary_Stats_2017 %>%
  ggplot(aes(x = VORP, y = Salary17)) +
  geom_point() +
  geom_smooth(method = "lm")

#Regression Analysis


#Regression based on per game stats
SAL_lmGM = Master_Salary_Stats_2017 %>%
  select(Salary17, MPG:SPG)
PERGAME_MODEL <- lm(log(Salary17) ~., data = SAL_lmGM)
summary(PERGAME_MODEL)

#Kicthen Sink significant at MP, FG, FGA, ORB, DRB, AST, MPG, PPG, APG, RPG,
#AST%, TOV%, will move to create best fit model

SAL_BEST = Master_Salary_Stats_2017 %>%
  select(Salary17, MP, FG, FGA, ORB, DRB, AST, MPG, PPG, APG, RPG, `AST%`,`TOV%`)
LM_BestFIT <- lm(Salary17~., data = SAL_BEST)
summary(LM_BestFIT)

#DIDNOT PRODUCE 
#WILL NOW GUESS WHICH STATS PRODUCE BEST MODEL
SAL_GUESS = PractiveDropLowSalaries %>%
  select(Salary17, MPG, PPG, APG, RPG, WS, VORP, X3P., Age)
LM_GUESS <- lm(log(Salary17)~., data = SAL_GUESS)
summary(LM_GUESS)

car::vif(LM_GUESS)



#accessing residuals and createing seperate df
#removing NAs in data
PracticeNAremove <- Master_Salary_Stats_2017
PracticeNAremove <- PracticeNAremove %>% drop_na("MPG","PPG", "APG", "RPG", "WS", "VORP", "X3P.","Age")

PractiveDropLowSalaries <- PracticeNAremove[!(PracticeNAremove$Salary17 <=200000),]

Practice <- PractiveDropLowSalaries %>% select(Player, Pos.y, Salary17, WS)
Practice$LogSalary17 <- log(Practice$Salary17)

Practice$DifferenceSalary17 <- residuals(LM_GUESS)
Practice <- Practice %>% select(-PredictedSalary17)

summary(LM_GUESS)
library(stargazer)
stargazer(LM_GUESS, type = "latex", out = "LM_Kitchen.txt",
          no.space = TRUE, single.row = TRUE, title = "Summary Table", omit.stat = "f","ll","ser")




