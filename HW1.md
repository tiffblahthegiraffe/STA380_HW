---
title: 'STA 380 Homework 1'
author: "Henry Chang, Tiffany Sung, Joseph Chin"
date: "8/8/2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Problem 1 Probability Practice

#### Part A
Given the following:<br>
Prob(RC) = 0.3<br>
Prob(Yes|RC) = Prob(No|RC) = 0.5<br>
Prob(TC) = 0.7<br>
Prob(Yes) = 0.65<br>
Prob(No) = 0.35<br>

We want to know Prob(Yes|TC) = ?

Solution:<br>
Prob(Yes) = Prob(Yes|RC) $*$ Prob(RC) + Prob(Yes|TC) $*$ Prob(TC)<br>
0.65 = 0.5  $*$ 0.3 + Prob(Yes|TC)  $*$ 0.7
```{r, collapse=TRUE}
(0.65-0.3*0.5) / 0.7
```


#### Part B
The probability of having disease given test positive:<br>
D: having disease, ND: not having disease<br>
P: getting postivie in the test, N: getting negative in the test<br>

P(D|P) = P(P|D) $*$  P(D) / ( P(P|D) $*$  P(D) + P(P|N) $*$  P(N) )
```{r, collapse=TRUE}
(0.993) * 0.000025 / ((0.993) * 0.000025 + 0.0001 * 0.999975)
```
Problem of the test lies in having too many "False Positive". Which is giving too many positive on tests for those who don't have a desease. This kind of implementing a universal testing policy for the disease will lead to panic and chaos.



#Problem 2 Green Buildings
```{r, collapse=TRUE}
library(corrplot)
library(ggplot2)
plot_data = read.csv(url("https://raw.githubusercontent.com/jgscott/STA380/master/data/greenbuildings.csv"))
#colnames(plot_data)
re_order_var = c("Rent", "green_rating", "LEED","Energystar", "size", "stories", "class_a",
 "class_b", "amenities",  "age", "renovated", "cd_total_07", "hd_total07","total_dd_07", "Precipitation","Gas_Costs" , "Electricity_Costs", "net")
```
After reading our data into R. I reordered the variables by their categories to make correlation
plots easier to read. Our new variable list starts with rent, green building certificate, characteristics of a building, weather indicators, ends with energy and other cost.



#### Variable correlation plot
```{r, collapse=TRUE}
corrplot(cor(plot_data[re_order_var]), type = "lower")
```


The correlations seems reasonable: rent postiviely correlated to good characteristics of a building
and negatively correlated to worse building conditions and less favorable weather conditions.<br> However, "cluster_rent" provides aggregated information by cluster. Which cannot be included in our correlation plot but provides valuable information.<br>

Therefore, we created varaiable "Rent_adjust" by dividing rent using its cluster's rent. Which tells us the house's rent proportional to the cluster's rent.


```{r}
plot_data["Rent_adjust"] = plot_data["Rent"] / plot_data["cluster_rent"]
corrplot(cor(plot_data[c("Rent_adjust", re_order_var[-1])]), type = "lower")
```


After replacing Rent using "Rent_adjust", we found that correlations between "Rent_adjust" and weather related variables has dissappeared. Which seems to tell us that the effect of weather on rent are reflected on the cluster's rent.



**Examine rent difference between green and non-green buildings having similar characteristic with our new building.**

After we have studied our variables, we look into information we have on our new building :<br>
size, age, and stories to help us decide whether we should build a green building.<br>

#### Size
```{r}
size_sd = sd(plot_data$size)
ggplot(plot_data, aes(x = size)) +
  geom_histogram(binwidth = 10000) +
  geom_histogram(data=subset(plot_data, green_rating == "1"),binwidth = 10000,fill = "green", alpha = 0.2) +
  labs( x = "Size in square footage", y ="No of Buildings" ) +
  geom_vline(aes(xintercept = 250000, color = "My Building")) +
  geom_vline(aes(xintercept = 250000 + size_sd, color = "upper") , linetype = "dashed") +
  geom_vline(aes(xintercept = 250000 - size_sd, color = "lower"), linetype = "dashed") +
  scale_color_manual(name="Used interval", values=c(`My Building` = "red", upper = "orange", lower = "orange"))
```


We plot the "size" variable's distribution of our entire data along with the green building's size distribution colored in green. The vertical red line shows the our building's size : 250000. And the orange dash line shows interval of one standard deviation, in which are the data points we are carrying to our boxplot.

```{r}
size_explore = plot_data[plot_data$size <=  250000 + size_sd, ]
size_explore = size_explore[size_explore$size >= 250000 - size_sd, ]
size_explore$green_rating = as.factor(size_explore$green_rating)
ggplot(size_explore, aes(x = green_rating, y = Rent) ) + 
  geom_boxplot(fill = c("grey", "green")) + 
  #scale_y_continuous(limit = c(0, 50)) +  
  labs(x = "Building", y = "Rent") 
```


Using the data points having less than one standard deviation on "size" with our new building, we plotted the rent difference between green and non-green buildings.


```{r, warning= FALSE}
size_explore = plot_data[plot_data$size <=  250000 + size_sd, ]
size_explore = size_explore[size_explore$size >= 250000 - size_sd, ]
size_explore$green_rating = as.factor(size_explore$green_rating)
ggplot(size_explore, aes(x = green_rating, y = Rent) ) + 
  geom_boxplot(fill = c("grey", "green")) + 
  scale_y_continuous(limit = c(0, 50)) +  # 363 among 6998 have rent > 50
  labs(x = "Building", y = "Rent") 
```


We zoom in our plot by resetting Y limit. We found that 25, 50, 75 rent quantile of green buildings are all higher than non-green buildings.


#### Age
```{r, warning= FALSE, message= FALSE}
age_sd = sd(plot_data$age)
ggplot(plot_data, aes(x = age)) +
  geom_histogram() +
  geom_histogram(data=subset(plot_data, green_rating == "1"),fill = "green", alpha = 0.2) +
  labs( x = "House age", y ="No of Buildings" ) +
  geom_vline(aes(xintercept = 0, color = "My Building"), size = 1) +
  geom_vline(aes(xintercept = 0 + age_sd, color = "upper") , linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = 0 - age_sd, color = "lower"), linetype = "dashed", size = 1) +
  scale_color_manual(name="Used interval", values=c(`My Building` = "red", 
                                                    upper = "orange", 
                                                    lower = "orange"))
```


We plot the "age" variable's distribution along with "age"'s distribution among green buildings. We found new buildings (age = 0) only consist of a small portion of our data.

```{r, warning= FALSE}
age_explore = plot_data[plot_data$age <=  age_sd, ]
age_explore$green_rating = as.factor(age_explore$green_rating)
ggplot(age_explore, aes(x = green_rating, y = Rent)) + 
      geom_boxplot(fill = c("grey", "green")) + 
      scale_y_continuous(limit = c(0, 50)) + #173 among 3740 have rent > 50
      labs(x = "Building", y = "Rent")  
```


Green buildings have higher rents in relatively new buildings.


####Stories
```{r, warning= FALSE, message= FALSE}
stories_sd = sd(plot_data$stories)
ggplot(plot_data, aes(x = stories)) +
  geom_histogram() +
  geom_histogram(data=subset(plot_data, green_rating == "1"),fill = "green", alpha = 0.2) +
  labs( x = "House stories", y ="No of Buildings" ) +
  geom_vline(aes(xintercept = 15, color = "My Building"), size = 1) +
  geom_vline(aes(xintercept = 15 + stories_sd, color = "upper") , linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = 15 - stories_sd, color = "lower"), linetype = "dashed", size = 1) +
  scale_color_manual(name="Used interval", values=c(`My Building` = "red", 
                                                    upper = "orange", 
                                                    lower = "orange"))

```


Under the distribution of "stories", we can see that our new buildings story is around 70 quantile among our data set.

```{r, warning= FALSE}
stories_explore = plot_data[plot_data$stories <= 15 + stories_sd, ]
stories_explore = stories_explore[stories_explore$stories >= 15 - stories_sd, ]
stories_explore$green_rating = as.factor(stories_explore$green_rating)
ggplot(stories_explore, aes(x = green_rating, y = Rent)) + 
  geom_boxplot(fill = c("grey", "green")) + 
  scale_y_continuous(limit = c(0, 50)) +  #173 among 3740 have rent > 50
  labs(x = "Building", y = "Rent")  
```


Green buildings can bring higher rent in our stories interval.


####Validation using regression


On plots above we obsereved higher rents from green buildings under different condition controled. Before coming up with our final advice, it's always good to check the result of linear regression, which is good at controling variables we consider might affect our rent.

We used Rent_adjust as dependent variable to control the effect from clusters. Then added class_a to control the raised rent from high-quality buildings .
```{r}
summary(lm( Rent_adjust ~ log(size) + age + stories+ class_a +green_rating ,
            data = plot_data))
```

Green building provides  3.3% increase on local cluster rent per square foot. (Our building 250,000 square foot, extra cost for green: 5,000,000)

#Problem 3 Bootstrapping

```{r results='hide', message=FALSE, warning=FALSE}
library(mosaic)
library(quantmod)
library(foreach)
library(GGally)
```

```{r, message=FALSE, warning=FALSE, collapse= TRUE}
# Import the five ETFs
mystocks = c("SPY", "TLT", "LQD", "EEM", "VNQ")
myprices = getSymbols(mystocks, from = "2007-01-01")
getSymbols(mystocks)

#Get the adjusted closing price of each ETF
for(ticker in mystocks) {

  expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
  eval(parse(text=expr))
}

# Combine all the returns in a matrix
all_returns = cbind(ClCl(LQDa),ClCl(TLTa),ClCl(SPYa), ClCl(VNQa), ClCl(EEMa))
all_returns = as.matrix(na.omit(all_returns))

head(all_returns)
```

```{r}
# The sample correlation matrix
cor(all_returns)
```


From the correlation plot, we can see that SPY is highly correlated with VNQ and EEM which indicates that emerging-market and real estate relatively more dominant by the market condition. Also, we noticed that TLT is negatively correlated with SPY. This can be explained as when the stock market goes up, people tend to sell treasury bonds to buy stocks, and when it goes down, people buy treasury bonds because they are safe.


```{r}
ggpairs(as.data.frame(all_returns))+theme_bw()
```

From the mean and variance of each ETFs, we can see that EEM, VNQ have relatively higher return, but higher risk consequently.
Thus, we can then assume EEM and VNQ is a risker investment compare to the other ETFs.
This is also very intuitive because investing in bonds is less risky than investing in emerging-market or real estate which are both highly volatile.


```{r}
#The mean and variance
sort(apply(all_returns,2,mean),decreasing = TRUE)
cat("\n")
sort(apply(all_returns,2,var),decreasing = TRUE)
```

```{r}
par(mfrow=c(2,3))
hist(all_returns[SPYa], 30, xlim=c(-0.04, 0.04))
hist(all_returns[TLTa], 30, xlim=c(-0.04, 0.04))
hist(all_returns[LQDa], 30, xlim=c(-0.04, 0.04))
hist(all_returns[EEMa], 30, xlim=c(-0.04, 0.04))
hist(all_returns[VNQa], 30, xlim=c(-0.04, 0.04))
```


**Evenly Weighted Portfolio** <br>
First, we construct a portfolio which the initial wealth is evenly distributed into each ETFs.
```{r}
set.seed(1)
initial_wealth = 100000
sim_even_split = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.2, 0.2, 0.2, 0.2, 0.2) #seperate evenly
  holdings = weights * total_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today #p+r(stock)
    total_wealth = sum(holdings)#p+r(portfolio)
    wealthtracker[today] = total_wealth
    holdings = total_wealth * weights #daily rebalance
  }
  wealthtracker
}
```

With an even weighted portfolio, we have a 0.9% return and 20-day 5% VaR at -6300.89  
```{r}
even_mean <- mean(sim_even_split[,n_days])
even_return <- (mean(sim_even_split[,n_days])-initial_wealth)/initial_wealth
even_5_perc <- quantile(sim_even_split[,n_days], 0.05) - initial_wealth

# Print out results
cat("For an even split investment of $100,000,\nExpected holdings:", even_mean, "\nReturn Rate:", even_return, "\n5% VaR:", even_5_perc)
```



**Safer Portfolio**<br>
Then, according to the mean and variance analysis of each ETFs, we want to construct a more risk-adverse portfolio by weighted toward the less-volatile ETFs for instance, SPY,TLT and LQD. We decided not to put any weight on EEM and VNQ because accoding to the correlation plot, these two ETF has relatively higher correlation with SPY which is the market index. If we put them in the portfolio, it would not diverse and reduce the risk.
```{r}
set.seed(1)
initial_wealth = 100000
sim_safe_split = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.5, 0.3, 0.2, 0, 0) # 0.2 SPY, 0.3 TLT, 0.5 LQD
  holdings = weights * total_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
    holdings = total_wealth * weights
  }
  wealthtracker
}
```

With a safer portfolio, we have a lower 20-day 5% VaR at -3002.014, however, this also means a lower return at 0.5%.
```{r}
safe_mean <- mean(sim_safe_split[,n_days])
safe_return <- (mean(sim_safe_split[,n_days])-initial_wealth)/initial_wealth
safe_5_perc <- quantile(sim_safe_split[,n_days], 0.05) - initial_wealth

# Print out results
cat("For an safer split investment of $100,000,\nExpected holdings:",safe_mean, "\nReturn Rate:", safe_return, "\n5% VaR:", safe_5_perc)
```



**Aggressive Portfolio**<br>
For the aggressive portfolio, we merely place our initial wealth on the risker investment EEM and VNQ. We did not place any holdings on the other ETFs because if we added them in the portfolio, it would diversify the risk and make the portfolio less aggressive.
```{r}
set.seed(1)
initial_wealth = 100000
sim_risky_split = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0, 0, 0, 0.3, 0.7) #0.7 EEM, 0.3 VNQ
  holdings = weights * total_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
    holdings = total_wealth * weights
  }
  wealthtracker
}
```

With an aggressive portfolio, we got a 1.65% return but follow by a really high 20-days 5% VaR at -12596.79
```{r}
risky_mean <- mean(sim_risky_split[,n_days])
risky_return <- (mean(sim_risky_split[,n_days])-initial_wealth)/initial_wealth
risky_5_perc <- quantile(sim_risky_split[,n_days], 0.05) - initial_wealth

# Print out results
cat("For a risky split investment of $100,000,\nExpected holdings:", risky_mean, "\nReturn Rate:", risky_return, "\n5% VaR:", risky_5_perc)
```

With the histogram below, again, we prove that portfolio that place higher proportions on ETFs with higher variance could be more volatile. The 'risk' portfolio has more chances of getting higher returns and also more loss. The 'safe' is the lower-volatility portfolio and as expected, it has lower chances of getting big profit or big loss.

**Plot Histogram of Portfolios**
```{r, message=FALSE, warning=FALSE}
even = sim_even_split[,n_days]- initial_wealth
safe = sim_safe_split[,n_days]- initial_wealth
risk = sim_risky_split[,n_days]- initial_wealth

plot_df = data.frame(risk,even, safe)

ggplot(data = plot_df) + 
    geom_histogram(aes(risk,color = 'risk'), fill="red4",alpha = 0.9,position="identity")+
    geom_histogram(aes(even,color = 'even'),fill = 'salmon1',alpha = 0.6,position="identity") + 
    geom_histogram(aes(safe,color = 'safe'),fill = 'white',alpha = 1.0,position="identity")+
    xlim(c(-25000,60000))+
    xlab('Portfolio Profit/Loss Spread')+
    ylab('Frequency')+
    scale_colour_manual("", 
                      breaks = c("risk", "even", "safe"),
                      values = c("salmon1", "red4","olivedrab4"))
```

**Evaluate Portfolio Performance**
```{r}
get_return = function(initial_wealth,weights,all_returns){
    
    sim_split = foreach(i = 1:5000, .combine = 'rbind') %do% {
        total_wealth = initial_wealth
        holdings = weights * total_wealth
        n_days = 20
        wealthtracker = rep(0, n_days)
        for(today in 1:n_days) {
            return.today = resample(all_returns, 1, orig.ids=FALSE)
            holdings = holdings + holdings*return.today #p+r(stock)
            total_wealth = sum(holdings)#p+r(portfolio)
            wealthtracker[today] = total_wealth
            holdings = total_wealth * weights #daily rebalance
        }
        wealthtracker
    }
    return(quantile(sim_split[,n_days], 0.05) - initial_wealth)
}  

#weight_even = rep(0.2,5)
#even = get_return(100000,weight_uni,all_returns)
```

Once again, by randomly tuning the weight on those 5 ETFs, we can observe that if placing more holdings on the higher-volatility ETFs, we would expect it to lose more money of the portfolios during the worst 5% of possible 20-day periods.
```{r}
set.seed(1)
weight = runif(5)
weight = weight / sum(weight)
for (i in 1:10){
    weight = runif(5)
    weight = weight / sum(weight)
    initial_wealth = 100000
    VaR = get_return(initial_wealth,weight,all_returns)
    cat('The portfolio weight is: ')
    print(weight)
    cat('VaR of the portfolio is: ')
    print(VaR)
}
```


#Problem 4 Market segmentation

```{r,results='hide', message=FALSE, warning=FALSE}
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(corrplot)

```

####Data Exploration
```{r}
# read file
social <- read.csv(url("https://raw.githubusercontent.com/jgscott/STA380/master/data/social_marketing.csv"), row.names = 1)
#drop columns spam, adult, and uncategorized from file
social = subset(social, select = -c(spam,adult,uncategorized))
```

```{r}
corrplot(cor(social), type = "lower")
```


From the correlation plot, we can identidy several variables with high correlation.<br>
For example: personal_fitness and health_nutrition.<br>


```{r}
knitr::include_graphics("https://raw.githubusercontent.com/tiffblahthegiraffe/STA380_HW/master/plot.png")
```

With excel, we looked further into the correlations between variables, and decided to manually group several highly correlated variables together into six hypothetical groups/clusters, as displayed below:
```{r}
knitr::include_graphics("https://raw.githubusercontent.com/tiffblahthegiraffe/STA380_HW/master/group.png")

```


Let's dig into each group a little more:<br>
Group 1 is influencer, whom can be seem as the social media savvy millennials that like sharing their lifestyle online<br>
Group 2 represents businessman, a group of corporate people that enjoy politics, news, and business
Group 3 are artists that talks about art, music and films on social media<br>
Group 4 captures familyguy. These people are very family oriented, with interests including religion, family, and parenting<br>
Group 5 is "dude." Think about a college male that likes online gaming and sports.<br>
Group 6 represents "fit." These people care about their fitness, shape, nutrition consumption, and hope to stay active<br>


Now, with six hypothetical groups, we want to use clustering to see if our correlation-based groups serve as a group proxy as NutrientH20's market segmentation

####K Means

```{r}
#create hypothetical groups in dataframe
social["influencer"] = social$chatter+social$photo_sharing+social$shopping+social$current_events+social$dating + social$cooking+social$beauty+social$fashion
social["businessman"] = social$travel + social$politics +social$computers +social$news + social$automotive +social$business
social['artists'] = social$tv_film +social$art +social$music +social$crafts +social$small_business
social['familyguy'] = social$sports_fandom + social$religion +social$parenting +social$school +social$food +social$family +social$home_and_garden
social['dude'] = social$online_gaming +social$college_uni +social$sports_playing
social['fit'] =social$outdoors+social$health_nutrition+social$personal_fitness+social$eco
```

```{r}
#select six groups and form new dataframe
social_new = social[, c(34:39)]

#normalize social_new for better comparison between rows
social_norm = social_new/rowSums(social_new)

#scale social_norm
social_scaled <- scale(social_norm, center=TRUE, scale=TRUE)
```

```{r}
#select K
set.seed(123)
fviz_nbclust(social_scaled, kmeans, method = "wss")
```

From elbow method, it is concluded that k=6 gives us best clustering result

```{r,message=FALSE,warning=FALSE}
set.seed(123)
fviz_nbclust(social_scaled, kmeans, method = "silhouette")

```


Again, K=6 returns best clusting result.

To triple check, we calculate CH<br>

```{r,message=FALSE,warning=FALSE}
set.seed(123)
for (i in 2:10){
  final <- kmeans(social_scaled, centers = i , nstart = 25)
  B = final$betweenss
  final$withinss
  W = final$tot.withinss
  B/W
  n = nrow(social_scaled)
  k=i
  CH = (B/(k-1))/(W/(n-k))
  cat("k=", k, ", CH:", CH, "\n")
  
}
```


When K=6, CH reches its max of 3358.518<br>
From the methods above, we get a preliminary idea that our six manually selected groups might be a good proxy for market segmentation

```{r}
set.seed(123)
#use K=6 to run kmeans
final <- kmeans(social_scaled, centers =6 , nstart = 25)
```
```{r}
set.seed(123)
#display centers of seven clusters to see how they are allocated
print(final$centers)
```


From the result, we can see that our hypothetical grouping method works well!<br>
Each of the six groups represents a distinct demographics.<br>


Cluster 1 represents "artists." Variables include: tv_film, art, music, crafts, small_business<br>

Cluster 2 is "fit." These people are the outdoor enthuiast that care about health_nutrition, outdoors, personal_fitness, and eco.<br>

Cluster 3 centers on "dude," a proxy for people who enjoy topics like online_gaming, college_uni, sports_playing.<br>

Cluster 4 centers on "familyguy." This category captures people who are "family oriented" and enojoy taking about topics like sports_fandom, religion, parenting, school, food, family, and home_and_garden.<br>

Cluster 5 centers heavily on the businessman group, which includes people who like talking about topics like travel, travel, politics, computers, news, automotive, business.<br>

Cluster 6 captures "influencers," which can be thought as the millennials that like sharing lifestyle related topics on social media. Topics include:photo_sharing, shopping, current_events, dating, cooking, beauty, fashion.<br>

```{r}
#lets see how many people belong in each group with this line of code:
final$size
```

cluster 1 (artists) has 841 people<br>
cluster 2 (fit) has 1362 people<br>
cluster 3 (dude) represents 627 people<br>
cluster 4 (familyguy) includes 1133 people<br>
cluster 5 (businessman) has 1205 people<br>
cluster 6 (influencer) includes 2714 people<br>
```{r}
#kmeans plot to show 6 clusters 
fviz_cluster(final, data = social_scaled, ellipse.type = "norm", stand = TRUE, geom = "point")
```

The plot shows that our six groups are separated in a fairly clear fashion. 

####PCA

Now, let us move on to PCA to further analyze our hypothetical groups.

```{r,warning=FALSE,message=FALSE}
#Run PCA with 5 ranks
pc1 = prcomp(social_norm, scale=TRUE, rank=5)
loadings = pc1$rotation
scores = pc1$x
```

```{r}
#several biplots show the first two PCs and how these groups are segmented

q1 = qplot(scores[,1], scores[,2], color= social_norm$influencer , xlab='Component 1', ylab='Component 2')
q2 = qplot(scores[,1], scores[,2], color = social_norm$businessman, xlab='Component 1', ylab='Component 2')
q3 = qplot(scores[,1], scores[,2], color = social_norm$artists, xlab='Component 1', ylab='Component 2')
q4 = qplot(scores[,1], scores[,2], color = social_norm$familyguy, xlab='Component 1', ylab='Component 2')
q5 = qplot(scores[,1], scores[,2], color = social_norm$dude, xlab='Component 1', ylab='Component 2')
q6 = qplot(scores[,1], scores[,2], color = social_norm$fit, xlab='Component 1', ylab='Component 2')
```


These plots showcase where each hypothetical group belong in the PCA two-domention result. A plot represent a twitter user, and the more red it is, the the more percentage of the user's posts relate to the corresponding group.<br>
In other words, the more red it is, the more the user belongs to a hypothetical group we created.<br>

**The influencer**
```{r}
#influencer
q1+scale_color_gradient(low="ivory", high="red")
```

This plot shows where influencer sits in this dimension.

**The businessman**
```{r}
#Businessman
q2+scale_color_gradient(low="ivory", high="red")
```

This plot shows  businessman (red) versus others (white).

**The artist**
```{r}
#Artists
q3+scale_color_gradient(low="ivory", high="red")
```

This plot points out those where are artists are. 

**The familyguy**
```{r}
#Familyguy
q4+scale_color_gradient(low="ivory", high="red")
```

This plot identifies the familyguy group.

**The dude**
```{r}
#Dude
q5+scale_color_gradient(low="ivory", high="red")
```

This plot shows where "dude" sit related to others

**The fit**
```{r}
#Fit
q6+scale_color_gradient(low="ivory", high="red")
```

This plot clearly identifies the "fit" group


**Interpretation: 
component1 does a great job separating influencer (right) from familyguy and businessman (left of graph)
Component 2 is great at separating "fit" (upper) from  "artists" and "dude" (lower)

```{r}
o1 = order(loadings[,1], decreasing=TRUE)
colnames(social_norm)[head(o1,2)]

```

from the formula below, it is clear that PC1, 2 has the ability to separate influencer and fit out from the rest of the data; result aligns with plots

```{r}
loadings
```
Looking at the vectors, we attain the same results.


####Conclusion 

From kmeans clustering and PCA, we can conclude that our hypothecial grouping method works very well in identifying users with different interests on social media, or "socialgraphics." This output can help NutrientH20 better target its audience and focus their social media marketing efforts on a more defined and targeted group of people. 

Once again, the groups, variables included, and number of peolpe in each group:
```{r}
knitr::include_graphics("https://raw.githubusercontent.com/tiffblahthegiraffe/STA380_HW/master/final.png")
```