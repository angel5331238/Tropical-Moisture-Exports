---
title: "HW4 Problem 2"
author: "An-Chi Ho, Sruti Devendran, & Miriam Nielsen"
date: "`r Sys.Date()`"
output:
  rmdformats::html_docco:
    highlight: kate
    toc: true
    toc_depth: 3
---

```{r knitr_init, echo=FALSE, cache=FALSE}
# DO NOT edit this block
knitr::opts_chunk$set(
  cache=TRUE,
  comment=NA,
  message=FALSE,
  warning=FALSE,
  fig.width=15,
  fig.height=10
)
```


Package Installation:

```{r}
if(!require(pacman)) install.packages('pacman')

pacman::p_load(dplyr, ggplot2, mapproj, readr, ggthemes, viridis, reshape2, ggmap)

```


1. Read in the location data. Create a map of the site locations using an appropriate projection. (2 pts)
```{r}
loc <- read_csv('location_data.csv')
glimpse(loc)

qmplot(lon, lat, data=loc) +
  coord_map(projection="lambert", parameters = c(25, 50)) +
  ggtitle("Site Locations", subtitle = "with Lambert Projection") +
  theme_few() 


```

2. Read in the prediction data frame. Plot the TME data as a function of time. (3 pts)
```{r}
pred <- read_csv('prediction_data.csv')
glimpse(pred)

ggplot(pred, aes(x=year, y=TME))+
  geom_point() +
  geom_line() +
  ggtitle("Annual Tropical Moisture Exports Index (TME)\n") 

```

Here we can see that the annual Tropical Moisture Exports (TME) for this region varies (between a little less than -1 to a little more than +2) from year to year.


3. Sum the number of extreme events over all sites, by year (hint: use dplyr :: groupby()). Plot the number of extreme events and the TME data. Does this match Steinschneider and Lall [2015], Figure 3? (3 pts)
```{r}
pred2 <- pred %>%
  group_by(year) %>%
  summarize(total_extreme=sum(n_extreme))
#  mutate(TME = pred[1:34,4])
pred2$TME <- pred$TME[1:34]

ggplot(pred2, aes(x=year)) +
  geom_point(aes(y = TME, colour = "TME")) +
  geom_point(aes(y = total_extreme/100, colour = "total_extreme")) +
  geom_line(aes(y = TME, colour = "TME")) +
  geom_line(aes(y = total_extreme/100, colour = "total_extreme")) +
  scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Number of extreme events")) +
  ggtitle("Annual Tropical Moisture Exports (TME) & Extreme Events", subtitle = "Across all locations") +
  guides(colour = guide_legend(title=NULL)) +
  scale_colour_discrete(breaks=c("TME", "total_extreme"),
                         labels=c("TME", "Count of Extreme Events"))
  

```

This plot shows the TME compared with the annual count of extreme events over the whole region. The pattern is similar to figure 3 in Steinschneider and Lall [2015] as we can see a similar relationship between TME and the count of extreme events. The magnitude is different because the counting value in the paper is spatially-averaged and the method of calculating TME seems to be slightly different. Our plot contains two y-axis (one each for TME and extreme events count). As the count of extreme events can never be negative but TME can, the left y-axis limits are -1.5 and 2.5 and the right y-axis has limits of -150 and +250, but no values below 0.

4. To pick a site of interest, we will use randomization. The set .seed function in R sets the seed for random number generation for reproducability. Pick the last 4 digits of your UNI and use that to set seed, for example set .seed(2136). Then choose a site with my_site <−sample(1:39, 1). (2 pts)
```{r}
set.seed(3508)
my_site <- sample(1:39, 1)
```
The site number is 24.


5. Plot the number of occurrences of extremes as a function of time for the site you have chosen (you may want to sub-set the data ﬁrst with dplyr :: ﬁlter ()). (3 pts)
```{r}
pred_mysite <- pred %>%
  filter(site==my_site) 

ggplot(pred_mysite,aes(x=year,y=n_extreme))+
  geom_point()+
  geom_line()+
  labs(title = "Total Annual Extreme Events", subtitle = paste("At Site", my_site), y = "Count of Extreme Events")

```


6. For your site, use a general linear model to estimate the number of extreme events per year, using the TME data as a predictor. What is the correct family of GLM to use here? Write out your regression model using formal notation. (10 pts)

The best distribution for discrete data (i.e. counts) is Poisson distribution. The assumption of Poisson distribution is that the mean and variation is the same. We examined these two values and found them close enough to make this assumption for our data.
```{r}
#assumption: Poisson random variable has the same mean and variance
mean(pred_mysite$n_extreme)
var(pred_mysite$n_extreme)
#are close!

glm1 <- glm(n_extreme ~ TME, family=poisson(link = log), data=pred_mysite)  #Poisson

summary(glm1)
```

Our Poisson regression model is: $$\theta_i = \alpha + X_i \beta$$

Where $\theta_i$ is the log value of the estimated number of extreme events, $\beta$ is the coefficient of $X_i$, $X_i$ is TME, and $\alpha$ is the intercept.

With the values found above, the Poisson GLM predict function gives us:

$$\theta_i = 0.847 + 0.332 * TME$$

In addition: 
$$\lambda = \exp(\theta)$$
Where $\lambda$ is the estimated number of extreme events.

Therefore, we have $$y_i \sim \text{Poiss}(\lambda_i)$$


7. Use your model to predict the exceedances at each site. Plot the time series of observed exceedances at your site, and the time series of predicted exceedances. What do you notice about your predicted values versus observed exceedances? (10 pts)
```{r}
#Method 1: estimate by ourselves
alpha_hat <- glm1$coefficients['(Intercept)']
beta_hat <- glm1$coefficients['TME']
theta_hat <- alpha_hat + beta_hat*pred_mysite
lambda_hat <- exp(theta_hat)

#Method 2: use 'predict' function
asd <- as.matrix(predict(glm1)) 
pred_mysite2 <- pred_mysite %>%
  mutate(pred=exp(asd))

#confidence limits
lb <- qpois(0.05, lambda = pred_mysite2$pred)  #90% confidence interval
ub <- qpois(0.95, lambda = pred_mysite2$pred)

ggplot(pred_mysite2, aes(x=year))+
  geom_point(aes(y = n_extreme, colour = "observed")) +
  geom_point(aes(y = pred, colour = "predicted")) +
  geom_line(aes(y = n_extreme, colour = "observed")) +
  geom_line(aes(y = pred, colour = "predicted")) +
      geom_ribbon(data=pred_mysite2,aes(ymin=lb,ymax=ub),alpha=0.2) +
  ggtitle("Predicted & Observed Count of Extreme Events") + 
  guides(colour = guide_legend(title=NULL)) 


```

The trend in observed and predicted value is quite similar, but the quantity does not match very well. However, this does not necessarily mean that the model is very bad. The shading represents a 90% confidence interval. We can see that all observed points fall within the shaded area, suggesting that the predicted value is reasonably likely with its corresponded TME. Therefore, the Poisson regression model does appear to explain the relationship between number of extreme events and TME.


8. Use a for loop to ﬁt your model at each of the 39 sites. Save the intercept and coeﬃcient term at each site. (5 pts)
```{r}
inter <- rep(0,39)
coeff <- rep(0,39)

for(i in 1:39){
  pred_site <- pred %>%
    filter(site==i) 
  glm2 <- glm(n_extreme ~ TME, family=poisson(link = log), data=pred_site)  #Poisson
  inter[i] = glm2$coefficients[1]
  coeff[i] = glm2$coefficients[2]
}
```


9. Join the data of slopes and intercepts with the location data available to you. Plot the intercept and coeﬃcient at each site, using an appropriate map projection. How does this estimate compare with Steinschneider and Lall [2015], Fig. 4, left side? (You can be qualitative - you are using diﬀerent methods). (5 pts)

```{r}
loc2 <- loc %>%
  mutate(inter = inter)%>%
  mutate(coeff = coeff)

#Coefficient
coeffPlot <- qmplot(lon,lat,data=loc2)+
  geom_point(aes(size = coeff, color = coeff)) +
  scale_size_continuous(range=c(0.1, 8)) +
  scale_color_viridis() +  
  coord_map(projection="lambert", parameters = c(25, 50)) +
  ggtitle("Coefficient") + 
  theme_few() 

#intercept
interceptPlot <- qmplot(lon,lat,data=loc2)+
  geom_point(aes(size=inter, color=inter)) +
    scale_size_continuous(range=c(0.1, 10)) +
    scale_color_viridis() +  
    coord_map(projection="lambert", parameters = c(25, 50)) +
    ggtitle("Intercept") + 
    theme_few() 

library(gridExtra)
grid.arrange(coeffPlot, interceptPlot, nrow = 1)

```

While the methods we used are different, there are some aspects between the coefficient plot and figure 4 (left) of Steinschneider and Lall [2015] worth pointing out. The slope in both plots is larger at the north-east and decreases to the southwest. There is one spot at (124W, 39N) in our figure with a larger value than its neighbors, which is different from the paper. The pattern indicates that the extreme precipitation is more sensitive to the TME variation in north-east region (or inland area).
In the intercept figure, the larger values happen at lower latitude and inland area. We can interpret this as the extreme events tend to be located in these areas under the same TME environment.


