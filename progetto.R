
# carico il dataset

data <- read.csv("/Users/saradagostino/Desktop/progetti master/2.statistica descrittiva/realestate_texas.csv")
attach(data)


# Indici di posizione

MEAN <- c(mean(sales), mean(volume), mean(median_price), mean(listings), mean(months_inventory))
MEDIAN <- c(median(sales), median(volume), median(median_price), median(listings), median(months_inventory))
MAX <- c(max(sales), max(volume), max(median_price), max(listings), max(months_inventory))
MIN <- c(min(sales), min(volume), min(median_price), min(listings), min(months_inventory))
variables <- c("sales", "volume", "median_price", "listings", "months_inventory")
FIRSTQ <- c(quantile(sales)[2],quantile(volume)[2],quantile(median_price)[2],quantile(listings)[2],quantile(months_inventory)[2])
THIRDQ <- c(quantile(sales)[4],quantile(volume)[4],quantile(median_price)[4],quantile(listings)[4],quantile(months_inventory)[4])

position_indices <- matrix(c(MEAN, MEDIAN, MAX, MIN, FIRSTQ, THIRDQ), ncol=6, dimnames = list(c(variables),
                                                                           c("MEAN", "MEDIAN", "MAX", "MIN", "1stQ", "3rdQ")))
#data.frame(position_indices)
write.csv(position_indices, "/Users/saradagostino/Desktop/progetti master/tabelle/position_indices.csv")


# Indici di variabilità

cv <- function(x){
  return(sd(x)/mean(x)*100)
}

gini.index <- function(x){
  ni = table(x)
  fi = ni/length(x)
  fi2=fi^2
  J = length(table(x))
  
  gini=1-sum(fi2)
  gini.normalizzato=gini/((J-1)/J)
  
  return(gini.normalizzato)
}

IntQRange <- c(IQR(sales), IQR(volume), IQR(median_price), IQR(listings), IQR(months_inventory))
VARIANCE <- c(var(sales), var(volume), var(median_price), var(listings), var(months_inventory))
StD <- c(sd(sales), sd(volume), sd(median_price), sd(listings), sd(months_inventory))
CV <- c(cv(sales), cv(volume), cv(median_price), cv(listings), cv(months_inventory))
GINI <- gini.index(city) # essendo 1, le città sono presenti con la stessa modalità

variability_indices <- matrix(c( IntQRange, VARIANCE, StD, CV), ncol = 4, dimnames = list(c(variables), 
                                                                                             c("IQR", "Variance", "SD", "CV")))
write.csv(variability_indices, "/Users/saradagostino/Desktop/progetti master/tabelle/variability_indices.csv")


#Asimmetria e curtosi

library(moments)

SKEWNESS <- c(skewness(sales), skewness(volume), skewness(median_price), skewness(listings), skewness(months_inventory))
KURTOSIS <- c(kurtosis(sales)-3, kurtosis(volume)-3, kurtosis(median_price)-3, kurtosis(listings)-3, kurtosis(months_inventory)-3)

skew_kurt <- matrix(c(SKEWNESS, KURTOSIS), ncol=2, dimnames = list(c(variables), c("Skewness", "Kurtosis")))
write.csv(skew_kurt, "/Users/saradagostino/Desktop/progetti master/tabelle/skew_kurt.csv")


#Distribuzioni di frequenze

distr_freq_city <- table(city)
distr_freq_month <- table(month)
distr_freq_year <- table(year)



# sales: distribuzione di frequenze e grafico a barre

N<-dim(data)[1] # numero di misurazioni per la variabile sales

freq_ass_sales <- table(sales)
freq_rel_sales <- freq_ass_sales/N

distr_freq_sales <-cbind(freq_ass_sales, freq_rel_sales)  #distribuzione di frequenze 

sales_cl <- cut(sales, breaks = c(50,  150, 250,   350,  450)) # suddivisione in classi 


ni_sales <- table(sales_cl) #frequenza assoluta sales
fi_sales <- table(sales_cl)/N #frequenza relativa sales
Ni_sales <- cumsum(ni_sales) #frequenza assoluta cumulata sales
Fi_sales <- cumsum(fi_sales) #frequenza assoluta cumulata sales


#figura 1
ggplot(data=data)+
  geom_bar(aes(x=sales_cl), stat = "count", col="black", fill="lightblue")+
  labs(
    title = "Distribuzione in classi della variabile 'sales'",
    x = "Classi",
    y = "Frequenze assolute"
  )+
  scale_y_continuous(breaks = c(10,20,30, 40, 50, 60, 70, 80, 90, 100))


sales_cl_gini <- gini.index(sales_cl)

sales_gini <- gini.index(sales)


# Probabilità


p_Beaumont <- distr_freq_city[1]/N

p_luglio <- distr_freq_month[7]/N

p_dic_2012 <- (distr_freq_month[12]/N) * (distr_freq_year[3]/N)


probabilità <- matrix(c(p_Beaumont, p_luglio, p_dic_2012), ncol =1, dimnames=list(c("Beaumont", "Luglio", "Dicembre 2012"),
                                                                                  c("Probabilità [%]")))
write.csv(probabilità, "/Users/saradagostino/Desktop/progetti master/tabelle/probabilità.csv")


# Prezzo medio delle vendite

mean_value <- (volume/sales)*1000000
data$mean_value <- mean_value

# Efficacia degli annunci 
sold_percentage <- (sales/listings) *100
data$sold_percentage <- sold_percentage


# Summary

library(dplyr)

# non riportato nel report
group_city<-data %>%
  group_by(city) %>%
  summarise(Mean=mean(sales),
            SD=sd(sales),
            
            CV=cv(sales),
            Max=max(sales),
            Median=median(sales),
            Min=min(sales)
            )

write.csv(group_city, "/Users/saradagostino/Desktop/progetti master/tabelle/group_city.csv")

#figura 4
library(ggplot2)
ggplot(data=group_city)+
  geom_col(aes(x=group_city$city, y=group_city$Mean), col="black",fill="orange")+
  labs(
    x="Città",
    y="Valore medio",
    title="Numero medio delle vendite nelle città del Texas"
  )+
  scale_y_continuous(breaks=c(0,50,100,150,200,250,300,350))
  

# non riportato nel report
group_year <-data %>%
  group_by(year) %>%
  summarise(Mean=mean(sales),
            SD=sd(sales),
            
            CV=cv(sales),
            Max=max(sales),
            Median=median(sales),
            Min=min(sales)
  )


# non riportato nel report
group_month <-data %>%
  group_by(month) %>%
  summarise(Mean=mean(sales),
            SD=sd(sales),
            
            CV=cv(sales),
            Max=max(sales),
            Median=median(sales),
            Min=min(sales)
  )


#figura 2
ggplot(data=data)+
  geom_boxplot(aes(x=data$city, y=data$median_price), fill="Pink")+
  labs(
    x="Città",
    y="Prezzo mediano [$]",
    title = "Confronto indici del prezzo mediano tra le città del Texas"
  )


#tabella 8
library(moments)
group_city_median_price<-data %>%
  group_by(city) %>%
  summarise(Mean=mean(median_price),
            SD=sd(median_price),
            CV=cv(median_price),
            Max=max(median_price),
            Median=median(median_price),
            Min=min(median_price)
  )
write.csv(group_city_median_price, "/Users/saradagostino/Desktop/progetti master/tabelle/group_city_median_price.csv")


#figura 3
ggplot(data=data)+
  geom_boxplot(aes(x=data$city, y=data$volume), fill="coral")+
  labs(
    x="Città",
    y="Valore delle vendite [M$]",
    title = "Confronto indici del valore delle vendite tra le città del Texas"
  )

library(moments)
group_city_skew_kurt<-data %>%
  group_by(city) %>%
  summarise(Skewness=skewness(median_price),
            Curtosi=kurtosis(median_price)-3)
write.csv(group_city_skew_kurt, "/Users/saradagostino/Desktop/progetti master/tabelle/group_city_skew_kurt.csv")





#figura 5
ggplot(data=data)+
  geom_boxplot(aes(x=data$year, y=data$volume, group=data$year), fill="cadetblue1")+
  labs(
    x="Anno",
    y="Valore delle vendite [M$]",
    title = "Confronto indici del valore delle vendite tra i vari anni"
  )



# Raggruppare i dati per città, anno e mese e calcolare il totale delle vendite per ogni mese
grouped_data <- data %>%
  group_by(city, year, month) %>%
  summarise(total_sales = sum(sales))

# grafico a barre sovrapposte per ogni anno e città
#figura 6

ggplot(grouped_data, aes(x = year, y = total_sales, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ city, nrow = 1) +
  labs(x = "Anno", y = "Totale Vendite", title = "Totale Vendite per mese e anno nelle città del Texas", fill="Mese") +
  theme_minimal() +
  theme(legend.position = "bottom",
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))+
  scale_y_continuous(breaks = seq(0, 4000, 200))


# grafico a barre sovrapposte normalizzato per ogni anno e città
#figura 7 

ggplot(grouped_data, aes(x = year, y = total_sales, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ city, nrow = 1) +
  labs(x = "Anno", y = "Frequenza relativa vendite", title = "Totale Vendite per mese e anno nelle città del Texas", fill="Mese") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
  

# serie storica

grouped_data_per_year <- data %>%
  group_by(city, year) %>%
  summarise(total_sales_per_year = sum(sales))

#figura 8
#line chart
ggplot(data=grouped_data_per_year, aes(x = year, y = total_sales_per_year)) +
  geom_line(col="red", lwd=0.5) +
  geom_point(col = "red", size = 1.5)+
  facet_wrap(~ city, nrow = 1) +
  labs(x = "Anno", y = "Totale Vendite", title = "Totale Vendite per anno nelle città del Texas") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))+
  scale_y_continuous(breaks = seq(0, 4000, 200))


# line chart sales + listings
listings_sales_per_year <- data %>%
  group_by(city, year) %>%
  summarise(total_listings_per_year = sum(listings),
            total_sales_per_year = sum(sales))

#figura 9
ggplot(data=listings_sales_per_year) +
  geom_line(aes(x = year, y = total_sales_per_year, col = "Numero vendite"),  lwd=0.5) +
  geom_point(aes(x = year, y = total_sales_per_year, col = "Numero vendite"), size = 1.5)+
  geom_line(aes(x = year, y = total_listings_per_year, col = "Numero annunci"), lwd=0.5) +
  geom_point(aes(x = year, y = total_listings_per_year, col = "Numero annunci"), size = 1.5)+
  facet_wrap(~ city, nrow = 1) +
  labs(x = "Anno",
       y="Numero di vendite/annunci", 
       title = "Totale Vendite e annunci per anno nelle città del Texas") +
  theme_minimal()+
  scale_color_manual(
    name="LEGENDA",
    breaks = c("Numero vendite", "Numero annunci"),
    values = c("Numero vendite" = "red", "Numero annunci" = "blue"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        legend.position = "bottom")+
  scale_y_continuous(breaks = seq(0,40000, 2000))

#efficacia
#tabella 9
efficacy_per_year <- data %>%
  group_by(city, year) %>%
  summarise(total_listings_per_year = sum(listings),
            total_sales_per_year = sum(sales),
            sold_percentage = (sold_percentage/total_listings_per_year)*100)

