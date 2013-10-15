# Common libraries
library(sqldf)
library(ggplot2)
library(caTools)
library(pdicommon)
library(reshape)



# Read plagues data
raw_plagues <- read.csv('/data/fiware/plagas_malaga.csv', 
                        header=FALSE, 
                        col.names=c('date_', 'district', 'hood', 'address', 'source', 'location_type', 'action', 'plague_type'))
                        
weather <- read.csv('/data/fiware/weather_malaga_2013.csv',
                    header=FALSE,
                    col.names=c('city', 'date_', 'avg_temp', 'max_temp', 'min_temp', 'atm_press',
                                'humidity', 'rain', 'visibility', 'avg_wind', 'max_wind_sust', 'max_wind_blasts',
                                'accum_rainy_days', 'accum_snow_days', 'accum_storm_days', 'accum_fog_days'))

raw_plagues$date <- as.Date(raw_plagues$date_, "%Y-%m-%d")
weather$date <- as.Date(weather$date_, "%Y-%m-%d")

# Get rid of weekends (to prevent driving wrong conclusions)
weekend <- format(raw_plagues$date, "%A") %in% c('Saturday', 'Sunday')
raw_plagues <- raw_plagues[!weekend, ]


# Groups
raw_plagues.perday <- sqldf("select date, count(1) cnt from raw_plagues group by 1")
raw_plagues.pertype <- sqldf("select plague_type, count(1) cnt from raw_plagues group by 1")
raw_plagues.perdayandtype <- sqldf("select date, plague_type
                                   , count(1) cnt from raw_plagues group by 1,  2")

raw_plagues.perdayandtype <- sqldf("select date, plague_type
                                   , count(1) cnt from raw_plagues group by 1,  2")

raw_plagues.perdayandtype_dataset <- sqldf("select date
                                           , count(1) cnt
                                           , sum(case when plague_type in ('Cucarachas', 'Pulgas') then 1 else 0 end) cnt_cucarachas
                                           , sum(case when plague_type in ('Ratas', 'Ratones') then 1 else 0 end) cnt_ratas
                                           , sum(case when plague_type in ('Abejas', 'Avispas', 'Moscas', 'Mosquitos') then 1 else 0 end) cnt_insectos
                                           , sum(case when plague_type in ('Cotorra Argentina', 'Gaviota', 'Paloma', 'Mosquitos') then 1 else 0 end) cnt_aves
                                           from raw_plagues group by 1")


raw_plagues.pertype

# Analysis per day
r <- merge(raw_plagues.perday, weather, by.x="date", by.y="date")
r2 <- merge(raw_plagues.perdayandtype, weather, by.x="date", by.y="date")
r3 <- merge(raw_plagues.perdayandtype_dataset, weather, by.x="date", by.y="date")

temp <- melt(raw_plagues.perdayandtype_dataset, id="date")

temp <- temp[temp$variable != "cnt", ]

ggplot(temp, aes(x = factor(date), y = value, fill = factor(variable))) +
  geom_bar()

ggplot(temp, aes(x = date, y = value)) +
  geom_smooth(aes(y=value)) +
  geom_line() + facet_wrap(~ variable)
       

r4 <- merge(temp, weather, by.x="date", by.y="date")



r$weekday <- format(r$date, format="%a")
r2$weekday <- format(r2$date, format="%a")
r3$weekday <- format(r3$date, format="%a")
r4$weekday <- format(r4$date, format="%a")


cor(r3[,sapply(r3, is.numeric)])[1:5,1:5]
       

# Line charts
ggplot(data=r, aes(x=date)) + 
  geom_line(aes(y = cnt, colour="cnt")) +
  geom_line(aes(y = avg_temp, colour="avg_temp")) + 
  # geom_line(aes(y = min_temp, colour="min_temp")) +
  # geom_line(aes(y = max_temp, colour="max_temp")) +
  # geom_line(aes(y = humidity, colour="humidity")) +
  geom_ribbon(aes(ymin=min_temp, ymax=max_temp, fill="avg_temp"),alpha=0.2)
  

# Line charts with smooth
ggplot(data=r, aes(x=date)) + 
  geom_line(aes(y = cnt, colour="cnt"))  + geom_smooth(aes(y=cnt, colour="cnt")) + 
  geom_line(aes(y = avg_temp, colour="avg_temp")) + geom_smooth(aes(y=avg_temp, colour="avg_temp")) +
  # geom_line(aes(y = min_temp, colour="min_temp")) +
  # geom_line(aes(y = max_temp, colour="max_temp")) +
  geom_line(aes(y = humidity, colour="humidity")) +geom_smooth(aes(y=humidity, colour="humidity"))
  # geom_ribbon(aes(ymin=min_temp, ymax=max_temp, fill="avg_temp"),alpha=0.2)


# Scatterplots
ggplot(data=r, aes(x=cnt, y=avg_temp, colour="black")) + geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)

ggplot(data=r2, aes(x=cnt, y=avg_temp, colour=plague_type)) + geom_point()

ggplot(data=r2[r2$plague_type %in% c('Cucarachas', 'Ratas'),],
       aes(x=cnt, y=avg_temp, colour=plague_type)) + geom_point()

ggplot(data=r2[r2$plague_type=='Cucarachas',], aes(x=cnt, y=humidity)) + geom_point(colour="#000099") + 
 geom_smooth(method = "lm", se=FALSE, aes(colour="cucarachas"), colour="#000099", formula = y ~ x)

ggplot(data=r2[r2$plague_type=='Ratas',], aes(x=cnt, y=humidity, colour="ratas")) + geom_point() +
  geom_smooth(method = "lm", se=FALSE, aes(colour="ratas"), formula = y ~ x)


#ggplot(data=r2[r2$plague_type=='Ratas',], aes(x=cnt, y=humidity, colour="ratas")) + geom_point()


cor(r2[r2$plague_type=='Gaviota',
       c('cnt', 'humidity', 'avg_temp', 'max_temp', 'min_temp')])

raw_plagues.pertype

#r <- r2[r2$plague_type == 'Cucarachas', ]

train_records <- sample.split(r, SplitRatio = 0.7)
model <- glm(cnt ~ avg_temp + max_temp + min_temp + humidity + avg_wind + max_wind_sust + weekday, data=r[train_records,])

r$predicted <- predict(model, r)
r$err <- abs(r$predicted - r$cnt)
r$err_norma <- r$err/mean(r$cnt)

mean(r[train_records, "cnt"])
mean(r[!train_records, "cnt"])

mean(r[train_records, "err"])
mean(r[!train_records, "err"])

mean(r[train_records, "err_norma"])
mean(r[!train_records, "err_norma"])


ggplot(data=r[!train_records,], aes(x=date, y=err, colour=weekday)) + geom_point()

summary(r[!train_records,])


g <- ggplot(data=r[train_records,], aes(x=date)) + 
  scale_y_continuous("incidentes", limits=c(0,70)) +
  geom_line(aes(y=cnt, colour="cnt"))  +
  geom_line(aes(y=predicted, colour="predicted")) + 
  geom_ribbon(aes(ymin=predicted*0.8, ymax=predicted*1.2),alpha=0.2) + scale_fill_manual(values=c('red', 'red'), labels=c('predicted'))

plot(g)

g <- g + geom_line(aes(y=cnt, colour="cnt"))  

ggplot(data=r[!train_records,], aes(x=err)) + 
  geom_histogram(aes(y=..count.., fill=..count..), binwidth=1)  +
  scale_fill_gradient("Count", low = "yellow", high = "red") 




qplot(err, data=r[!train_records, ], geom="histogram")

  
