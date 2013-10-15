# Common libraries
library(sqldf)
library(ggplot2)
library(caTools)
library(reshape)


weekday_levels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

############################
# Load ecocontadores data
############################
ecocounters <- read.csv('/data/fiware/ecocontadores.csv', 
                    header=FALSE, 
                    sep=';',
                    col.names=c('date_', 'hour_', 'eco1', 'eco2', 'eco3', 'eco4', 'eco5', 'eco6', 'eco7', 'eco8', 'eco9'))

# Include a sum of all of them (3 to 11 are column position for eco1 to eco9)
ecocounters$all <- apply(ecocounters[,3:11], 1, sum)
# Format date and consolidate hour as number, and weekday and month as factors
ecocounters$date <- as.Date(ecocounters$date_, "%Y-%m-%d")
ecocounters$weekday <- factor(format(ecocounters$date, format="%a"), levels=weekday_levels)
ecocounters$hour <- as.numeric(gsub(":", "", substr(ecocounters$hour,1,2)))
ecocounters$month <- factor(format(ecocounters$date, format="%b"), levels=month_levels)

# Quick look at some of them
ggplot(ecocounters, aes(x=hour, y=all, group=date_, colour=weekday)) + geom_line() 
ggplot(ecocounters, aes(x=hour, y=eco1, group=date_, colour=weekday)) + geom_line() + facet_grid(weekday ~ month)
ggplot(eco_df, aes(x=hour, y=eco2, group=date_, colour=weekday)) + geom_line() 
ggplot(eco_df, aes(x=hour, y=eco3, group=date_, colour=weekday)) + geom_line() 


#################
# Load weather
#################
# INDICATIVO;NOMBRE;ALTITUD;C_X;C_Y;LATITUD;LONGITUD;DATUM;
#   A?O;MES;DIA;TMAX;HTMAX;TMIN;HTMIN;TMED;R_MAX_DIR;
#   R_MAX_VEL;R_MAX_HOR;DIR_VEL_10;VEL_MED_10;HOR_VEL_10;P24

weather <- read.csv('/data/fiware/aemet/Datos_Sevilla.csv',
                    header=TRUE, sep=";", dec=",")
weather$date_ <- apply(weather, 1, function(x) {gsub(" ", "", paste(as.integer(x['ANYO']), "-", x['MES'], "-", x['DIA'])) } )
weather$date <- as.Date(weather$date_)
# Drop unused columns (simple example just average temperature and rain)
weather <- weather[, c('date', 'TMED', 'TMAX', 'TMIN',  'P24')]
colnames(weather) <- c('date', 'avg_temp', 'max_temp', 'min_temp', 'rain')
weather$month <- factor(format(weather$date, format="%b"), levels=month_levels)
weather$year <- format(weather$date, "%Y")


# Quick view of weather
ggplot(weather[weather$year==2013,], aes(x=date)) + 
  geom_line(aes(y=avg_temp, colour="avg", group="")) + geom_smooth(aes(y=avg_temp, colour="avg")) + 
  geom_line(aes(y=max_temp, colour="max", group="")) + geom_smooth(aes(y=max_temp, colour="max")) + 
  geom_line(aes(y=min_temp, colour="min", group="")) + geom_smooth(aes(y=min_temp, colour="min"))  


######################
# Merge both datasets
######################
ecocounters <- merge(ecocounters, weather, by.x="date", by.y="date", all.x=TRUE)
ggplot(ecocounters, aes(x=hour, y=eco1, group=date_, colour=rainy)) + geom_line() + facet_grid(weekday ~ month.x)

ecocounters$rainy <- ifelse(ecocounters$rain > 0.5, TRUE, FALSE)

ec <- ecocounters[, c("date", "hour", "eco1", "eco2", "eco3", "eco4", "eco5", "eco6", "eco7", "eco8", "eco9", "all", "weekday", "rainy", "month.x" )]

ec <- melt(ec, id.vars=c("date", "hour", "weekday", "rainy", "month.x"))

ggplot(ec[ec$variable != "all", ], aes(x=hour, y=value, group=date, colour=weekday)) + geom_line() + facet_grid(variable ~ weekday)


########################
# Principal components:
#   1. Reshaope to create dataset-like dataframe (24 features being bikefall per hour)
#   2. Run PCA
########################

ec_dataset <- reshape(ec, direction = 'wide', 
                      idvar=c('date', 'variable', 'weekday', 'month.x', 'rainy'), 
                      timevar='hour')


# Features for PCA only bikefalls (do not consider first 5 columns)
# Remove also aggregation variable ("all)
ec_dataset <- ec_dataset[ec_dataset$variable != "all", ]
pcafit <- princomp(ec_dataset[, -5:-1], cor=TRUE)
#fit <- prcomp(eco_dataset[,c(-1, -2)])
summary(pcafit)
#Reachs 82% variance with just 2 components
ec_dataset$comp1 <- pcafit$scores[,1]
ec_dataset$comp2 <- pcafit$scores[,2]

ggplot(ec_dataset, aes(x=comp1, y=comp2, colour=variable)) + geom_point()
ggplot(ec_dataset[ec_dataset$variable != "all",], aes(x=comp1, y=comp2, colour=variable)) + geom_point() + facet_wrap(~weekday)
ggplot(ec_dataset[ec_dataset$variable != "all",], aes(x=comp1, y=comp2, colour=weekday)) + geom_point() + facet_wrap(~variable)


