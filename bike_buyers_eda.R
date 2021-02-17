#   libek ----
#         hiányzó library esetén: install.packages("libraryneve")

library(dplyr)
library(plotly)
library(ggplot2)     #imports library ggplot2
library(hrbrthemes)  #imports library hrbrthemes
library (tidyr)      #imports the tidyverse package
library(viridis)     #imports the library viridis
library(gmodels)
library(datasets)

#   adatok beolvasása ----
bike_buyers = read.csv("assets/bike_buyers.csv")

#   AZ ADATHALMAT ÁTTEKINTÉSE MAGASABB SZINTEN ----

str(bike_buyers)
summary(bike_buyers)
class(bike_buyers)

View(bike_buyers)
head(bike_buyers)



##  ELSŐ KÉRDÉSEK: ----

#   Szükséges-e változtatásokat végeznünk? (Például: adattípus, struktúra stb)
#   nem lesz szükségünk az ID oszlopra az elemzésünk során

bike_buyers = select(bike_buyers, -ď.żID) #valami furcsa anomália miatt az első változó nevének dekódolása hibás, így lett "ID" helyett "ď.żID"
summary(bike_buyers)


##  KATEGÓRIÁS VS FOLYTONOS VÁLTOZÓK ----
# Vessük össze az iskolai végezttségeket és a fizetéseket!

by(bike_buyers$Income, bike_buyers$Education, summary)
by(bike_buyers$Income, bike_buyers$Education, mean)
by(bike_buyers$Income, bike_buyers$Education, median)

# rögtön szembetűnő az iskolai végezttség hatása a fizetésre
# durván három nagyobb csoportot láthatunk kirajzolódni:

# 1. akik átlagosan a legtöbbet keresnek: főiskolai diploma és diploma
# 2. az itt lévő adatokhoz viszonyítva egy "kb" közepes kategória: tanult főiskolán / középiskolát végzett
# 3. és a legalacsonyabb kategória: tanult középiskolában, de nem végezte el

# A csoportok a változók nevei szerint:  
  #1. Bacherlors & Graduate degree  - legtöbbet keresik
  #2. Partial College & High School - közepes kereset
  #3. Partial High School           - legalacsonyabb kereset


# vizualizációk:

# fizetések az iksolai végzettségek függvényében
boxplot(bike_buyers$Income~bike_buyers$Education, notch=TRUE, col=c("lightgreen","darkgreen","gold","orange","tomato"), main="Fizetések iskolai végzettség szerint")

# sűrűség plot
density_income_edu <- ggplot(bike_buyers, aes(x=Income, group=Education, fill=Education, alpha=.5))+
  geom_density(adjust=1)+         #plots the density graph
  labs(title="Fizetések iskolai végzettség szerint")+
  theme_ipsum()
density_income_edu


##  KATEGÓRIÁS VS KATEGÓRIÁS VÁLTOZÓK ----

#   Cross Tabulation (xtabs) / Kontingencia táblák

#   vásárolt biciklit vs végzettség
xtabs(~Education+Purchased.Bike, bike_buyers)
plot(xtabs(~Education+Purchased.Bike, bike_buyers), col=c("indianred1","lightgreen"), main="Bicikli vásárlók és iskolai végzettség")

# vásárolt biciklit vs foglalkozás
xtabs(~Occupation+Purchased.Bike, bike_buyers)
plot(xtabs(~Occupation+Purchased.Bike, bike_buyers), col=c("indianred1","lightblue"), main="Bicikli vásárlók és foglalkozás")

# vásárolt biciklit vs ingázás
xtabs(~Commute.Distance+Purchased.Bike, bike_buyers)
plot(xtabs(~Commute.Distance+Purchased.Bike, bike_buyers), col=c("indianred1","seagreen3"), main="Bicikli vásárlók és ingázási távolság")


## ÁSSUNK MÉLYEBBRE:

#   a következőkben khi négyzetösszegekkel meg fogjuk határozni a p-értéket, hogy megtudjuk az egyes változók szignifikancia szintjét
#   ha a p-értéket > 0.05 akkor nem szignifikáns 
#   figyeljünk arra, hogy az Y változó a függő változó (tehát jelen esetben a függvény második paramétere: Purchased.Bike)

CrossTable(bike_buyers$Education, bike_buyers$Purchased.Bike, chisq = TRUE, prop.t = F)
#   a p-érték nagyon kicsi (jelentősen kisebb, mint 0.05) tehát a végzettség fontos változó


CrossTable(bike_buyers$Occupation, bike_buyers$Purchased.Bike, chisq = TRUE, prop.t = F)
#   a p-érték nagyobb, mint 0.05 tehát a jelek szerint a foglalkozás nem szignifikáns



##  FOLYTONOS VS FOLYTONOS VÁLTOZÓK ----

#   keressünk összefüggéseket a biciklivásárlók életkora és fizetése közt (van-e egyáltalán? ha igen, mi lehet az?)

str(bike_buyers)
scatter.smooth(bike_buyers$Age, bike_buyers$Income, main="Kor és fizetés viszonya")


# érdekesség, mivel az előző dataset nem volt túl szerencsés a folytonos változók tekintetében, nézzük meg az egyik beépített datasetet
cars = mtcars
scatter.smooth(cars$mpg, cars$hp, xlab="Mérföld / Gallon", ylab="Lóerő")

#   ez az ábra kicsit látványosabb, mit is látunk?
#   minél nagyobb a fogyasztás (mpg) annál kisebb a lóerők száma
#   tehát ezesetben egy negatív irányú korrelációt láthatunk
