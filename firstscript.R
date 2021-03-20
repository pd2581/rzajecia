
# install.packages("httr")
# install.packages("jsonlite")
# 
# library(httr)
# require(jsonlite)

endpoint<- "https://api.openweathermap.org/data/2.5/weather?q=Warszawa&appid=1765994b51ed366c506d5dc0d0b07b77"
getWeather<- GET(endpoint)
weatherText<- content(getWeather,as ="text")
weatherJson<- fromJSON(weatherText,flatten = FALSE)
weatherDF<- as.data.frame(weatherJson)
View(weatherDF)
weatherText

#as.data.frame(weatherText)

x<-124.5
x
class(x)
is.vector(x)
x<-"123"
class(x)
x<-c(1,2,3,4,5)
y<-2
y<-vector(2,mode="integer")
class(x)

x<-as.integer(x)
wynik<- x+y
wynik
class(wynik)

v1<-c(1,2,3,4,5,6,7,8,9,10)
v1<-as.integer(v1)
v2<-as.vector(c(2),mode="integer")

wynik<- v1-v2
class(wynik)
wynik<- v1*v2
class(wynik)
wynik<- v1+v2
class(wynik)
wynik<- v1/v2
class(wynik)



wynik<- v1%%v2
wynik
wynik<- v1%/%v2
wynik

lista <- list(1,2,3,4,5)
class(lista)

