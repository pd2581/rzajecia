
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
str(lista)

plec<-c("mezczyzna","kobieta","mezczyzna")
plecf<-as.factor(plec)
str(plecf)
plecf2<-factor(plec,levels = c("mezczyzna", "kobieta"))
str(plecf2)

df<- data.frame(index=1:3,imie=c("jan","alina","bartek"),plec=plecf,stringsAsFactors = FALSE)
str(df)
View(df)

df2<-read.csv2("dane.csv")
View(df2)

#petla
for (i in 1:10){
  if(i==2){
  break
  }
  else{
  print (i)
  }
}


