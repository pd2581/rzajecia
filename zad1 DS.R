Zadania 1

#1.Napisz funkcję sprawdzająca czy 1 liczba jest podzielna przez druga użyj - %

podzielne <- function(liczba1, liczba2){
  if (liczba1%%liczba2 == 0){
    answer <- paste0(liczba1, ' jest podzielne przez ', liczba2)} 
  else
  {  answer <- paste0(liczba1, ' nie jest podzielne przez ', liczba2)}
  return(answer)}

# sprawdzenie
podzielne(13,2)

#2. Pociąg z Lublina do Warszawy przejechał połowę drogi ze średnią prędkością 120 km/h.
#Drugą połowę przejechał ze średnią prędkością 90 km/h.
#Jaka była średnia prędkość pociągu.

avg_speed_func <- function(avg_spd1, avg_spd2){
  solution <- 2/((1/avg_spd1) + (1/avg_spd2))
  return(solution)}

#check
avg_speed_func(120, 90)

#3. Utwórz funkcję obliczającą współczynnik korelacji r Pearsona dla 2 wektorów o tej samej długości.
#Wczytaj dane plik dane.csv i oblicz współczynnik dla wagi i wzrostu. W komentarzu napisz co oznacza wynik.

dane <- read.csv(file = 'dane.csv', sep = ';')
summary(dane)
anyNA(dane)

correlation <- function(vec1, vec2, interp = TRUE){
  
  # validation
  if (length(vec1) != length(vec2))
    stop("Lenghts of vector are not the same anymore!", call. = TRUE)
  
  # count a correlation
  korr <- cor.test(vec1, vec2, method = 'pearson')
  
  if(isTRUE(interp)){
    if(korr$estimate == 0 ){
      relacja <- paste0('There is no correlation between the two variables.')}
    else if(korr$estimate > 0)
    {relacja <- paste0('Positive correlation between two variables, value of the variable X increases with the value of Y.')}
    else if(korr$estimate < 0){
      relacja <- paste0('Negative correlation between two variables, when the value of the variable X increases, the value of the Y variable decreases.')}
    
    return(relacja)}
  else{
    return(korr$estimate)}
}
#check
correlation(dane$wzrost, dane$waga)

#4. Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika 
#stworzDataFrame <- function(ile=1)
#W pierwszym wierszu użytkownik podaje nazwy kolumn. w kolejnych wierszach zawartość wierszy ramki danych ( tyle wierszy ile podaliśmy w argumencie ile. ile=1 oznacza, że gdy użytkownik nie poda żadnej wartości jako parametr, domyślna wartością będzie 1)

stworzDataFrame <- function(ile=1, kolumny, ramka = list()){
  
  # checks
  if (class(ramka) != "list")
    stop("Zmienna 'ramka' musi miec klase typu lista", call. = TRUE)
  
  if (length(kolumny) != unique(sapply(ramka, length)) | ile != length(ramka) )
    stop("Zmienne maja rozna dlugosc", call. = TRUE)
  
  #pobranie pakietu
  packages <- 'data.table'
  
  for(package in packages){
    if(!require(package, character.only = T, quietly = T)){
      install.packages(package, repos="http://cran.us.r-project.org")
      library(package, character.only = T)}
    library(package, character.only = T)                  }
  
  # creating the data frame
  dt <- data.table()
  dt <- rbind(dt,do.call(rbind,ramka))
  dt <- as.data.frame(dt)
  colnames(dt) <- kolumny
  
  return(dt)
}

stworzDataFrame(ile=2, kolumny = c('imie', 'wiek'),ramka = list(c('Dawid', 29), c('Magda', 27)))

### brak zadania 5 gdyz nie wychodzi mi do konca -> przesle w ciagu kilku dni max