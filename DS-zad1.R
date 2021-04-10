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
correlation(dane$wzrost, dane$wzrost)


#4. Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika 
#stworzDataFrame <- function(ile=1)
#W pierwszym wierszu użytkownik podaje nazwy kolumn. w kolejnych wierszach zawartość wierszy ramki danych ( tyle wierszy ile podaliśmy w argumencie ile. ile=1 oznacza, że gdy użytkownik nie poda żadnej wartości jako parametr, domyślna wartością będzie 1)


stworz_Data_Frame <- function(ile = 1, nazwy_kolumn, zawartosc_ramki = list()){
  
  # checks
  if (class(zawartosc_ramki) != "list")
    stop("Zmienna 'zawartosc_ramki' musi byc lista", call. = TRUE)
  
  if (length(nazwy_kolumn) != unique(sapply(zawartosc_ramki, length)) | ile != length(zawartosc_ramki) )
    stop("Zmienne sa roznej dlugosci!", call. = TRUE)
  
  # 
  packages <- 'data.table'
  
  for(package in packages){
    if(!require(package, character.only = T, quietly = T)){
      install.packages(package, repos="http://cran.us.r-project.org")
      library(package, character.only = T)}
    library(package, character.only = T)
                        }
  
  # creating the data frame
  my_dt <- data.table()
  my_dt <- rbind(my_dt,do.call(rbind,zawartosc_ramki))
  my_dt <- as.data.frame(my_dt)
  colnames(my_dt) <- nazwy_kolumn
  
  return(my_dt)
}

stworz_Data_Frame(ile =2, nazwy_kolumn = c('imie', 'wiek'),zawartosc_ramki = list(c('Dawid', 29), c('Magda', 27)))

#5 Napisz funkcję , która pobiera sciezkeKatalogu, nazweKolumny, jakaFunkcje, DlaIluPlikow i liczy: 
#mean, median,min,max w zależności od podanej nazwy funkcji w argumencie, z katologu który podaliśmy i z tylu plików ilu podaliśmy dla wybranej nazwy kolumny. 
#UWAGA: w podanych plikach R pobierając komórki nazwane liczbami R wstawi przed nazwy X. Funkcję przetestuj dla katalogu smogKrakow.zip.  Wykonując obliczenia pomiń brakujące wartości.

liczZplikow <- function(sciezka,jakaFunkcja="mean",DlaIluPlikow=1){ 
  
  # checks
  if (!(jakaFunkcja %in% c('mean', 'max', 'min', 'median')))
    stop("Blednie uzupelniony parametr 'jakaFunkcja'", call. = TRUE)
  
  # packages <- 'FactoMineR'
  # 
  # for(package in packages){
  #   if(!require(package, character.only = T, quietly = T)){
  #     install.packages(package, repos="http://cran.us.r-project.org")
  #     library(package, character.only = T)
  #   }
  #   library(package, character.only = T)
  # }
  

  #select all files
  files <- list.files(sciezka, full.names = T)[1: DlaIluPlikow]
  
  #creating data.table with data
  my_dt <- lapply(files, fread)
  my_dt <- rbindlist(my_dt)
  colnames(my_dt)
  
  # removing empty columns
  cols <- sapply(my_dt, function (k) all(!is.na(k)))
  my_dt2 <- my_dt[, ..cols]
  
  #selecting the columns 
  print(colnames(my_dt2))
  nazwaKolumny <-  as.vector(readline(paste0('Select the columns: ')))
  
  nn <- as.vector(unlist(strsplit(nazwaKolumny, '"')))
  nn <- str_split(nn, ' ')
  nn <- nn[nn != '']
  nn <- unlist(nn[seq(1, length(nn), 2)])
  
  new_dt22 <- my_dt2[, ..nn]
  
  # counting the results
  results <- data.table(zmienna = paste0(nn, '_mean'),arythmetic=lapply(new_dt22, jakaFunkcja))
  return(results)
}

liczZplikow(sciezka = '../smogKrakow/smogKrakow/', jakaFunkcja="mean",DlaIluPlikow=2)
