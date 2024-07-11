if(!require(forecast)){install.packages("forecast")}
library(forecast)
if(!require(urca)){install.packages("urca")}
library(urca)
if(!require(lmtest)){install.packages("lmtest")}
library(lmtest)


testdf <- function(variable, adf_order){
  results_adf <- data.frame(order = -1, adf = 0,
                            p_adf = "", bgodfrey = 0, p_bg = 0)
  variable <- variable[!is.na(variable)]
  
  for(order in 0:adf_order){
    df.test_ <- ur.df(variable, type = c("drift"), lags = order)
    df_ <- df.test_@teststat[1]
    df_crit <- df.test_@cval[1, ]
    df_crit <- (df_ < df_crit) * 1
    p_adf <- ifelse (sum(df_crit) == 0,
                     ">10pct",
                     paste("<", names(df_crit)[min(which(df_crit == 1))],
                           sep = ""))
    resids_ <- df.test_@testreg$residuals
    bgtest_ <- bgtest(resids_ ~ 1, order = 1)
    bgodfrey <- bgtest_$statistic
    names(bgodfrey)<-NULL
    p_bg <- bgtest_$p.value
    
    results_adf <- rbind(results_adf,
                         data.frame(order = order,
                                    adf = df_,
                                    p_adf = p_adf,
                                    bgodfrey = bgodfrey,
                                    p_bg = p_bg)
    )
  }
  
  results_adf<-results_adf[results_adf$order>=0,]
  
  plot(variable,
       type = "l",
       col = "blue",
       lwd = 2,
       main = "Plot of the examined variable")
  
  return(results_adf)
}


# Dane zostały pobrane z Dziedzinowej Bazy Wiedzy utworzonej przez Główny Urząd Statystyczny

# Szereg niesezonowy - dane kwartalne dotyczące średniej ceny mieszkań z rynku pierwotnego w Olsztynie

ceny_mieszkan_df <- read.csv("property_prices_olsztyn.csv",
                          header = TRUE,
                          sep = ";",
                          dec = ',')

ceny_mieszkan_df <- ceny_mieszkan_df[,c('id_okres', 'id_daty', 'wartosc')]
ceny_mieszkan_df$id_okres <- ceny_mieszkan_df$id_okres-269

str(ceny_mieszkan_df)
ceny_mieszkan_df$wartosc <- as.numeric(ceny_mieszkan_df$wartosc)
ceny_mieszkan_df$id_daty <- as.numeric(ceny_mieszkan_df$id_daty)

ceny_mieszkan = ts(data=ceny_mieszkan_df$wartosc, frequency = 4,             
                      start=c(2010,1), end=c(2023,4)) 

plot(ceny_mieszkan, type = "l", main = "Średnia cena za 1m2 mieszkań", xlab="Czas", ylab="cena")

ceny_mieszkan_diff <- diff(ceny_mieszkan, 1)
plot(ceny_mieszkan_diff, type = "l", main = "Ceny mieszkan diff")


ceny_mieszkan_sqrt <- BoxCox(ceny_mieszkan,lambda=0.5)
ceny_mieszkan_log <- BoxCox(ceny_mieszkan,lambda=0)
BoxCox.lambda(ceny_mieszkan)                       # wybrana lambda = -0.6
ceny_mieszkan_autom <- BoxCox(ceny_mieszkan,lambda=-0.6)

par(mfrow = c(2,2))
plot(ceny_mieszkan, main="Bez transformacji", xlab="Czas", ylab="Cena")
plot(ceny_mieszkan_sqrt, main="lambda=0,5", xlab="Czas", ylab="Cena")
plot(ceny_mieszkan_log, main="lambda=0", xlab="Czas", ylab="Cena")
plot(ceny_mieszkan_autom, main="lambda=-0,6", xlab="Czas", ylab="Cena")


par(mfrow = c(1,1))
ceny_mieszkan_dekomp_add<-decompose(ceny_mieszkan, type="additive")
plot(ceny_mieszkan_dekomp_add, xlab="Czas")

ceny_mieszkan_dekomp_mult<-decompose(ceny_mieszkan, type="multiplicative")
plot(ceny_mieszkan_dekomp_mult, xlab="Czas")


m3<-filter(ceny_mieszkan, sides=2, filter=rep(1/3,3))       # Ruchoma srednia dla q=1
m11<-filter(ceny_mieszkan, sides=2, filter=rep(1/11,11))    # Ruchoma srednia dla q=5
m21<-filter(ceny_mieszkan, sides=2, filter=rep(1/21,21))    # Ruchoma srednia dla q=10
m_spencer<-filter(ceny_mieszkan,
                  sides=2,
                  filter=(1/320)*c(-3, -6, -5,3,21,46,67,74,67,46,21,3.-5,-6,-3))

plot(ceny_mieszkan,
          main=paste("Metoda symetrycznej ruchomej średniej"),
     col="black", lty=1, xlab='Czas', ylab='Cena')

lines(m3, col="green", lty=2)
lines(m11, col="blue", lty=2)
lines(m21, col="red", lty=2)
lines(m_spencer, col="orange", lty=2)


# legenda
legend("topleft",
       legend=c("rzeczywisty szereg",
                "ruchoma średnia (q=1)",
                "ruchoma średnia (q=5)",
                "ruchoma średnia (q=10)",
                "filtr Spencera (q=7)"),
       col=c("black", "green", "blue", "red", "orange"),
       lty=c(1,2,2,2,2))




# Dobór parametrów do modelu ARIMA

testdf(variable = ceny_mieszkan, adf_order = 4)
tsdisplay(ceny_mieszkan)
# ACF wygasa bardzo wolno, co wskazuje na niestacjonarność

ceny_mieszkan_diff1 <- diff(ceny_mieszkan, 1)
testdf(variable = ceny_mieszkan_diff1, adf_order = 4)
# pierwsze różnice szeregu sa stacjonarne, rząd d = 1
tsdisplay(ceny_mieszkan_diff1, xlab="Czas", ylab="delta ceny")


## Identyfikacja rzędów p i q
Acf(ceny_mieszkan_diff1,  lag.max = 30)
Pacf(ceny_mieszkan_diff1, lag.max = 30)

### Modele

arima010 <- arima(ceny_mieszkan, order = c(0, 1, 0),xreg = 1:length(ceny_mieszkan)) 
arima010

arima011 <- arima(ceny_mieszkan, order = c(0, 1, 1),xreg = 1:length(ceny_mieszkan)) 
arima011

arima110 <- arima(ceny_mieszkan, order = c(1, 1, 0),xreg = 1:length(ceny_mieszkan)) 
arima110

summary(arima010)
summary(arima011)
summary(arima110)

Box.test(resid(arima010), type = "Ljung-Box", lag=24) 
Box.test(resid(arima011), type = "Ljung-Box", lag=24) 
Box.test(resid(arima110), type = "Ljung-Box", lag=24) 

tsdiag(arima010, gof.lag=12)
tsdiag(arima011, gof.lag=12)
tsdiag(arima110, gof.lag=12)

par(mfrow = c(1,2))
Acf(resid(arima010), lag.max = 24,
    ylim = c(-0.3, 0.3),
    lwd = 7, col = "dark green", main="") 
Pacf(resid(arima010), lag.max = 24,
     lwd = 7, col = "dark green", main="", ylab="PACF")
par(mfrow = c(1,1))

par(mfrow = c(1,2))
Acf(resid(arima011), lag.max = 24,
    ylim = c(-0.3, 0.3),
    lwd = 7, col = "dark green", main="") 
Pacf(resid(arima011), lag.max = 24,
     lwd = 7, col = "dark green", main="", ylab="PACF")
par(mfrow = c(1,1))


par(mfrow = c(1,2))
Acf(resid(arima110), lag.max = 24,
    ylim = c(-0.3, 0.3),
    lwd = 7, col = "dark green", main="") 
Pacf(resid(arima110), lag.max = 24,
     lwd = 7, col = "dark green", main="", ylab="PACF")
par(mfrow = c(1,1))



AIC(arima010, arima011, arima110) 
BIC(arima010, arima011, arima110)





# Prognozowanie

ceny_mieszkan_train<-window(ceny_mieszkan, end=c(2022,4))
ceny_mieszkan_test<-window(ceny_mieszkan, start=c(2023,1))


# Prognozowanie - Model ekstrapolacyjny
ceny_mieszkan_holt<-holt(ceny_mieszkan_train, h=4)
plot(ceny_mieszkan_holt)

summary(ceny_mieszkan_holt)


# Prognozowanie - ARIMA
arima010=arima(ceny_mieszkan_train, order=c(0,1,0))
forecast_arima010=predict(arima010, 4)

arima011=arima(ceny_mieszkan_train, order=c(0,1,1))
forecast_arima011=predict(arima011, 4)

arima110=arima(ceny_mieszkan_train, order=c(1,1,0))
forecast_arima110=predict(arima110, 4)


plot(ceny_mieszkan, type='l', xlim=c(2020, 2024), ylim=c(4000, 10000), xlab='Czas', ylab='Cena')
# początek okresu prognozy
abline(v = 2023, lty =22, col = "gray")
lines(forecast_arima010$pred, col = "red", lwd = 2)
lines(forecast_arima010$pred + 2 * forecast_arima010$se, col = "red", lty = 22)
lines(forecast_arima010$pred - 2 * forecast_arima010$se, col = "red", lty = 22)

lines(forecast_arima010$pred, col = "blue", lwd = 2)
lines(forecast_arima010$pred + 2 * forecast_arima010$se, col = "blue", lty = 22)
lines(forecast_arima010$pred - 2 * forecast_arima010$se, col = "blue", lty = 22)

lines(forecast_arima010$pred, col = "deeppink", lwd = 2)
lines(forecast_arima010$pred + 2 * forecast_arima010$se, col = "deeppink", lty = 22)
lines(forecast_arima010$pred - 2 * forecast_arima010$se, col = "deeppink", lty = 22)

lines(ceny_mieszkan_holt$mean, col = "green", lwd = 2)
lines(ceny_mieszkan_holt$upper[,2], col="green", lty=22)        # przedziałów ufności 95%
lines(ceny_mieszkan_holt$lower[,2], col="green", lty=22)


# legenda
legend("topleft",
       legend=c("rzeczywisty szereg",
                "ARIMA(0,1,0)",
                "ARIMA(0,1,1)",
                "ARIMA(1,1,0)",
                "metoda Holta"),
       col=c("black","red", "blue", "deeppink", "green"),
       lty=c(1,1,1),
       lwd=c(2,2,2))

# Porównanie modeli

jakosc_prognozy_arima010 <- data.frame(forecast = forecast_arima010$pred, ceny_mieszkan_test)
jakosc_prognozy_arima011 <- data.frame(forecast = forecast_arima011$pred, ceny_mieszkan_test)
jakosc_prognozy_arima110 <- data.frame(forecast = forecast_arima110$pred, ceny_mieszkan_test)
jakosc_prognozy_holt <- data.frame(forecast = ceny_mieszkan_holt$mean, ceny_mieszkan_test)

# sprawdzamy jakość prognozy
jakosc_prognozy_arima010$mae <- abs(jakosc_prognozy_arima010$ceny_mieszkan_test -
                                      jakosc_prognozy_arima010$forecast)
jakosc_prognozy_arima010$mse <- (jakosc_prognozy_arima010$ceny_mieszkan_test -
                                   jakosc_prognozy_arima010$forecast)^2
jakosc_prognozy_arima010$mape <- abs((jakosc_prognozy_arima010$ceny_mieszkan_test -
                                        jakosc_prognozy_arima010$forecast) /
                                       jakosc_prognozy_arima010$ceny_mieszkan_test)
jakosc_prognozy_arima010$amape <- abs((jakosc_prognozy_arima010$ceny_mieszkan_test -
                                         jakosc_prognozy_arima010$forecast) /
                                        (jakosc_prognozy_arima010$ceny_mieszkan_test +
                                           jakosc_prognozy_arima010$forecast))

# sprawdzamy jakość prognozy
jakosc_prognozy_arima011$mae <- abs(jakosc_prognozy_arima011$ceny_mieszkan_test -
                                      jakosc_prognozy_arima011$forecast)
jakosc_prognozy_arima011$mse <- (jakosc_prognozy_arima011$ceny_mieszkan_test -
                                   jakosc_prognozy_arima011$forecast)^2
jakosc_prognozy_arima011$mape <- abs((jakosc_prognozy_arima011$ceny_mieszkan_test -
                                        jakosc_prognozy_arima011$forecast) /
                                       jakosc_prognozy_arima011$ceny_mieszkan_test)
jakosc_prognozy_arima011$amape <- abs((jakosc_prognozy_arima011$ceny_mieszkan_test -
                                         jakosc_prognozy_arima011$forecast) /
                                        (jakosc_prognozy_arima011$ceny_mieszkan_test +
                                           jakosc_prognozy_arima011$forecast))

# sprawdzamy jakość prognozy
jakosc_prognozy_arima110$mae <- abs(jakosc_prognozy_arima110$ceny_mieszkan_test -
                                      jakosc_prognozy_arima110$forecast)
jakosc_prognozy_arima110$mse <- (jakosc_prognozy_arima110$ceny_mieszkan_test -
                                   jakosc_prognozy_arima110$forecast)^2
jakosc_prognozy_arima110$mape <- abs((jakosc_prognozy_arima110$ceny_mieszkan_test -
                                        jakosc_prognozy_arima110$forecast) /
                                       jakosc_prognozy_arima110$ceny_mieszkan_test)
jakosc_prognozy_arima110$amape <- abs((jakosc_prognozy_arima110$ceny_mieszkan_test -
                                         jakosc_prognozy_arima110$forecast) /
                                        (jakosc_prognozy_arima110$ceny_mieszkan_test +
                                           jakosc_prognozy_arima110$forecast))

# sprawdzamy jakość prognozy
jakosc_prognozy_holt$mae <- abs(jakosc_prognozy_holt$ceny_mieszkan_test -
                                  jakosc_prognozy_holt$forecast)
jakosc_prognozy_holt$mse <- (jakosc_prognozy_holt$ceny_mieszkan_test -
                               jakosc_prognozy_holt$forecast)^2
jakosc_prognozy_holt$mape <- abs((jakosc_prognozy_holt$ceny_mieszkan_test -
                                    jakosc_prognozy_holt$forecast) /
                                   jakosc_prognozy_holt$ceny_mieszkan_test)
jakosc_prognozy_holt$amape <- abs((jakosc_prognozy_holt$ceny_mieszkan_test -
                                     jakosc_prognozy_holt$forecast) /
                                    (jakosc_prognozy_holt$ceny_mieszkan_test +
                                       jakosc_prognozy_holt$forecast))

colMeans(jakosc_prognozy_arima010[, 3:6]) 
colMeans(jakosc_prognozy_arima011[, 3:6])
colMeans(jakosc_prognozy_arima110[, 3:6])
colMeans(jakosc_prognozy_holt[, 3:6]) 








# Szereg sezonowy - dane kwartalne dotyczące liczby turystów (w tysiącach) korzystających z 
# turystycznych obiektów noclegowych w województwie Warmińsko-Mazurskim
turysci_df <- read.csv("turists.csv",
                    header = TRUE,
                    sep = ";",
                    dec = ',')

turysci_df <- turysci_df[,c('id_okres', 'id_daty', 'wartosc')]
turysci_df$id_okres <- turysci_df$id_okres-269

str(turysci_df)
turysci_df$wartosc <- as.numeric(turysci_df$wartosc)
turysci_df$id_daty <- as.numeric(turysci_df$id_daty)

turysci = ts(data=turysci_df$wartosc, frequency = 4,             
                start=c(2010,1), end=c(2023,4)) 

par(mfrow = c(1,1))
plot(turysci, type = "b", main="", xlab="Czas", ylab="Liczba turystów (w tyś.)")

turysci_diff1 <- diff(turysci, 1)
plot(turysci_diff1, type = "l", main = "Turysci diff")

turysci_diff4 <- diff(turysci, 4)                           # różnicowanie po cyklu
plot(turysci_diff4, type = "l", main = "Turysci diff 4")

par(mfrow = c(1,2))
turysci_do_2016 <- window(turysci, end=c(2016, 4))
turysci_od_2017 <- window(turysci, start=c(2017, 1))
seasonplot(turysci_do_2016,
           col=c("violet", "blue","deeppink", "darkgrey", "yellow", "orange", "red"),
           pch=19,
           main="",
           xlab="Kwartał",
           ylab="Liczba turystów (w tyś.)")
legend("topleft",
       legend=seq(2010, 2016, 1),
       col=c("violet", "blue","deeppink", "darkgrey", "yellow", "orange", "red"),
       lty=c(1,1,1,1,1,1,1),
       lwd=c(3,3,3,3,3,3,3))

seasonplot(turysci_od_2017,
           col=c("violet", "blue","deeppink", "darkgrey", "yellow", "orange", "red"),
           pch=19,
           main="",
           xlab="Kwartał",
           ylab="Liczba turystów (w tyś.)")
legend("topleft",
       legend=seq(2017, 2023, 1),
       col=c("violet", "blue","deeppink", "darkgrey", "yellow", "orange", "red"),
       lty=c(1,1,1,1,1,1,1),
       lwd=c(3,3,3,3,3,3,3))
par(mfrow = c(1,1))

turysci_sqrt <- BoxCox(turysci,lambda=0.5)
turysci_log <- BoxCox(turysci,lambda=0)
BoxCox.lambda(turysci)                       # wybrana lambda = 1.19
turysci_autom <- BoxCox(turysci,lambda=1.19)

par(mfrow = c(2,2))
plot(turysci, main="Bez transformacji", xlab="Czas", ylab="Liczba turystów (w tyś.)")
plot(turysci_sqrt, main="lambda=0.5", xlab="Czas", ylab="Liczba turystów (w tyś.)")
plot(turysci_log, main="lambda=0", xlab="Czas", ylab="Liczba turystów (w tyś.)")
plot(turysci_autom, main="lambda=1.19", xlab="Czas", ylab="Liczba turystów (w tyś.)")


par(mfrow = c(1,1))
turysci_dekomp_add<-decompose(turysci, type="additive")
plot(turysci_dekomp_add, xlab="Czas")

turysci_dekomp_mult<-decompose(turysci, type="multiplicative")
plot(turysci_dekomp_mult, xlab="Czas")


m3<-filter(turysci, sides=2, filter=rep(1/3,3))       # Ruchoma srednia dla q=1
m11<-filter(turysci, sides=2, filter=rep(1/9,9))    # Ruchoma srednia dla q=4
m21<-filter(turysci, sides=2, filter=rep(1/17,17))    # Ruchoma srednia dla q=8
m_spencer<-filter(turysci,
                  sides=2,
                  filter=(1/320)*c(-3, -6, -5,3,21,46,67,74,67,46,21,3.-5,-6,-3))

plot(turysci,
     main=paste("Metoda symetrycznej ruchomej średniej"),
     col="black", lty=1, xlab='Czas', ylab='Liczba turystów (w tyś.)')

lines(m3, col="green", lty=2)
lines(m11, col="blue", lty=2)
lines(m21, col="red", lty=2)
lines(m_spencer, col="orange", lty=2)


# legenda
legend("topleft",
       legend=c("rzeczywisty szereg",
                "ruchoma średnia (q=1)",
                "ruchoma średnia (q=4)",
                "ruchoma średnia (q=8)",
                "filtr Spencera (q=7)"),
       col=c("black", "green", "blue", "red", "orange"),
       lty=c(1,2,2,2,2))




# Sprawdzamy stacjonarnosc
testdf(variable = turysci, adf_order = 4)              # funkcja na początku pliku
tsdisplay(turysci)             # ACF dla wielokrotności 4 (co cykl) powoli wygładza się, dla wielokrotności 2 ciężko stwierdzić czy się wygładza


turysci_diff4 <- diff(turysci, 4)                           # różnicowanie po cyklu
plot(turysci_diff4, type = "l", main = "Turysci diff 4")
tsdisplay(turysci_diff4, xlab="Czas", ylab="sezonowa delta liczby turystów")

turysci_diff4_1 <- diff(turysci_diff4, 1)                           # różnicowanie po cyklu
plot(turysci_diff4_1, type = "l", main = "Turysci diff 4_1")
tsdisplay(turysci_diff4_1, xlab="Czas")

testdf(variable = turysci, adf_order = 4)
testdf(variable = turysci_diff4, adf_order = 4)
testdf(variable = turysci_diff4_1, adf_order = 4)

Acf(turysci_diff4_1, lag.max = 36) 
Pacf(turysci_diff4_1, lag.max = 36) 

SARIMA010010<- arima(turysci,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 0),period = 4))
SARIMA010010

SARIMA010011<- arima(turysci,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),period = 4))
SARIMA010011

SARIMA010110<- arima(turysci,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(1, 1, 0),period = 4))
SARIMA010110

SARIMA010111<- arima(turysci,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(1, 1, 1),period = 4))
SARIMA010111


# Test Ljung-Boxa
Box.test(resid(SARIMA010010), type = "Ljung-Box", lag = 36)
Box.test(resid(SARIMA010011), type = "Ljung-Box", lag = 1)
Box.test(resid(SARIMA010110), type = "Ljung-Box", lag = 1)
Box.test(resid(SARIMA010111), type = "Ljung-Box", lag = 1)

# Wykresy diagnostyczne
tsdiag(SARIMA010010, gof.lag=12)
tsdiag(SARIMA010011, gof.lag=12)
tsdiag(SARIMA010110, gof.lag=12)
tsdiag(SARIMA010111, gof.lag=12)

# Korelogramy dla reszt z modelu 
par(mfrow = c(2, 1))
Acf(resid(SARIMA010010), lag.max = 36,
    lwd = 7, col = "dark green")
Pacf(resid(SARIMA010010), lag.max = 36,
     lwd = 7, col = "dark green")
par(mfrow = c(1,1))

par(mfrow = c(2, 1))
Acf(resid(SARIMA010011), lag.max = 36,
    lwd = 7, col = "dark green")
Pacf(resid(SARIMA010011), lag.max = 36,
     lwd = 7, col = "dark green")
par(mfrow = c(1,1))

par(mfrow = c(2, 1))
Acf(resid(SARIMA010110), lag.max = 36,
    lwd = 7, col = "dark green")
Pacf(resid(SARIMA010110), lag.max = 36,
     lwd = 7, col = "dark green")
par(mfrow = c(1,1))

par(mfrow = c(2, 1))
Acf(resid(SARIMA010111), lag.max = 36,
    lwd = 7, col = "dark green")
Pacf(resid(SARIMA010111), lag.max = 36,
     lwd = 7, col = "dark green")
par(mfrow = c(1,1))

# Kryteria informacyjne
AIC(SARIMA010010, SARIMA010011, SARIMA010110, SARIMA010111) 
BIC(SARIMA010010, SARIMA010011, SARIMA010110, SARIMA010111) 




# Parametry (p, d, q)

SARIMA010011<- arima(turysci,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),period = 4))
SARIMA010011

SARIMA011011<- arima(turysci,
                     # rzędy (p,d,q)
                     order = c(0, 1, 1),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),period = 4))
SARIMA011011

SARIMA110011<- arima(turysci,
                     # rzędy (p,d,q)
                     order = c(1, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),period = 4))
SARIMA110011

SARIMA111011<- arima(turysci,
                     # rzędy (p,d,q)
                     order = c(1, 1, 1),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),period = 4))
SARIMA111011


# Test Ljung-Boxa
Box.test(resid(SARIMA010011), type = "Ljung-Box", lag = 36)
Box.test(resid(SARIMA011011), type = "Ljung-Box", lag = 36)
Box.test(resid(SARIMA110011), type = "Ljung-Box", lag = 36)
Box.test(resid(SARIMA111011), type = "Ljung-Box", lag = 36)

# Wykresy diagnostyczne
tsdiag(SARIMA010011, gof.lag=12)
tsdiag(SARIMA011011, gof.lag=12)
tsdiag(SARIMA110011, gof.lag=12)
tsdiag(SARIMA111011, gof.lag=12)

# Korelogramy dla reszt z modelu 
par(mfrow = c(2, 1))
Acf(resid(SARIMA010011), lag.max = 36,
    lwd = 7, col = "dark green")
Pacf(resid(SARIMA010011), lag.max = 36,
     lwd = 7, col = "dark green")
par(mfrow = c(1,1))

par(mfrow = c(2, 1))
Acf(resid(SARIMA011011), lag.max = 12,
    lwd = 7, col = "dark green")
Pacf(resid(SARIMA011011), lag.max = 12,
     lwd = 7, col = "dark green")
par(mfrow = c(1,1))

par(mfrow = c(2, 1))
Acf(resid(SARIMA110011), lag.max = 12,
    lwd = 7, col = "dark green")
Pacf(resid(SARIMA110011), lag.max = 12,
     lwd = 7, col = "dark green")
par(mfrow = c(1,1))

par(mfrow = c(2, 1))
Acf(resid(SARIMA111011), lag.max = 12,
    lwd = 7, col = "dark green")
Pacf(resid(SARIMA111011), lag.max = 12,
     lwd = 7, col = "dark green")
par(mfrow = c(1,1))

# nowe wykresy reszt
par(mfrow = c(3, 2))
Acf(resid(SARIMA011011), lag.max = 12,
    lwd = 7, col = "dark green")
Pacf(resid(SARIMA011011), lag.max = 12,
     lwd = 7, col = "dark green", ylab="PACF")

Acf(resid(SARIMA110011), lag.max = 12,
    lwd = 7, col = "dark green")
Pacf(resid(SARIMA110011), lag.max = 12,
     lwd = 7, col = "dark green", ylab="PACF")

Acf(resid(SARIMA111011), lag.max = 12,
    lwd = 7, col = "dark green")
Pacf(resid(SARIMA111011), lag.max = 12,
     lwd = 7, col = "dark green", ylab="PACF")

# Kryteria informacyjne
AIC(SARIMA010011, SARIMA011011, SARIMA110011, SARIMA111011) 
BIC(SARIMA010011, SARIMA011011, SARIMA110011, SARIMA111011)

AIC(SARIMA011011, SARIMA110011, SARIMA111011) 
BIC(SARIMA011011, SARIMA110011, SARIMA111011)











tsdisplay(turysci_diff4_1)
SARIMA<- arima(turysci,
                     # rzędy (p,d,q)
                     order = c(0, 1, 1),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 2, 1),period = 4))
SARIMA
Box.test(resid(SARIMA), type = "Ljung-Box", lag = 36)
tsdiag(SARIMA, gof.lag=24)
par(mfrow = c(2, 1))
Acf(resid(SARIMA), lag.max = 36,
    lwd = 7, col = "dark green")
Pacf(resid(SARIMA), lag.max = 36,
     lwd = 7, col = "dark green")
par(mfrow = c(1,1))
AIC(SARIMA010010, SARIMA011010, SARIMA110010, SARIMA111010, SARIMA) 
BIC(SARIMA010010, SARIMA011010, SARIMA110010, SARIMA111010, SARIMA)


# 010011 561.2594 565.1231, 110011 554.6822 560.4777, 011011 552.9816 558.7771, 011021 536.5748 542.1253

##### Metoda Holta-Wintersa

turysci_train<-window(turysci, end=c(2022,4))
turysci_test<-window(turysci, start=c(2023,1))

turysci_add<-hw(turysci_train, h=4,seasonal="additive")
turysci_mult<-hw(turysci_train, h=4,seasonal="multiplicative")

par(mfrow = c(2, 1))
plot(turysci_add)
plot(turysci_mult)

# Wykresy dopasowania
plot(turysci_train, xlab='Czas', ylab='Turyści (w tys.)')
lines(fitted(turysci_add), col="blue", lty=2)
lines(fitted(turysci_mult), col="red", lty=2)
legend("topleft",
       legend=c("wyjściowy szereg",
                "metoda addytywna",
                "metoda multiplikatywna"),
       col=c("black", "blue", "red"),
       lty=c(1,2,2,2))

turysci_add$model
turysci_mult$model

#### Prognozowanie

SARIMA011011<- arima(turysci_train,
                     # rzędy (p,d,q)
                     order = c(0, 1, 1),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),period = 4))
SARIMA110011<- arima(turysci_train,
                     # rzędy (p,d,q)
                     order = c(1, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),period = 4))
SARIMA111011<- arima(turysci_train,
                     # rzędy (p,d,q)
                     order = c(1, 1, 1),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),period = 4))

forecast_SARIMA011011=predict(SARIMA011011, 4)
forecast_SARIMA110011=predict(SARIMA110011, 4)
forecast_SARIMA111011=predict(SARIMA111011, 4)


par(mfrow = c(1, 1))
plot(turysci, type='l', xlab='Czas', ylab='Liczba turystów (w tyś.)', xlim=c(2020, 2024), ylim=c(0, 700))
# początek okresu prognozy
abline(v = 2023, lty =22, col = "gray")
lines(forecast_SARIMA011011$pred, col = "red", lwd = 2)
#lines(forecast_SARIMA011011$pred + 2 * forecast_SARIMA011011$se, col = "red", lty = 22)
#lines(forecast_SARIMA011011$pred - 2 * forecast_SARIMA011011$se, col = "red", lty = 22)

lines(forecast_SARIMA110011$pred, col = "blue", lwd = 2)
#lines(forecast_SARIMA110011$pred + 2 * forecast_SARIMA110011$se, col = "blue", lty = 22)
#lines(forecast_SARIMA110011$pred - 2 * forecast_SARIMA110011$se, col = "blue", lty = 22)

lines(forecast_SARIMA111011$pred, col = "yellow", lwd = 2)
#lines(forecast_SARIMA010011$pred + 2 * forecast_SARIMA010011$se, col = "yellow", lty = 22)
#lines(forecast_SARIMA010011$pred - 2 * forecast_SARIMA010011$se, col = "yellow", lty = 22)


# legenda
legend("topleft",
       legend=c("rzeczywisty szereg",
                "SARIMA(0,1,1)(0,1,1)",
                "SARIMA(1,1,0)(0,1,1)",
                "SARIMA(1,1,1)(0,1,1)"),
       col=c("black", "red", "blue", "yellow"),
       lty=c(1,1,1,1))

plot(turysci, type='l', xlab='Czas', ylab='Liczba turystów (w tyś.)', xlim=c(2020, 2024), ylim=c(0, 700))
# początek okresu prognozy
abline(v = 2023, lty =22, col = "gray")

lines(turysci_add$mean, col='green', lwd=2)
lines(ts(data = data.frame(as.matrix(turysci_add$upper), date=time(turysci_add$upper))$X95.,start=123), col = "green", lty = 22)
lines(ts(data = data.frame(as.matrix(turysci_add$lower), date=time(turysci_add$upper))$X95.,start=123), col = "green", lty = 22)

lines(turysci_mult$mean, col='orange', lwd=2)
lines(ts(data = data.frame(as.matrix(turysci_mult$upper), date=time(turysci_mult$upper))$X95.,start=123), col = "orange", lty = 22)
lines(ts(data = data.frame(as.matrix(turysci_mult$lower), date=time(turysci_mult$upper))$X95.,start=123), col = "orange", lty = 22)

# legenda
legend("topleft",
       legend=c("rzeczywisty szereg",
                "metoda addytywna",
                "metoda multiplikatywna"),
       col=c("black", "green", "orange"),
       lty=c(1,1,1))


# Porównanie modeli

forecast_SARIMA011011$pred
turysci_test


jakosc_prognozy_SARIMA011011 <- data.frame(forecast = forecast_SARIMA011011$pred, turysci_test)
jakosc_prognozy_SARIMA110011 <- data.frame(forecast = forecast_SARIMA110011$pred, turysci_test)
jakosc_prognozy_SARIMA111011 <- data.frame(forecast = forecast_SARIMA111011$pred, turysci_test)
jakosc_prognozy_add <- data.frame(forecast = turysci_add$mean, turysci_test)
jakosc_prognozy_mult <- data.frame(forecast = turysci_mult$mean, turysci_test)


# sprawdzamy jakość prognozy
jakosc_prognozy_SARIMA011011$mae <- abs(jakosc_prognozy_SARIMA011011$turysci_test -
                                          jakosc_prognozy_SARIMA011011$forecast)
jakosc_prognozy_SARIMA011011$mse <- (jakosc_prognozy_SARIMA011011$turysci_test -
                                       jakosc_prognozy_SARIMA011011$forecast)^2
jakosc_prognozy_SARIMA011011$mape <- abs((jakosc_prognozy_SARIMA011011$turysci_test -
                                            jakosc_prognozy_SARIMA011011$forecast) /
                                           jakosc_prognozy_SARIMA011011$turysci_test)
jakosc_prognozy_SARIMA011011$amape <- abs((jakosc_prognozy_SARIMA011011$turysci_test -
                                             jakosc_prognozy_SARIMA011011$forecast) /
                                            (jakosc_prognozy_SARIMA011011$turysci_test +
                                               jakosc_prognozy_SARIMA011011$forecast))

# sprawdzamy jakość prognozy
jakosc_prognozy_SARIMA110011$mae <- abs(jakosc_prognozy_SARIMA110011$turysci_test -
                                          jakosc_prognozy_SARIMA110011$forecast)
jakosc_prognozy_SARIMA110011$mse <- (jakosc_prognozy_SARIMA110011$turysci_test -
                                       jakosc_prognozy_SARIMA110011$forecast)^2
jakosc_prognozy_SARIMA110011$mape <- abs((jakosc_prognozy_SARIMA110011$turysci_test -
                                            jakosc_prognozy_SARIMA110011$forecast) /
                                           jakosc_prognozy_SARIMA110011$turysci_test)
jakosc_prognozy_SARIMA110011$amape <- abs((jakosc_prognozy_SARIMA110011$turysci_test -
                                             jakosc_prognozy_SARIMA110011$forecast) /
                                            (jakosc_prognozy_SARIMA110011$turysci_test +
                                               jakosc_prognozy_SARIMA110011$forecast))
# sprawdzamy jakość prognozy
jakosc_prognozy_SARIMA111011$mae <- abs(jakosc_prognozy_SARIMA111011$turysci_test -
                                          jakosc_prognozy_SARIMA111011$forecast)
jakosc_prognozy_SARIMA111011$mse <- (jakosc_prognozy_SARIMA111011$turysci_test -
                                       jakosc_prognozy_SARIMA111011$forecast)^2
jakosc_prognozy_SARIMA111011$mape <- abs((jakosc_prognozy_SARIMA111011$turysci_test -
                                            jakosc_prognozy_SARIMA111011$forecast) /
                                           jakosc_prognozy_SARIMA111011$turysci_test)
jakosc_prognozy_SARIMA111011$amape <- abs((jakosc_prognozy_SARIMA111011$turysci_test -
                                             jakosc_prognozy_SARIMA111011$forecast) /
                                            (jakosc_prognozy_SARIMA111011$turysci_test +
                                               jakosc_prognozy_SARIMA111011$forecast))


# sprawdzamy jakość prognozy
jakosc_prognozy_add$mae <- abs(jakosc_prognozy_add$turysci_test -
                                 jakosc_prognozy_add$forecast)
jakosc_prognozy_add$mse <- (jakosc_prognozy_add$turysci_test -
                              jakosc_prognozy_add$forecast)^2
jakosc_prognozy_add$mape <- abs((jakosc_prognozy_add$turysci_test -
                                   jakosc_prognozy_add$forecast) /
                                  jakosc_prognozy_add$turysci_test)
jakosc_prognozy_add$amape <- abs((jakosc_prognozy_add$turysci_test -
                                    jakosc_prognozy_add$forecast) /
                                   (jakosc_prognozy_add$turysci_test +
                                      jakosc_prognozy_add$forecast))

# sprawdzamy jakość prognozy
jakosc_prognozy_mult$mae <- abs(jakosc_prognozy_mult$turysci_test -
                                  jakosc_prognozy_mult$forecast)
jakosc_prognozy_mult$mse <- (jakosc_prognozy_mult$turysci_test -
                               jakosc_prognozy_mult$forecast)^2
jakosc_prognozy_mult$mape <- abs((jakosc_prognozy_mult$turysci_test -
                                    jakosc_prognozy_mult$forecast) /
                                   jakosc_prognozy_mult$turysci_test)
jakosc_prognozy_mult$amape <- abs((jakosc_prognozy_mult$turysci_test -
                                     jakosc_prognozy_mult$forecast) /
                                    (jakosc_prognozy_mult$turysci_test +
                                       jakosc_prognozy_mult$forecast))
options(scipen=999)


colMeans(jakosc_prognozy_SARIMA011011[, 3:6]) 
colMeans(jakosc_prognozy_SARIMA110011[, 3:6])
colMeans(jakosc_prognozy_SARIMA111011[, 3:6]) 
colMeans(jakosc_prognozy_add[, 3:6]) 
colMeans(jakosc_prognozy_mult[, 3:6]) 









