# Memuat library yang diperlukan
library(ggplot2)
library(forecast)
library(tseries)
library(rpart)
library(TSA)
library(tidyverse)
library(lubridate)
library(dplyr)
library(BETS)

# Memuat dataset dan mengonversi kolom tanggal ke format datetime
df <- read.csv("C:/Users/HP/Downloads/JPM-PD.csv")
# Memeriksa nilai yang hilang
sum(is.na(df))
head(df$Date)
### Konversi dan pengurutan data berdasarkan tanggal
df <- df[order(df$Date),]  # Memastikan data terurut berdasarkan tanggal
df$Date <- as.Date(df$Date)

### Membuat time series dari harga penutupan
close_ts <- ts(df$Close, frequency = 12)

### Menampilkan struktur dari time series yang baru dibuat
print(close_ts)
plot(close_ts)
tsdisplay(close_ts)

### Visualisasi data harga penutupan seiring waktu dengan ggplot2
ggplot(df, aes(x = Date, y = Close, group = 1)) +
  geom_line() +
  ggtitle("Harga Penutupan Saham JPM") +
  xlab("Tahun") +
  ylab("Harga Penutupan") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

### Uji stasioneritas dengan Augmented Dickey-Fuller test
adf_test_weekly <- adf.test(close_ts, alternative = "stationary")
print(adf_test_weekly)  # Tampilkan hasil uji ADF

### Differencing untuk mencapai stasioneritas
datadiff <- diff(close_ts, differences = 1)
print(datadiff)  # Tampilkan data setelah differencing

### Uji stasioneritas lagi dengan ADF test setelah differencing
adf_test_datadiff <- adf.test(datadiff)
print(adf_test_datadiff)

### Menampilkan time series setelah differencing
tsdisplay(datadiff)
eacf(datadiff)

### Plot hasil differencing dan display ulang
plot(datadiff)
tsdisplay(datadiff)

### Model ARIMA
model1 <- Arima(close_ts, order=c(0,1,1))
model2 <- Arima(close_ts, order=c(1,1,0))
model3 <- Arima(close_ts, order=c(1,1,1))

### Ringkasan dan perbandingan model
summary(model1)
summary(model2)
summary(model3)

### Kalkulasi AIC dan BIC untuk pemilihan model
AIC(model1, model2, model3)
BIC(model1, model2, model3)

cbind(model1, model2, model3)

### Memilih model terbaik berdasarkan residuals
fit <- model2

# Uji Stasioneritas Residual
adf_test_fit <- adf.test(fit$residuals)
print(adf_test_fit)

# Uji Normalitas Residual
shapiro_test_fit <- shapiro.test(fit$residuals)
print(shapiro_test_fit)

# Uji Independensi Residual
lbtest_fit <- checkresiduals(fit)

# Uji Signifikansi Parameter ARIMA
t_test_fit <- t_test(fit, alpha = 0.05)
print(t_test_fit)

# Overfitting
overfit1 <- Arima(close_ts, order=c(2,1,0))
overfit1

overfit2 <- Arima(close_ts, order=c(1,1,1))
overfit2

cbind(overfit1,fit,overfit2)

# Uji Signifikansi Model Overfit
t_test_fit1 <- t_test(overfit1)
t_test_fit2 <- t_test(overfit2)
print(t_test_fit1)
print(t_test_fit2)

### Pemilihan model otomatis dengan auto.arima
best_model <- auto.arima(close_ts)
summary(best_model)

#Lakukan Peramalan
# Membagi data menjadi training dan testing untuk cross-validation
train_size <- floor(0.8 * length(close_ts))
train_data <- close_ts[1:train_size]
test_data <- close_ts[(train_size + 1):length(close_ts)]

# Melatih model pada training data
train_model <- auto.arima(train_data)

# Forecasting pada data test
forecast_horizon <- length(test_data)
forecast_test <- forecast(train_model, h = forecast_horizon)

# Menyusun dataframe hasil forecasting bersama dengan nilai actual
results <- data.frame(
  Date = df$Date[(train_size + 1):length(df$Date)],
  Actual = test_data,
  Forecast = as.numeric(forecast_test$mean)
)

# Menampilkan hasil forecasting beserta nilai actual
print(results)

# Menghitung MAE
mae <- mean(abs(results$Actual - results$Forecast))
print(paste("Mean Absolute Error (MAE):", mae))

# Menghitung MAPE
mape <- mean(abs((results$Actual - results$Forecast) / results$Actual) * 100)
print(paste("Mean Absolute Percentage Error (MAPE):", mape))

# Menghitung RMSE
rmse <- sqrt(mean((results$Actual - results$Forecast)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))

# Forecasting menggunakan model2 untuk 12 periode ke depan
forecast_future <- forecast(model2, h = 12)

# Menampilkan hasil forecasting
print(forecast_future)

# Plot hasil forecasting
autoplot(forecast_future) + 
  ggtitle("Forecasting Harga Penutupan Saham JPM untuk 12 Periode Kedepan") +
  xlab("Tahun") +
  ylab("Harga Penutupan") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))