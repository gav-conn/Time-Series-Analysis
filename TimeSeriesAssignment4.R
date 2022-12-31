library("forecast")
library("TSA")
library("lmtest")
library("tseries")
library("urca")

# Q1. a)
x = c(-1, 6, 1, 20, 28, 14, 30, 21, 18, 20)
plot(x, type = 'l')
modelx = Arima(x, order = c(1, 0, 0), include.drift = T)
coeftest(modelx)

# b)
forecast(modelx, h = 10)$mean
# Model includes a drift component, meaning the model will tend toward infinity as expectation of the random component = 0
plot(x, xlim = c(0,20), ylim = c(0,50))
lines(forecast(modelx, h = 10)$mean)
lines(fitted(modelx))

# Q2. a)
data(prescrip)
prescrip
plot(prescrip)

adf.test(prescrip, k = 0)
# fail to reject null hypothesis of unit root

# b)
n = length(prescrip)
tt = 2:n
y= diff(prescrip)
fit = lm(y~tt+prescrip[-n])
yhat = fitted(fit)

SSM = sum(yhat^2)
SSE = sum((y-yhat)^2)
dofM = 3
dofE = n-4 # length of y = n, so dofE = n-1-3 = n-4
phi2 = (SSM/dofM)/(SSE/dofE)
phi2 # phi2 = 8.8117

# c)
test = ur.df(prescrip, type = 'trend', lags = 0)
summary(test)

# Q3. a)
data(beersales)
tsdisplay(beersales)
# Clear seasonal component to time series period of 12 lags
# magnitude of seasonal component does not seem to grow with time, suggesting an additive decomposition.
# seems to be a increasing trend to time series, with trend levelling off later in the series

adf.test(beersales)
# ADF test reports p-value < 0.01, so we reject null hypothesis and conclude there is no unit root & that series is stationary.

# b)
TC = ma(beersales, 12) # chose order 12 for MA smoother as peaks/troughs of ACF separated by 12 time lags 
plot(TC)

# c)
pseudo_S = beersales-TC
matrix_S = matrix(pseudo_S, nrow = 12)
s = rowMeans(matrix_S, na.rm = TRUE)
S = s-mean(s)
R = beersales - TC - S
plot(rep(S, 16), type = 'l')
plot(R)

# d)
linear_TC = lm(TC~time(beersales))
t = deltat(beersales)
newdata = seq(1991, 1993-t, t)
pred_TC = linear_TC$coefficients[2]*newdata + linear_TC$coefficients[1]
pred_beersales = pred_TC + rep(S, 2)

# e)
LinTC = linear_TC$coefficients[2]*time(beersales) + linear_TC$coefficients[1]
fit_beersales = LinTC + rep(S, 16)
res_fit = beersales - fit_beersales
plot(res_fit)
tsdisplay(res_fit) # ACF & PACF plots of residuals both tail off indicating an ARMA process

model_res = Arima(res_fit, order = c(1, 0, 1), include.mean = F)
coeftest(model_res)
f_res = forecast(model_res, h = 30)

msef = pred_beersales + f_res$mean[-c(1:6)]
msef
plot(msef, type = 'l')

plot(beersales, xlim=c(time(beersales)[1], newdata[length(newdata)]), ylim=c(8, 20))
lines(newdata, msef, col=4)