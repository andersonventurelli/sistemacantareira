#Instalação dos Pacotes
pacotes <- c("readxl","forecast","crayon","ggplot2","tibble","zoo","knitr","dplyr","tseries","urca","lmtest")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Carregando bibliotecas
library(forecast)
library(readxl)
library(crayon)
library(ggplot2)
library(tibble)
library(zoo)
library(knitr)
library(dplyr)
library(tseries) #testes para verificar estacionariedade das série temporal
library(urca)
library(lmtest)

#Carregamento dos dados do Sistema Cantareira do Portal dos Mananciais da Sabesp. https://mananciais-sabesp.fcth.br/HistoricoSistemas?SistemaId=0
dados <- read_excel("dadoscompletos.xlsx")
print(dados) #Visualização dos dados.

#Estatisticas descritivas dos dados
summary(dados)

#Estrutura dos dados
str(dados)

#Criação de um  dataframe somente com as variavéis Data e Volume  (hm³) do Sistema Cantareira..
df <- dados 
df <- df %>%
  select(Data, `Volume (hm³)`)
print(df)

#Colocar os dados em ordem crescente. Dados originais em ordem decrescente de 2000 - 2024.
df$`Volume (hm³)` <- rev(df$`Volume (hm³)`) #inverter a ordem dos dados da data e do volume.
df$Data <- rev(df$Data)

# Convertendo o a variavel Data de tipo "char" para o tipo "Date" no padrão do RStudio. Ano/Mês/Dia.
df$Data <- as.Date(df$Data, format = "%d/%m/%Y")
print(df)

#Criação dos vetores Data e Volume (hm³) para utilizar a função zoo para criação da serie temporal
volume <- df$`Volume (hm³)`
data <- df$Data 

#Criação da Serie Temporal
vol_canta <- zoo(volume, order.by = data)

#Plot do Gráfico da Serie Temporal
plot(vol_canta, main = "Volúme Diário do Sistema Cantareira", xlab = "Ano", ylab = "Volume (hm³)",xaxt = "n") #Plotar gráfico
grid()
anos_para_exibir <- seq(2000, 2025, by = 1)
axis(1, at = as.Date(paste0(anos_para_exibir, "-01-01")), labels = anos_para_exibir, las = 2)

########################FIM DA ETAPA DE PREPARAÇÂO DOS DADOS ################################################

#############################################################################
#Teste de Estacionariedade dos dados ( Dickey- Fuller e KPSS)
#############################################################################

#Verificação se a série temporal é estacionaria ou não.

#Teste de Dickey- Fuller Aumentado (ADF)

vol_canta %>% ur.df() %>% summary()

#test-statistic = -0,3463. test-statistic >-1,95 (Critério de 5% de significancia) a série contém pelo menos uma raiz unitaria, e  não é estacionaria

#Teste de KPSS 
vol_canta %>% ur.kpss() %>% summary()

#test-statistic = 4,5077. test-statistic >0,463 (Critério de 5% de significancia) , e  não é estacionaria!


#############################################################################
#Teste de Sazonalidade na Série Temporal
#############################################################################

vol_canta_ts <- as.ts(vol_canta)
vol_canta <- ts(vol_canta, frequency = 365, start = c(2000,1))

ACF_Volume_Cantareira <- diff(vol_canta_ts, lag =90)  # Diferenciação sazonal
acf(ACF_Volume_Cantareira) #ACF - Autocorrelation Function 

#Decomposição Clássica
decomposicao <- decompose(vol_canta_ts)
plot(decomposicao)

#Decomposição STL
decomposicao_stl <- stl(vol_canta_ts, s.window = 365)
plot(decomposicao_stl)

#############################################################################
#Divisão base de dados treino e teste 70/30
#############################################################################

n_total <- length(vol_canta)
tamanho_treino <- round(70*n_total/100)
tamanho_treino

indices <- c(base::rep("treino", tamanho_treino), base::rep("teste", n_total - tamanho_treino))

# Usar split() para dividir a série temporal
split_series <- split(vol_canta, indices)

# Acessar treino e teste
treino7030 <- split_series$treino
teste7030 <- split_series$teste

#############################################################################
#Divisão base de dados treino e teste 80/20
#############################################################################

n_total <- length(vol_canta)
tamanho_treino <- round(80*n_total/100)
tamanho_treino

indices <- c(base::rep("treino", tamanho_treino), base::rep("teste", n_total - tamanho_treino))

# Usar split() para dividir a série temporal
split_series <- split(vol_canta, indices)

# Acessar treino e teste
treino8020 <- split_series$treino
teste8020 <- split_series$teste

#############################################################################
#Divisão base de dados treino e teste 90/10
#############################################################################

n_total <- length(vol_canta)
tamanho_treino <- round(90*n_total/100)
tamanho_treino

indices <- c(base::rep("treino", tamanho_treino), base::rep("teste", n_total - tamanho_treino))

# Usar split() para dividir a série temporal
split_series <- split(vol_canta, indices)

# Acessar treino e teste
treino9010 <- split_series$treino
teste9010 <- split_series$teste


############################################################################
#MODELO ARIMA - 70x30
#############################################################################
#Criação do Modelo
modelo_arima7030 <- auto.arima(treino7030) # Criação do modelo
summary(modelo_arima7030)
previsao_arima7030 <- forecast(modelo_arima7030, h = length(teste7030))

# Plotando as previsões de ARIMA
plot(previsao_arima7030, 
     main = "Previsões vs Dados Reais", 
     xlim = c(as.Date("2000-01-01"), as.Date("2025-01-01")), 
     xlab = "Ano", 
     ylab = "Volume (hm³)", 
     xaxt = "n")  # Desativar o eixo X padrão
    grid()

# Definindo os anos para exibição no eixo X
anos_para_exibir <- seq(2000, 2025, by = 1)

# Gerando os rótulos de data para o eixo X
# Supondo que a série temporal começa em 2000, ajustando conforme o número de pontos no seu teste
axis(1, 
     at = as.Date(paste0(anos_para_exibir, "-01-01")), 
     labels = anos_para_exibir, 
     las = 2)
# Adicionando os dados reais do teste em vermelho

lines(teste7030, col = 'red')
# Adicionando a legenda
legend("bottomright", 
       legend = c("Previsões ARIMA", "Dados Reais"), 
       col = c("blue", "red"), 
       lty = 1, 
       cex = 0.8)

# Verificar os Resíduos do Modelo Arima 
checkresiduals(modelo_arima7030)

# Teste de Box-Ljung (autocorelação dos residuos)
Box.test(modelo_arima7030$residuals, lag=1,type=c("Ljung-Box"))

############################################################################
#MODELO ARIMA - 80x20
#############################################################################
#Criação do Modelo
modelo_arima8020 <- auto.arima(treino8020,
                               stepwise = FALSE,
                               approximation = FALSE) # Criação do modelo
summary(modelo_arima8020)
previsao_arima8020 <- forecast(modelo_arima8020, h = length(teste8020))

coeftest(modelo_arima8020)

# Plotando as previsões de ARIMA
plot(previsao_arima8020, 
     main = "Modelo ARIMA: Previsões x Dados Reais", 
     xlim = c(as.Date("2000-01-01"), as.Date("2025-01-01")), 
     xlab = "Ano", 
     ylab = "Volume (hm³)", 
     xaxt = "n")  # Desativar o eixo X padrão
grid()

# Definindo os anos para exibição no eixo X
anos_para_exibir <- seq(2000, 2025, by = 1)

# Gerando os rótulos de data para o eixo X
# Supondo que a série temporal começa em 2000, ajustando conforme o número de pontos no seu teste
axis(1, 
     at = as.Date(paste0(anos_para_exibir, "-01-01")), 
     labels = anos_para_exibir, 
     las = 2)
# Adicionando os dados reais do teste em vermelho

lines(teste8020, col = 'red')
# Adicionando a legenda
legend("bottomright", 
       legend = c("Previsões ARIMA", "Dados Reais"), 
       col = c("blue", "red"), 
       lty = 1, 
       cex = 0.8)

# Verificar os Resíduos do Modelo Arima 
checkresiduals(modelo_arima8020)

# Teste de Box-Ljung (autocorelação dos residuos)
Box.test(modelo_arima8020$residuals, lag=1,type=c("Ljung-Box"))


############################################################################
#MODELO ARIMA - 90x10
#############################################################################
#Criação do Modelo
modelo_arima9010 <- auto.arima(treino9010) # Criação do modelo
summary(modelo_arima9010)
previsao_arima9010 <- forecast(modelo_arima9010, h = length(teste9010))

# Plotando as previsões de ARIMA
plot(previsao_arima9010, 
     main = "Previsões vs Dados Reais", 
     xlim = c(as.Date("2000-01-01"), as.Date("2025-01-01")), 
     xlab = "Ano", 
     ylab = "Volume (hm³)", 
     xaxt = "n")  # Desativar o eixo X padrão
grid()

# Definindo os anos para exibição no eixo X
anos_para_exibir <- seq(2000, 2025, by = 1)

# Gerando os rótulos de data para o eixo X
# Supondo que a série temporal começa em 2000, ajustando conforme o número de pontos no seu teste
axis(1, 
     at = as.Date(paste0(anos_para_exibir, "-01-01")), 
     labels = anos_para_exibir, 
     las = 2)
# Adicionando os dados reais do teste em vermelho

lines(teste9010, col = 'red')
# Adicionando a legenda
legend("bottomright", 
       legend = c("Previsões ARIMA", "Dados Reais"), 
       col = c("blue", "red"), 
       lty = 1, 
       cex = 0.8)

# Verificar os Resíduos do Modelo Arima 
checkresiduals(modelo_arima9010)

# Teste de Box-Ljung (autocorelação dos residuos)
Box.test(modelo_arima9010$residuals, lag=1,type=c("Ljung-Box"))

#############################################################################
#MODELO ETS ( Error, Trend, Seasonal)
#############################################################################

modelo_ets <- ets(treino8020)
summary(modelo_ets)

previsao_ets <- forecast(modelo_ets, h = length(teste8020))
summary(previsao_ets)


alpha_ic <- 0.9999 + c(-1.96, 1.96) * 0.0001  # IC para alpha
beta_ic  <- 0.2206 + c(-1.96, 1.96) * 0.05    # IC para beta
phi_ic   <- 0.9236 + c(-1.96, 1.96) * 0.02     # IC para phi
alpha_ic
beta_ic
phi_ic
plot(previsao_ets, main = "Modelo ETS: Previsões x Dados Reais", xlab = "Ano", ylab = "Volume(hm³)",xaxt = "n")
grid()
anos_para_exibir <- seq(2000, 2025, by = 1)
axis(1, at = as.Date(paste0(anos_para_exibir, "-01-01")), labels = anos_para_exibir, las = 2)
lines(teste8020, col = 'red')

# Verificar os Resíduos do Modelo ETS
checkresiduals(modelo_ets)

# Teste de Box-Ljung (autocorelação dos residuos)
Box.test(modelo_ets$residuals, lag=1,type=c("Ljung-Box"))


#############################################################################
#MODELO ETS ( Error, Trend, Seasonal) - 70/30
#############################################################################

modelo_ets <- ets(treino7030)
summary(modelo_ets)

previsao_ets <- forecast(modelo_ets, h = length(teste7030))
summary(previsao_ets)



plot(previsao_ets, main = "Modelo ETS: Previsões x Dados Reais", xlab = "Ano", ylab = "Volume(hm³)",xaxt = "n")
grid()
anos_para_exibir <- seq(2000, 2025, by = 1)
axis(1, at = as.Date(paste0(anos_para_exibir, "-01-01")), labels = anos_para_exibir, las = 2)
lines(teste8020, col = 'red')

# Verificar os Resíduos do Modelo ETS
checkresiduals(modelo_ets)

# Teste de Box-Ljung (autocorelação dos residuos)
Box.test(modelo_ets$residuals, lag=1,type=c("Ljung-Box"))



#############################################################################
#MODELO SARIMA - Ta igual o arima
#############################################################################

modelo_sarima <- auto.arima(treino8020,
                                 seasonal = TRUE,
                                 stepwise = TRUE,
                                 trace  = TRUE,
                                 approximation = FALSE)

previsao_sarima <- forecast(modelo_sarima, h = length(teste8020))

plot(previsao_sarima, main = "Modelo SARIMA: Previsões x Dados Reais", xlab = "Ano", ylab = "Volume(hm³)",xaxt = "n")
grid()
anos_para_exibir <- seq(2000, 2025, by = 1)
axis(1, at = as.Date(paste0(anos_para_exibir, "-01-01")), labels = anos_para_exibir, las = 2)
lines(teste8020, col = 'red')

summary(modelo_sarima)

# Verificar os Resíduos do Modelo sarima
checkresiduals(modelo_sarima)

# Teste de Box-Ljung (autocorelação dos residuos)
Box.test(modelo_sarima$residuals, lag=1,type=c("Ljung-Box"))


#############################################################################
#MODELO NNAR - 80/20
#############################################################################
modelo_nnar <- nnetar(treino8020)
previsao_nnar <- forecast(modelo_nnar, h = length(teste8020))
summary(modelo_nnar)
# Plotando as previsões do NNAR
plot(previsao_nnar, 
     main = "Modelo NNAR: Previsões x Dados Reais", 
     xlim = c(as.Date("2000-01-01"), as.Date("2025-01-01")), 
     xlab = "Ano", 
     ylab = "Volume (hm³)", 
     xaxt = "n")  # Desativar o eixo X padrão
grid()

# Definindo os anos para exibição no eixo X
anos_para_exibir <- seq(2000, 2025, by = 1)

# Gerando os rótulos de data para o eixo X
# Supondo que a série temporal começa em 2000, ajustando conforme o número de pontos no seu teste
axis(1, 
     at = as.Date(paste0(anos_para_exibir, "-01-01")), 
     labels = anos_para_exibir, 
     las = 2)
# Adicionando os dados reais do teste em vermelho

lines(teste8020, col = 'red')
# Adicionando a legenda
legend("bottomright", 
       legend = c("Previsões NNAR", "Dados Reais"), 
       col = c("blue", "red"), 
       lty = 1, 
       cex = 0.8)


plot(ts.union(Observado = modelo_nnar$x, 
              Ajustado = modelo_nnar$fitted),
     main = "Desempenho do Modelo no Treino")

accuracy(modelo_nnar)
checkresiduals(modelo_nnar)
Box.test(modelo_nnar$residuals, lag=1,type=c("Ljung-Box"))


#############################################################################
#MODELO NNAR - 70/30
#############################################################################
modelo_nnar <- nnetar(treino7030)
previsao_nnar <- forecast(modelo_nnar, h = length(teste7030))
summary(modelo_nnar)
# Plotando as previsões do NNAR
plot(previsao_nnar, 
     main = "Modelo NNAR: Previsões x Dados Reais", 
     xlim = c(as.Date("2000-01-01"), as.Date("2025-01-01")), 
     xlab = "Ano", 
     ylab = "Volume (hm³)", 
     xaxt = "n")  # Desativar o eixo X padrão
grid()

# Definindo os anos para exibição no eixo X
anos_para_exibir <- seq(2000, 2025, by = 1)

# Gerando os rótulos de data para o eixo X
# Supondo que a série temporal começa em 2000, ajustando conforme o número de pontos no seu teste
axis(1, 
     at = as.Date(paste0(anos_para_exibir, "-01-01")), 
     labels = anos_para_exibir, 
     las = 2)
# Adicionando os dados reais do teste em vermelho

lines(teste8020, col = 'red')
# Adicionando a legenda
legend("bottomright", 
       legend = c("Previsões NNAR", "Dados Reais"), 
       col = c("blue", "red"), 
       lty = 1, 
       cex = 0.8)


plot(ts.union(Observado = modelo_nnar$x, 
              Ajustado = modelo_nnar$fitted),
     main = "Desempenho do Modelo no Treino")

accuracy(modelo_nnar)
checkresiduals(modelo_nnar)
Box.test(modelo_nnar$residuals, lag=1,type=c("Ljung-Box"))


#############################################################################
#GRÁFICO COM OS VALORES REAIS EM COMPARAÇÃO COM OS TRES MODELOS (ARIMA)
#############################################################################
#Plotando as previsões do ARIMA
plot(previsao_arima7030, 
     main = "Previsões ARIMA VS Dados Reais",
     xlim = c(as.Date("2000-01-01"), as.Date("2024-10-24")),
     xlab = "Ano", 
     ylab = "Volume (hm³)", 
     xaxt = "n", 
     lwd = 1
     )         # Largura da linha para as previsões ARIMA

# Adicionando as previsões do modelo ARIMA
lines(previsao_arima8020$mean, col = "blue", lwd = 2)  # Previsões ARIMA em azul

# Adicionando as previsões do modelo ETS
lines(previsao_arima9010$mean, col = "green", lwd = 2)  # Previsões ETS em verde

# Adicionando os dados reais do teste em vermelho
lines(teste7030, col = 'red', lwd = 2)  # Dados reais em vermelho

lines(previsao_arima7030$mean, col = "cyan", lwd = 2) 

# Definindo os anos para exibição no eixo X
anos_para_exibir <- seq(2000, 2025, by = 1)
axis(1, 
     at = as.Date(paste0(anos_para_exibir, "-01-01")), 
     labels = anos_para_exibir, 
     las = 2)

# Adicionando a legenda no canto inferior direito
legend("bottomright", 
       legend = c("ARIMA(70x30)", "ARIMA(80x20)", "ARIMA(90x10)","Dados Reais"), 
       col = c("cyan", "blue", "green", "red"), 
       lty = 1, 
       lwd = 2, 
       cex = 0.8)

#############################################################################
#GRÁFICO COM OS VALORES REAIS EM COMPARAÇÃO COM OS TRES MODELOS (ARIMA, ETS e SARIMA)
#############################################################################
#Plotando as previsões do ARIMA
plot(previsao_arima8020, 
     main = "Previsões( ARIMA, ETS e NNAR) x Dados Reais",
     xlim = c(as.Date("2000-01-01"), as.Date("2024-10-24")),
     xlab = "Ano", 
     ylab = "Volume (hm³)", 
     xaxt = "n", 
     lwd = 1
)         # Largura da linha para as previsões ARIMA


# Adicionando as previsões do modelo ETS
lines(previsao_ets$mean, col = "green", lwd = 1.8)  # Previsões ETS em verde

# Adicionando as previsões do modelo NNAR
lines(previsao_nnar$mean, col = "blue", lwd = 1.8)  # Previsões ETS em verde

# Adicionando os dados reais do teste em vermelho
lines(teste8020, col = 'red', lwd = 1.8)  # Dados reais em vermelho

lines(previsao_arima8020$mean, col = "cyan", lwd = 1.8) 

# Definindo os anos para exibição no eixo X
anos_para_exibir <- seq(2000, 2025, by = 1)
axis(1, 
     at = as.Date(paste0(anos_para_exibir, "-01-01")), 
     labels = anos_para_exibir, 
     las = 2)

# Adicionando a legenda no canto inferior direito
legend("bottomright", 
       legend = c("ARIMA", "ETS", "NNAR","Dados Reais"), 
       col = c("cyan","green","blue", "red"), 
       lty = 1, 
       lwd = 1.8, 
       cex = 0.8)


