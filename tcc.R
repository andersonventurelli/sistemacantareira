#Instalação dos Pacotes
pacotes <- c("readxl","forecast","crayon","ggplot2","tibble","zoo","dplyr","urca","lmtest")

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
library(dplyr)
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
Decomposição STL da Série Temporal
#############################################################################

vol_canta_ts <- as.ts(vol_canta)

#Decomposição STL
decomposicao_stl <- stl(vol_canta_ts, s.window = 365)
plot(decomposicao_stl)

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

############################################################################
#MODELO ARIMA 
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

#############################################################################
#MODELO SARIMA - Resultou no mesmo modelo ARIMA (2,1,2)
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


