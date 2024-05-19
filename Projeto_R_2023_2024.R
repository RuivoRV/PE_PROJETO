# PROBLEMA_1

# Definir o diretório de trabalho
setwd("C:\\Users\\inesr\\Downloads\\PE\\Projeto_2023_2024")

# Ler csv
dados <- read.csv("Paises_PIB_ICH.csv")
print(dados)
continentes <- c("Europe", "Americas")
decada <- 20
library(ggplot2)
theme_set(theme_gray())

dados |>
  subset(Years = decada, Continent %in% continentes) |>
  ggplot() +
  geom_point(aes(x = GDP, y = HCI, color = Continent)) +
  scale_x_log10() +
  labs(title = paste("GDP VS HCI na década de:", decada),
       x = "GDP per capita (2023)",
       y = "HCI (data from 2020)") +
  geom_text(data = subset(dados, Country %in% c("Lithuania", "Iceland", "United States", "Saint Lucia")),
            aes(x = GDP, y = HCI, label = Country),
            hjust = -0.1, vjust = -0.5, size = 3)  # Ajuste para colocar o texto próximo aos pontos


# PROBLEMA_2:

# Definir o diretório de trabalho
setwd("C:\\Users\\inesr\\Downloads\\PE\\Projeto_2023_2024")

dados_2 <- read.csv("master.csv")
print(dados_2)

# Dois diagramas de extremos e quartis = dois boxplots
library(ggplot2)

# Dados para o ano de 2002 e o grupo etário 55-74 years
dados_2a <- subset(dados_2, year == 2002 & age == "55-74 years")

# Criar um gráfico de caixa comparando homens e mulheres

ggplot(dados_2a, aes(x = sex, y = suicides.100k.pop, fill = sex)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#009cff", "#ff1493")) +  # Cor escolhida para cada sexo
  labs(title = "Número de suicídios por 100.000 habitantes em 2002 na faixa etária de 55-74 anos",
       x = "Sexo do indivídou",
       y = "Suicídios por 100.000 habitantes") +
  theme_minimal()

# PROBLEMA_3: ---- CORRIGIR!!!!!!!!!!

# Definir o diretório de trabalho:
setwd("C:\\Users\\inesr\\Downloads\\PE\\Projeto_2023_2024")

# install readxl previamente!!!
library(readxl) 

# Ler Excel: 

dados_3 <- read_excel("electricity.xlsx")
#print(dados_3)

#… with 181,905 more rows, and abbreviated variable names ¹​MONTH_NAME, ²​DISPLAY_ORDER,
#   ³​yearToDate, ⁴​previousYearToDate
# ℹ Use `print(n = ...)` to see more rows

library(ggplot2)
# Subset dos dados a partir de 2015
dados_3a <- subset(dados_3, YEAR >= 2015)

# Filtrar os dados para incluir apenas as informações do enunciado:

dados_3b <- subset(dados_3a, PRODUCT == "Renewables")
print(dados_3b)


dados_3c <- subset(dados_3b, COUNTRY %in% c("Latvia", "Italy", "IEA Total"), select = VALUE)
print(dados_3c)

# Converter a coluna VALUE para numérica
dados_3c$VALUE <- as.numeric(dados_3c$VALUE)

# Verificar se a conversão foi bem-sucedida
print(class(dados_3c$VALUE))

# Filtrar dados_3b para conter apenas os países relevantes
dados_3b_relevantes <- subset(dados_3b, COUNTRY %in% c("Latvia", "Italy", "IEA Total"))

# Adicionar as colunas MONTH_NAME e COUNTRY aos dados já convertidos
dados_3c$MONTH_NAME <- dados_3b_relevantes$MONTH_NAME
dados_3c$COUNTRY <- dados_3b_relevantes$COUNTRY

# Organizar os meses corretamente
dados_3c$MONTH_NAME <- factor(dados_3c$MONTH_NAME, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Calcular a proporção de energia elétrica renovável em percentagem
dados_3c$PERCENT_RENEWABLE <- dados_3c$VALUE / sum(dados_3c$VALUE) * 100


# Criar o gráfico usando ggplot2
ggplot(dados_3c, aes(x = MONTH_NAME, y = PERCENT_RENEWABLE, color = COUNTRY, group = COUNTRY)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  labs(x = "Mês", y = "Proporção de Energia Renovável (%)", title = "Evolução Mensal da Proporção de Energia Renovável") +
  theme_minimal()


# PROBLEMA_4:

# Fixar a semente! Início
set.seed(2336)

num_circuits <- 7 
num_sims <- 650

warning_count <- 0
off_count <- 0

for(i in 1:num_sims){
  signals <- sapply(1:9, function(x) rbinom(num_circuits, 1, x/45))
  
  if(any(signals[,2] == 1)){
    warning_count <- warning_count + 1 
  }
  
  if(any(signals[,1] == 1)){
    off_count <- off_count + 1
  }
}

prop <- round(warning_count / (num_sims - off_count), 14)

print(prop)

# PROBLEMA_5 

# Parametros:

n <- 23
m <- 170
r <- 300

# Semente:

set.seed(1950)

# Iniciação de um vector para guardar proporçoes
props <- numeric(r)

# Generate r samples:

for(i in 1:r){
  Z <- rnorm(n+1) # Generate n+1 standard normals
  
# Calculo do "T statistic" 
  T <- (n^0.5) * (Z[1] / sqrt(sum(Z[-1]^2))) 
  
# Guardar as proporçoes abaixo de 1.5
  props[i] <- mean(T <= 1.5) 
}

#Média
p_empirical <- mean(props)

# T Verdadeira probabilidade da "t- distribution 
p_true <- pt(q = 1.5, df = n) 

# Diferença absoluta
diff <- abs(p_empirical - p_true)

# Multiply by 100 and round Multiplicar por 100 e arredondar:
round(diff * 100, 5)

# PROBLEMA_6:

# Parâmetros
set.seed(1973)
n_simulations <- 1000
n_components <- 40
mean <- 4
threshold <- 126

# Simulação: 

samples <- matrix(rexp(n_simulations * n_components, rate=1/mean), nrow=n_simulations, byrow=TRUE)
Y_simulated <- rowSums(samples)

# Probabilidade simulada
prob_simulated <- mean(Y_simulated > threshold)

# Parâmetros da distribuição gama:
k <- n_components
theta <- 1 / mean

# Probabilidade exata:
prob_exact <- 1 - pgamma(threshold, shape=k, scale=theta)

# Calculando a diferença entre os resultados
difference <- abs(prob_simulated - prob_exact) * 100
difference_rounded <- round(difference, 4)

# Resultados:
cat("Probabilidade simulada:", prob_simulated, "\n")
cat("Probabilidade exata:", prob_exact, "\n")
cat("Diferença entre as abordagens (em %):", difference_rounded)


# PROBLEMA_7:

# Observed data
x <- c(8.54, 4.76, 5.15, 4.96, 6.25, 7.22, 12.9, 6.04, 8.86, 4.88, 6.54, 4.53, 4.7, 5.38, 5.96, 5.17, 5.09, 5.11)

# Instalar biblioteca:

library(stats4)

# Initial value 
theta0 <- 3.4

# Find MLE of theta
mle_theta <- mle(x,  
                 minuslogl = function(theta) -sum(dgamma(x, shape = theta, log = TRUE)), 
                 start = list(theta = theta0))

# Extract theta MLE
theta_mle <- coef(mle_theta)[1] 

# Calculate 0.25 quantile using MLE
q_mle <- qgamma(p = 0.25, shape = theta_mle)

# Calculate true quantile when theta = 3.4  
q_true <- qgamma(p = 0.25, shape = 3.4)

# Absolute deviation 
abs_dev <- abs(q_mle - q_true)

# Round to 4 decimal places
round(abs_dev, 4)



# PROBLEMA_8: CORRE MAS.........

# Step 1: Fix the seed and extract a random sample
set.seed(1592)
data <- c(31.8, 31.7, 35.2, 37.1, 31.7, 36.1, 36.3, 33.2, 34.3, 37.5, 30.4, 34.6, 32.4, 31.7, 30.2, 34.3, 35.6, 34.9, 38.9)
n <- 12
sample <- sample(data, n, replace = FALSE)

# Step 2: Obtain the confidence interval for sigma^2
gamma <- 0.96
alpha <- (1 - gamma) / 2
s2 <- var(sample)

# Quantiles of the chi-square distribution
a <- qchisq(alpha, df = n - 1)
b <- qchisq(1 - alpha, df = n - 1)

# Limits of the confidence interval for sigma^2
IC_inf <- (n - 1) * s2 / b
IC_sup <- (n - 1) * s2 / a

# Amplitude of the confidence interval
amplitude_IC1 <- IC_sup - IC_inf

# Step 3: Minimize the amplitude of the confidence interval using fsolve from the pracma package
library(pracma)

# Function to solve the system of equations
f <- function(x) {
  c <- x[1]
  d <- x[2]
  eq1 <- pchisq(d, df = n - 1) - pchisq(c, df = n - 1) - gamma
  eq2 <- dchisq(d, df = n + 3) - dchisq(c, df = n + 3)
  return(c(eq1, eq2))
}

# Numerical solution using fsolve
sol <- fsolve(f, c(a, b))

c <- sol[[1]]
d <- sol[[2]]

# Limits of the new confidence interval for sigma^2
IC2_inf <- (n - 1) * as.numeric(s2) / as.numeric(d)
IC2_sup <- (n - 1) * as.numeric(s2) / as.numeric(c)

# Amplitude of the new confidence interval
amplitude_IC2 <- IC2_sup - IC2_inf

# Difference in the amplitudes of the confidence intervals
difference_amplitude <- (amplitude_IC1) - (amplitude_IC2)

# Results
cat("Sample:", sample, "\n")
cat("Original confidence interval for sigma^2:", IC_inf, "a", IC_sup, "\n")
cat("Amplitude of the original confidence interval:", amplitude_IC1, "\n")
cat("Optimized confidence interval for sigma^2:", IC2_inf, "a", IC2_sup, "\n")
cat("Amplitude of the optimized confidence interval:", amplitude_IC2, "\n")
cat("Difference in the amplitudes:", round(difference_amplitude, 3), "\n")

# PROBLEMA_9:

set.seed(2822)

# Parâmetros das distribuições de Poisson
lambda_0 <- 2.90
lambda_1 <- 3.15

# Número de amostras
m <- 5000

# Tamanho da amostra
n <- 100

# Contadores para os tipos de decisões erradas
erro_tipo_1 <- 0  # Erro de 1ª espécie: rejeitar H0 quando H0 é verdadeira
erro_tipo_2 <- 0  # Erro de 2ª espécie: não rejeitar H0 quando H1 é verdadeira

# Gerar amostras e realizar o teste de hipóteses para cada par de amostras
for (i in 1:m) {
  # Gerar amostras sob H0 e H1
  amostra_H0 <- rpois(n, lambda_0)
  amostra_H1 <- rpois(n, lambda_1)
  
  # Calcular média das amostras
  media_amostra_H0 <- mean(amostra_H0)
  media_amostra_H1 <- mean(amostra_H1)
  
  # Teste de hipóteses: rejeitar H0 se média > k
  k <- 3.234
  if (media_amostra_H0 > k) {
    erro_tipo_1 <- erro_tipo_1 + 1  # Erro de 1ª espécie
  }
  if (media_amostra_H1 <= k) {
    erro_tipo_2 <- erro_tipo_2 + 1  # Erro de 2ª espécie
  }
}

# Calcular frequências relativas
frequencia_erro_tipo_1 <- erro_tipo_1 / m
frequencia_erro_tipo_2 <- erro_tipo_2 / m

# Calcular o quociente entre a probabilidade de erro de 2ª espécie e a probabilidade de erro de 1ª espécie
quociente <- frequencia_erro_tipo_2 / frequencia_erro_tipo_1

# Imprimir resultado
print(quociente)

# PROBLEMA_10:

# Amostra observada
amostra <- c(6.52, 5.48, 7.01, 7.07, 9.76, 7.45, 10.11, 8.78, 8.53, 7.14, 8.56, 8.22, 8.11, 8.72, 9.89, 12.01, 9.14, 7.26, 11.05, 12.14, 8.44, 10.98, 6.95, 11.37, 8.65, 11.03, 10.55, 8.77, 10.04, 9.70, 9.43, 8.91, 10.79, 12.38, 7.41, 10.80, 8.40, 8.16, 9.22, 11.29, 8.54, 12.57, 8.53, 9.12, 8.53, 8.62, 12.03, 7.64, 11.13, 7.18, 8.16, 9.29, 6.89, 10.45, 11.20, 9.38, 8.79, 4.71, 9.35, 10.80, 10.90, 9.86, 9.50, 8.17, 11.19, 7.89, 8.72, 8.74, 8.87, 8.63, 9.28, 6.58, 10.74, 8.12, 6.92, 6.89, 5.19, 10.73, 10.38, 8.74, 8.96, 8.60, 4.92, 7.48, 8.96, 10.96, 8.08, 7.15, 8.82, 5.69, 8.09, 10.21, 5.80, 9.76, 11.90, 8.73, 7.10, 11.13, 5.07, 9.43, 8.04, 10.76, 7.95, 9.06, 9.39, 7.70, 5.92, 9.32, 10.53, 10.26, 6.31, 6.34, 5.36, 7.75, 7.74, 8.42, 10.76, 11.39, 8.42, 9.79, 9.45, 9.17, 7.91, 5.79, 5.70, 8.74, 10.33, 11.00, 6.17, 9.11, 9.57, 11.82, 10.35, 8.37, 6.66, 6.97, 9.66, 12.18, 10.80, 11.14)

# Definir o número de classes
k <- 6

# Calcular a amplitude de cada classe
amplitude_classe <- (13 - 4.5) / k

# Inicializar vetor para armazenar as frequências observadas e esperadas
frequencias_obs <- rep(0, k)
frequencias_esperadas <- rep(0, k)

# Definir a função de distribuição acumulada (CDF) corrigida
F_triangular <- function(x, a, b) {
  ifelse(x < a, 0,
         ifelse(x < (a + b) / 2, 2 * ((x - a) / (b - a))^2,
                ifelse(x < b, 1 - 2 * ((b - x) / (b - a))^2,
                       ifelse(x >= b, 1, NA)
                )
         )
  )
}

# Calcular as frequências observadas e esperadas para cada classe
for (i in 1:k) {
  a <- 4.5 + (i - 1) * amplitude_classe
  b <- a + amplitude_classe
  
  # Contar o número de observações na classe
  frequencias_obs[i] <- sum(amostra >= a & amostra < b)
  
  # Calcular a probabilidade acumulada em b e a
  F_b <- F_triangular(b, 4.5, 13)
  F_a <- F_triangular(a, 4.5, 13)
  
  # Calcular a frequência esperada para a classe
  frequencias_esperadas[i] <- length(amostra) * (F_b - F_a)
}

# Calcular a estatística do qui-quadrado
chi_squared <- sum((frequencias_obs - frequencias_esperadas)^2 / frequencias_esperadas)

# Graus de liberdade: número de classes - número de parâmetros estimados
df <- k - 1

# Calcular o valor-p
p_value <- 1 - pchisq(chi_squared, df)

p_value


