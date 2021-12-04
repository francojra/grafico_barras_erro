
# Gráfico de barras com erro padrão --------------------------------------------------------------------------------------------------------

# Baixar dados -----------------------------------------------------------------------------------------------------------------------------

datasets::ToothGrowth
dados <- ToothGrowth # Nomeando o conjunto de dados

# Transformar variáveis categóricas como fator ---------------------------------------------------------------------------------------------

dados$supp <- as.factor(dados$supp) # Variável categórica com dois níveis
dados$dose <- as.factor(dados$dose) # Variável categórica y
dados$len <- as.numeric(dados$len) # Variável numérica x

# Verificar variáveis numéricas e categóricas ----------------------------------------------------------------------------------------------------------------------

library(dplyr) # Pacote que serve para verificar os dados e calcular médias e desvios-padrão
glimpse(dados)

# Calcular médias, desvios-padrão e erros-padrão -------------------------------------------------------------------------------------------

# drop_na pode ser adicionado a função caso seja necessário retirar dados faltantes da análise

dados_tooth <- dados %>%
  group_by(supp, dose) %>% # Agrupar média e erro-padrão por dose e tipo de suplemento (supp)
  dplyr::summarise(media = mean(len),  # Cálculo da média
                   se = sd(len) / sqrt(length(len))) # Cálculo do erro-padrão
dados_tooth # Novo conjunto de dados

# Gráfico ----------------------------------------------------------------------------------------------------------------------------------

library(ggplot2) # Pacote para produzir o gráfico

ggplot(dados_tooth, aes(x = dose, y = media, fill = supp)) +
  geom_col(position = "dodge") +
   geom_errorbar(aes(x = dose, ymin = media - se, ymax = media + se), 
                width = 0.14, position = position_dodge(.9), size = 0.9) +
  scale_fill_manual(values = c("#8c510a","#01665e"),
                    name = "Tipo de suplemento", breaks = c("OJ", "VC"),
                    labels = c("Suco de laranja", "Vitamina C")) +
  labs(x = "Dose (mg)", y = "Comprimento dentário") +
  theme_bw(base_size = 15)
