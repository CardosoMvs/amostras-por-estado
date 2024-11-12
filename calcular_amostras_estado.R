# Pacotes necessários
library(dplyr)
library(ggplot2)
library(readr)

# Caminhos dos arquivos
conj1 <- "C:/Users/marco/Downloads/amostras-por-estado-brasil_Conju1_10_30.csv"
beta <- "C:/Users/marco/Downloads/amostras-por-estado-brasil_beta.csv"

# Carregar os dados e adicionar uma coluna de origem para cada arquivo
data1 <- read_csv(conj1) %>% mutate(origem = "conj1")
data2 <- read_csv(beta) %>% mutate(origem = "beta")

# Combinar os dados
data_combined <- bind_rows(data1, data2)

# 1. Contagem de amostras por estado e por arquivo
count_by_state_file <- data_combined %>%
  group_by(estado, origem) %>%
  summarise(total_amostras = n_distinct(dataset_id), .groups = "drop")

# Salvar a contagem como CSV na pasta Downloads
write_csv(count_by_state_file, "C:/Users/marco/Downloads/contagem_amostras_por_estado_arquivo.csv")

# 2. Histograma da distribuição dos anos de coleta por estado e por arquivo
# Criar uma pasta para os gráficos, se ainda não existir
dir.create("C:/Users/marco/Downloads/histogramas_anos_por_estado_arquivo", showWarnings = FALSE)

# Gerar e salvar os histogramas por estado e por arquivo
for (state in unique(data_combined$estado)) {
  for (source in unique(data_combined$origem)) {
    # Filtrar dados para o estado e origem atuais
    data_state_source <- filter(data_combined, estado == state, origem == source)
    
    # Criar o histograma, se houver dados para essa combinação
    if (nrow(data_state_source) > 0) {
      plot <- ggplot(data_state_source, aes(x = data_coleta_ano)) +
        geom_histogram(binwidth = 2, fill = "blue", color = "black", alpha = 0.7) +
        labs(title = paste("Histograma de Anos de Coleta - Estado:", state, "- Origem:", source),
             x = "Ano de Coleta", y = "Frequência") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16),  # Tamanho do título
          axis.title.x = element_text(size = 14),  # Tamanho do rótulo do eixo x
          axis.title.y = element_text(size = 14),  # Tamanho do rótulo do eixo y
          axis.text.x = element_text(size = 12),  # Tamanho do texto do eixo x
          axis.text.y = element_text(size = 12)   # Tamanho do texto do eixo y
        )
      
      # Salvar o gráfico em um arquivo PNG
      ggsave(filename = paste0("C:/Users/marco/Downloads/histogramas_anos_por_estado_arquivo/histograma_", state, "_", source, ".png"), 
             plot = plot, width = 8, height = 6)
    }
  }
}
