###SCRIPT COM A MANIPULAÇÃO DOS DADOS PARA CRIAÇÃO DE FIGURAS

##CARREGANDO PACOTES E ARQUIVOS-------------
{
  
  packages <-
    c('ggplot2', 'readxl', 'dplyr', 'readr', 'data.table', 'esquisse',
      'sf',
      'tidyr','car','plyr','tidyverse')
  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)}})
  load('p.rda')
  ##PADRONIZANDO TEMA
  theme_set(
    theme_classic(base_size = 10) +
      theme(
        text = element_text(family = "Arial"),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  )
  
}
## 1.  GRÁFICO DE CORRELAÇÃO DAS MERCADORIAS--------------
###MANIPULAÇÃO DOS DADOS
p_mercadoria <- pivot_longer(
  p_mercadoria,cols = everything())

###REMOÇÃO DAQUELAS CATEGORIAS QUE APRESENTARAM P>0,05
p_mercadoria <- p_mercadoria %>% filter(name!="pval_peso_container"& name !="rho.est_container")
p_mercadoria <- p_mercadoria %>% filter(name !="pval_n_outros" & name !="rho.est_n_outros")
p_mercadoria <- p_mercadoria %>% filter(name !="ano")
##CRIAÇÃO DO DATAFRAME COM AS INFORMAÇÕES SOBRE PESO MOVIMENTADO
mercadoria_peso <- p_mercadoria %>% filter(str_detect(name, "rho.est_peso"))
##CRIAÇÃO DO DATAFRAME COM AS INFORMAÇÕES SOBRE NÚMERO DE OPERAÇÕES
mercadoria_n <- p_mercadoria %>% filter(str_detect(name, "rho.est_n"))

###RENOMEANDO AS LINHAS DOS DOIS DATAFRAMES
{
  mercadoria_peso <- mercadoria_peso %>% dplyr::mutate(name = ifelse(name == "rho.est_peso_maquinario",'MAQUINÁRIO E MATERIAL INDUSTRIAL, MATERIAL DE TRANSPORTE E DERIVADOS' , name))
  mercadoria_peso <- mercadoria_peso %>% dplyr::mutate(name = ifelse(name == "rho.est_peso_commodities","OUTROS COMMODITIES E DERIVADOS" , name))
  mercadoria_peso <- mercadoria_peso %>% dplyr::mutate(name = ifelse(name == "rho.est_peso_outros", "OUTROS", name))
  mercadoria_peso <- mercadoria_peso %>% dplyr::mutate(name = ifelse(name == "rho.est_peso_animal","PRODUTOS DO REINO ANIMAL" , name))
  mercadoria_peso <- mercadoria_peso %>% dplyr::mutate(name = ifelse(name == "rho.est_peso_vegetal","PRODUTOS DO REINO VEGETAL" , name))
  mercadoria_peso <- mercadoria_peso %>% dplyr::mutate(name = ifelse(name == "rho.est_peso_container", "CONTAINERES", name))
  mercadoria_peso <- mercadoria_peso %>% dplyr::mutate(name = ifelse(name == "rho.est_peso_combustivel","COMBUSTIVEIS MINERAIS, DERIVADOS DO PETROLEO E OUTROS DERIVADOS DO PETROLEO" , name))
  mercadoria_peso <- mercadoria_peso %>% dplyr::mutate(name = ifelse(name == "rho.est_pesototal", "TOTAL AGREGADO", name))
  
  mercadoria_n <- mercadoria_n %>% dplyr::mutate(name = ifelse(name == "rho.est_n_maquinario",'MAQUINÁRIO E MATERIAL INDUSTRIAL, MATERIAL DE TRANSPORTE E DERIVADOS' , name))
  mercadoria_n <- mercadoria_n %>% dplyr::mutate(name = ifelse(name == "rho.est_n_commodities","OUTROS COMMODITIES E DERIVADOS" , name))
  mercadoria_n <- mercadoria_n %>% dplyr::mutate(name = ifelse(name == "rho.est_n_outros", "OUTROS", name))
  mercadoria_n <- mercadoria_n %>% dplyr::mutate(name = ifelse(name == "rho.est_n_animal","PRODUTOS DO REINO ANIMAL" , name))
  mercadoria_n <- mercadoria_n %>% dplyr::mutate(name = ifelse(name == "rho.est_n_vegetal","PRODUTOS DO REINO VEGETAL" , name))
  mercadoria_n <- mercadoria_n %>% dplyr::mutate(name = ifelse(name == "rho.est_n_container", "CONTAINERES", name))
  mercadoria_n <- mercadoria_n %>% dplyr::mutate(name = ifelse(name == "rho.est_n_combustivel","COMBUSTIVEIS MINERAIS, DERIVADOS DO PETROLEO E OUTROS DERIVADOS DO PETROLEO" , name))
  mercadoria_n <- mercadoria_n %>% dplyr::mutate(name = ifelse(name == "rho.est_ntotal", "TOTAL AGREGADO", name))
}
## 1.  CORRELAÇÃO DE MERCADORIAS PELO PESO---------------

ggplot(mercadoria_peso) +
  aes(x = name,  y = value, fill = str_wrap(name, 23)) +
  geom_col() +
  scale_fill_brewer(palette = "Accent", 
                    direction = 1) + 
  labs(y = "Correlação", fill = "Legenda",title="Peso movimentado pelas mercadorias pelo PIB")+  
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  geom_text(aes(label = sprintf("%0.2f", value, digits=2)),position=position_dodge(width=1), vjust=-0.2, size=3)

#  ggsave('figures/mercadoria_peso_PIB.jpg',             DESATIVAR O COMENTÁRIO CASO QUEIRA SALVAR AS FIGURAS NOVAMENTE, MAS ELAS ESTÃO SALVAS NA PASTA FIGURES
#  width = 17,
#  height = 15,
#  units = 'cm',
#  dpi = 250)


## 1.1 CORRELAÇÃO DE MERCADORIAS PELO NÚMERO DE OPERAÇÕES----------------------------
ggplot(mercadoria_n) +
  aes(x = name, fill = str_wrap(name, 23), y= value) +
  geom_col() +
  scale_fill_brewer(palette = "Accent", 
                    direction = 1) +
  labs(x = "Mercadoria", y = "Correlação", fill = "Mercadorias",title="N° de operações das mercadorias pelo PIB") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  geom_text(aes(label = sprintf("%0.2f", value, digits=2)),position=position_dodge(width=1), vjust=-0.2, size=3)


#ggsave(
#  'figures/mercadoria_n_PIB.jpg',           DESATIVAR O COMENTÁRIO CASO QUEIRA SALVAR AS FIGURAS NOVAMENTE, MAS ELAS ESTÃO SALVAS NA PASTA FIGURES
#  width = 17,
#  height = 15,
#  units = 'cm',
#  dpi = 250)


## 2.  GRÁFICO DE CORRELAÇÃO PARA AS MESORREGIÕES-----------

##SEPARAÇÃO DAS TABELAS COM BASE NO VALOR DO TESTE DE HIPOTESE
pmeso_n <- p_meso %>% filter(pval_n<0.05)
pmeso_peso <- p_meso %>% filter(pval_peso<0.05)

## 2.1 GRÁFICO DE CORRELAÇÃO PARA AS MESORREGIÕES POR NÚMERO DE OPERAÇÃO-----------
##ordenando o gráfico por região
pmeso_n$NM_MESO <- factor(pmeso_n$NM_MESO,                                   
                          levels = pmeso_n$NM_MESO[order(pmeso_n$NM_REGIAO.y, decreasing = FALSE)])

ggplot(arrange(pmeso_n, NM_REGIAO.y), aes(x = NM_MESO, y =rho.est_n, fill = NM_REGIAO.y, order=NM_REGIAO.y)) +
  geom_col() +labs(x="Nome das mesorregiões",y="Valor da correlação",fill="Grande Região",title="Correlação do N° de operações com o PIB regional")+
  scale_fill_hue(direction = 1) + scale_x_discrete(guide = guide_axis(angle = 90))+
  geom_text(aes(label = sprintf("%0.2f", rho.est_n, digits=2)),position=position_dodge(width=1), vjust=-0.2, size=3)
#ggsave(
#  'figures/n_PIB.jpg',              DESATIVAR O COMENTÁRIO CASO QUEIRA SALVAR AS FIGURAS NOVAMENTE, MAS ELAS ESTÃO SALVAS NA PASTA FIGURES
#  width = 17,
#  height = 15,
#  units = 'cm',
#  dpi = 250)

## 2.2 GRÁFICO DE CORRELAÇÃO PESO MOVIMENTADO-----------
pmeso_peso$NM_MESO <- factor(pmeso_peso$NM_MESO,                                   
                             levels = pmeso_peso$NM_MESO[order(pmeso_peso$NM_REGIAO.y, decreasing = FALSE)])

ggplot(arrange(pmeso_peso, NM_REGIAO.y), aes(x = NM_MESO, y =rho.est_n, fill = NM_REGIAO.y, order=NM_REGIAO.y)) +
  geom_col() +labs(x="Nome das mesorregiões",y="Valor da correlação",fill="Grande Região",title="Correlação do peso com o PIB regional")+
  scale_fill_hue(direction = 1) + scale_x_discrete(guide = guide_axis(angle = 90))+
  geom_text(aes(label = sprintf("%0.2f", rho.est_n, digits=2)),position=position_dodge(width=1), vjust=-0.2, size=3)
#ggsave(
#  'figures/peso_PIB.jpg',           DESATIVAR O COMENTÁRIO CASO QUEIRA SALVAR AS FIGURAS NOVAMENTE, MAS ELAS ESTÃO SALVAS NA PASTA FIGURES
#  width = 17,
#  height = 15,
#  units = 'cm',
#  dpi = 250)



##SCATERPLOT E HISTOGRAMAS-----------


ggplot(p) +
 aes(x = pib, y = pesototal, colour = NM_REGIAO.y) +
 geom_point(shape = "circle", size = 1.5) +
 scale_color_manual(values = c(Nordeste = "#F8766D", Norte = "#31B425", Sudeste = "#20AFEC", Sul = "#FFEF61"
 )) +
 labs(x = "PIB", y = "PESO TOTAL ", color = "GRANDE REGIÃO") +
 theme_minimal()
#ggsave('figures/scater_pib_peso.jpg', width=17 , height =15, units='cm', dpi=250)

ggplot(p) +
  aes(x = pib, y = ntotal, colour = NM_REGIAO.y) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_manual(values = c(Nordeste = "#F8766D", Norte = "#31B425", Sudeste = "#20AFEC", Sul = "#FFEF61"
  )) +
  labs(x = "PIB", y = "PESO TOTAL ", color = "GRANDE REGIÃO") +
  theme_minimal()
#ggsave('figures/scater_pib_n.jpg', width=17 , height =15, units='cm', dpi=250)

ggplot(p) +
 aes(x = pesototal, fill = NM_REGIAO.y) +
 geom_histogram(bins = 30L) +
 scale_fill_hue(direction = 1) +
 theme_minimal()
ggsave('figures/hist_peso.jpg', width=17 , height =15, units='cm', dpi=250)
ggplot(p) +
  aes(x = ntotal, fill = NM_REGIAO.y) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  theme_minimal()
ggsave('figures/hist_n.jpg', width=17 , height =15, units='cm', dpi=250)
ggplot(p) +
  aes(x = pib, fill = NM_REGIAO.y) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  theme_minimal()
ggsave('figures/hist_pib.jpg', width=17 , height =15, units='cm', dpi=250)

