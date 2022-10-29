##SCRIPT ONDE FOI REALIZADO AS CORRELAÇÕES A PARTIR DOS DADOS MANIPULADOS E FILTRADOS

###CARREGANDO PACOTES E ARQUIVOS-------------
{
  
  packages <-
    c('ggplot2', 'readxl', 'dplyr', 'readr', 'data.table', 'esquisse',
      'sf',
      'tidyr','car','plyr')
 {
   
     package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)}})
   }
  ###CRIAÇÃO DE UMA FUNÇÃO DE CORRELAÇÃO
  {
   corfun<-function(x, y) {
    corr=(cor.test(x, y, method="spearman",exact=NULL, conf.level= 0.95))} 
  
  }
  
  load('p.rda')
}



###CORRELAÇÃO DAS MOVIMENTAÇÕES POR NÚMERO DE OPERAÇÕES E PESO DE CADA MESORREGIÃO ----------------
{
  p_meso <-
    ddply(
      p,
      .(NM_MESO),
      summarise,
      pval_peso = corfun(pib, pesototal)$p.value,
      rho.est_peso = corfun(pib, pesototal)$estimate,
      pval_n = corfun(pib, ntotal)$p.value,
      rho.est_n = corfun(pib, ntotal)$estimate,
      pval_pesoporcapita = corfun(pibpercapita, pesototal)$p.value,
      rho.est_pesoporcapita = corfun(pibpercapita, pesototal)$estimate,
      pval_nporcapita= corfun(pibpercapita, ntotal)$p.value,
      rho.est_nporcapita = corfun(pibpercapita, ntotal)$estimate)
  
  ##ADIÇÃO DOS DADOS DE REGIÃO NOVAMENTE
  pchave <- ungroup(p) %>% select(NM_MESO, NM_REGIAO.y) 
  pchave <- unique(pchave)
  p_meso <- left_join(p_meso, pchave, by="NM_MESO")
  rm(list=ls()[! ls() %in% c("p","p_meso","corfun")])
}




###CORRELAÇÃO DAS MOVIMENTAÇÕES POR NÚMERO DE OPERAÇÕES E PESO POR CADA MERCADORIA-------------------------
{
  ##primeiro vou renomear todas as colunas de mercadorias para ficar mais fácil de chamar elas durante o script
  p_mercadoria <- p %>% transmute(ano=ano,pib, ntotal, pesototal, peso_maquinario=`peso_MAQUINÁRIO E MATERIAL INDUSTRIAL, MATERIAL DE TRANSPORTE E DERIVADOS`,
                                  n_maquinario=`n_MAQUINÁRIO E MATERIAL INDUSTRIAL, MATERIAL DE TRANSPORTE E DERIVADOS`,
                                  peso_outros=peso_OUTROS,
                                  n_outros=n_OUTROS,
                                  peso_commodities=`peso_OUTROS COMMODITIES E DERIVADOS`,
                                  n_commodities=`n_OUTROS COMMODITIES E DERIVADOS`,
                                  peso_animal=`peso_PRODUTOS DO REINO ANIMAL`,
                                  n_animal=`n_PRODUTOS DO REINO ANIMAL`,
                                  peso_vegetal=`peso_PRODUTOS DO VEGETAL`,
                                  n_vegetal=`n_PRODUTOS DO VEGETAL`,
                                  peso_combustivel =`peso_COMBUSTIVEIS MINERAIS, DERIVADOS DO PETROLEO E OUTROS DERIVADOS DO PETROLEO`,
                                  n_combustivel= `n_COMBUSTIVEIS MINERAIS, DERIVADOS DO PETROLEO E OUTROS DERIVADOS DO PETROLEO`,
                                  peso_container = peso_CONTAINERES,
                                  n_container=n_CONTAINERES)
  p_mercadoria[p_mercadoria==0] <- NA
}
###criação da tabela de correlação das mercadorias com o pib
{
  p_mercadoria$ano <- 0
  p_mercadoria <- ungroup(p_mercadoria)
  p_mercadoria <- ddply(p_mercadoria, .(ano), summarise,
                        pval_peso_combustivel = corfun(pib,peso_combustivel)$p.value,
                        rho.est_peso_combustivel = corfun(pib,peso_combustivel)$estimate,
                        pval_n_combustivel = corfun(pib, n_combustivel)$p.value,
                        rho.est_n_combustivel = corfun(pib, n_combustivel)$estimate ,
                        
                        pval_peso_maquinario = corfun(pib,peso_maquinario)$p.value,
                        rho.est_peso_maquinario = corfun(pib,peso_maquinario)$estimate,
                        pval_n_maquinario = corfun(pib, n_maquinario)$p.value,
                        rho.est_n_maquinario = corfun(pib, n_maquinario)$estimate,
                        
                        pval_peso_outros = corfun(pib,peso_outros)$p.value,
                        rho.est_peso_outros = corfun(pib,peso_outros)$estimate,
                        pval_n_outros = corfun(pib, n_outros)$p.value,
                        rho.est_n_outros = corfun(pib, n_outros)$estimate,
                        
                        pval_peso_commodities = corfun(pib,peso_commodities)$p.value,
                        rho.est_peso_commodities = corfun(pib,peso_commodities)$estimate,
                        pval_n_commodities = corfun(pib, n_commodities)$p.value,
                        rho.est_n_commodities = corfun(pib, n_commodities)$estimate,
                        
                        pval_peso_animal = corfun(pib,peso_animal)$p.value,
                        rho.est_peso_animal = corfun(pib,peso_animal)$estimate,
                        pval_n_animal = corfun(pib, n_animal)$p.value,
                        rho.est_n_animal = corfun(pib, n_animal)$estimate,
                        
                        pval_peso_vegetal = corfun(pib,peso_vegetal)$p.value,
                        rho.est_peso_vegetal = corfun(pib,peso_vegetal)$estimate,
                        pval_n_vegetal = corfun(pib, n_vegetal)$p.value,
                        rho.est_n_vegetal = corfun(pib, n_vegetal)$estimate,
                        
                        pval_peso_container = corfun(pib,peso_container)$p.value,
                        rho.est_peso_container = corfun(pib,peso_container)$estimate,
                        pval_n_container = corfun(pib, n_container)$p.value,
                        rho.est_n_container = corfun(pib, n_container)$estimate,
                        
                        pval_pesototal = corfun(pib,pesototal)$p.value,
                        rho.est_pesototal = corfun(pib,pesototal)$estimate,
                        pval_ntotal = corfun(pib, ntotal)$p.value,
                        rho.est_ntotal = corfun(pib, ntotal)$estimate)
  
}
###save(p_meso, p_mercadoria, p, file='p.rda') DESATIVAR COMENTÁRIO CASO QUEIRA SALVAR
