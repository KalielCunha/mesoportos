###ROTINA PARA MANIPULAÇÃO DAS TABELAS BASE, PRETENDO A PARTIR DISSO GERAR UM DATATABLE COM OS DADOS DE MOVIMENTAÇÕES PORTUÁRIAS
###E OS DADOS DE PIB, A PARTIR DO DATAFRAME p
####CARREGANDO E INSTALANDO PACOTES--------
{
  packages <-
    c('ggplot2', 'readxl', 'dplyr', 'readr', 'data.table', 'esquisse',
      'sf',
      'tidyr','car')
  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)}})  
}


# 1. ROTINA PARA LEITURA E PROCESSAMENTO DE DADOS RELATIVOS A MOVIMENTAÇÕES PORTUÁRIAS DA ANTAQ----------
{
  ###Rotina para leitura e processamento dos dados relativos a movimentação portuária(Disponível na base de dados 
  ###da ANTAq). A partir destes dados gerou-se a tabela p: uma tabela com todas as movimentações portuárias anuais agregadas por porto,
  ###tipo de mercadoria, e tipo de navegação.
  
  
  # 1.1 CARREGANDO A LISTA DAS TABELAS-----------
  {
    files <- list.files(
      'input_data/brutos',
      pattern = "Carga.txt",
      full.names = T,
      recursive = T
    )
    atracacaofiles <- list.files(
      'input_data/brutos',
      pattern = "Atracacao.txt",
      full.names = T,
      recursive = T
    )
    
    
    
    mercadoriamodificada <- read_delim("input_data/brutos/mercadoriamodificada.csv",
                                       "\t",
                                       escape_double = FALSE,
                                       trim_ws = TRUE)
    
    
  }
  
  # 1.2 FAZENDO UM LOOP PARA ABRIR TODOS OS ARQUIVOS REFERENTES AOS ANOS PARA CARGA E ATRACAÇÃO-----------
  i = 1
  p <- list()
  n <- list()
  for (i in 1:length(files)) {
    df <- read.table(files[i],
                     sep = ';',
                     header = T,
                     dec = ',')
    df <- df %>% transmute(
      idcarga = IDCarga,
      idatracacao = IDAtracacao,
      cdmercadoria = CDMercadoria,
      TipoNavegacao = Tipo.Navegação,
      peso = VLPesoCargaBruta,
    ) 
    
    p[[i]] <- df
  }
  
  for (i in 1:length(atracacaofiles)) {
    at <- read.table(
      atracacaofiles[i],
      header = T ,
      dec = ',',
      sep = ";",
      fill = T
    )
    at <- at %>% transmute(
      idatracacao = IDAtracacao,
      complexoportuario = `Complexo.Portuário`,
      ano = Ano,
      CDTUP = CDTUP
    )
    n[[i]] <- at
  }
  {
    ##transformando as listas em dataframes----------
    p <- do.call(rbind, p)
    n <- do.call(rbind, n)
  }
  
  ##Surge a tabela principal "p" com junção de "m" e "n"
  
  
  # 1.3 LEFTJOIN E FILTRAGEM,  E AGREGAÇÃO DO DATAFRAME -----------
  p <- left_join(p , n, by = c("idatracacao" = "idatracacao"))
  p <-
    left_join(p,
              mercadoriamodificada,
              by = c("cdmercadoria" = "cdmercadoria"))
  
  p<- p %>% select(TipoNavegacao, CDTUP, complexoportuario, ano, nomencl_simpl_mercadoria, peso) %>%
    group_by(TipoNavegacao, CDTUP, complexoportuario, ano, nomencl_simpl_mercadoria) %>% summarise(peso=sum(peso), n=n())
  
  
  
  
  # 1.4 CORREÇÃO DO NOME DAS LINHAS E REMOÇÃO DE ACENTOS------
  p$TipoNavegacao <-
    recode_factor(
      as.factor(p$TipoNavegacao),
      `Apoio Marítimo` = 'Apoio Maritimo',
      `Apoio Portuário` = 'Apoio Portuario',
      `Não Indentificado` = 'Nao Identificado'
    )
  
  
  
  
  p <- p %>%
    mutate(mercadoria = nomencl_simpl_mercadoria) %>% select(-nomencl_simpl_mercadoria)
  p$mercadoria <-
    as.factor(p$mercadoria)
  levels(p$mercadoria) <-
    c(
      'ANIMAIS VIVOS E PRODUTOS DO REINO ANIMAL',
      'BEBIDAS, LIQUIDOS ALCOOLICOS E VINAGRES',
      'COMBUSTIVEIS MINERAIS, DERIVADOS DO PETROLEO E OUTROS DERIVADOS DO PETROLEO',
      'CONTAINERES',
      'GORDURAS E OLEOS ANIMAIS OU VEGETAIS',
      'MAQUINAS E APARELHOS, MATERIAL ELETRICO, E SUAS PARTES',
      'MADEIRA E SUAS OBRAS',
      'MATERIAIS TEXTEIS, PELES E COURO',
      'MATERIAIS TEXTEIS, PELES E COURO E COURO',
      'MATERIAL DE TRANSPORTE E DERIVADOS',
      'OBRAS DE PEDRA, GESSO, CIMENTO, AMIANTO, MICA OU DE MATERIAS SEMELHANTES; PRODUTOS CERAMICOS; VIDRO E SUAS OBRAS',
      'PLASTICO E SUAS OBRAS; BORRACHA E SUAS OBRAS',
      'PRODUTOS DAS INDUSTRIAS QUIMICAS OU DAS INDUSTRIAS CONEXAS',
      'PRODUTOS DIVERSOS'
      ,
      'PRODUTOS DO REINO VEGETAL',
      'PRODUTOS MINERAIS, METAIS RAROS, METAIS COMUNS E SUAS OBRAS',
      'TRANSACOES ESPECIAIS'
    )
  
  # 1.5 AGREGAÇÃO DAS MERCADORIAS-------------------------------------
  
  
  levels(p$mercadoria)[levels(p$mercadoria) == "PLASTICO E SUAS OBRAS; BORRACHA E SUAS OBRAS"] <-
    "plastico"
  
  p <-
    mutate(
      p,
      mercadoria = recode(
        mercadoria,
        "c('GORDURAS E OLEOS ANIMAIS OU VEGETAIS','ANIMAIS VIVOS E PRODUTOS DO REINO ANIMAL',
'MATERIAIS TEXTEIS, PELES E COURO', 'MATERIAIS TEXTEIS, PELES E COURO E COURO')='PRODUTOS DO REINO ANIMAL'"
      )
    )
  p <-
    mutate(
      p,
      mercadoria = recode(
        mercadoria,
        "c('PRODUTOS DO REINO VEGETAL','BEBIDAS, LIQUIDOS ALCOOLICOS E VINAGRES')='PRODUTOS DO VEGETAL'"
      )
    )
  p <-
    mutate(
      p,
      mercadoria = recode(
        mercadoria,
        "c('MATERIAL DE TRANSPORTE E DERIVADOS','MAQUINAS E APARELHOS, MATERIAL ELETRICO, E SUAS PARTES')='MAQUINÁRIO E MATERIAL INDUSTRIAL, MATERIAL DE TRANSPORTE E DERIVADOS'"
      )
    )
  p <- p%>%
    mutate(
      mercadoria = recode(
        mercadoria,
        "c('PRODUTOS DIVERSOS','TRANSACOES ESPECIAIS', NA)='OUTROS'"
      )
    )
  p <-
    mutate(
      p,
      mercadoria = recode(
        mercadoria,
        "c('MADEIRA E SUAS OBRAS', 'PRODUTOS MINERAIS, METAIS RAROS, METAIS COMUNS E SUAS OBRAS','plastico','PRODUTOS DAS INDUSTRIAS QUIMICAS OU DAS INDUSTRIAS CONEXAS')='OUTROS COMMODITIES E DERIVADOS'"
      )
    )
  
  p$mercadoria <-
    recode_factor(
      p$mercadoria,
      `OBRAS DE PEDRA, GESSO, CIMENTO, AMIANTO, MICA OU DE MATERIAS SEMELHANTES; PRODUTOS CERAMICOS; VIDRO E SUAS OBRAS` =
        'OUTROS COMMODITIES E DERIVADOS'
    )
  
  
} 
# 1.6 REMOÇÃO DAS NAs E DOS TIPOS DE NAVEGAÇÃO QUE NÃO SERÃO UTILIZADOS
p <- p %>%
  filter(!is.na(peso))
p <- p[!p$TipoNavegacao == "Apoio Maritimo", ]
p <- p[!p$TipoNavegacao == "Interior", ]
p <- p[!p$TipoNavegacao == "Nao Identificado", ]
p <- p[!p$TipoNavegacao == "Apoio Portuario", ]
#1.7 ADIÇÃO DE UM PIVOT_WIDER PARA MELHOR ANALISE DAS MERCADORIAS 
p <- p %>%  pivot_wider(names_from="mercadoria", values_from=c(n,peso), values_fn=sum)
p[, 5:18] <- p[, 5:18] %>% mutate_all(funs(ifelse(is.na(.), 0, .)))
p <-  p[!is.na(p$ano),]
p <- ungroup(p)
p <- p %>% select(-TipoNavegacao) %>% group_by(CDTUP, complexoportuario, ano) %>% summarise_all(sum)


portos_geom <- st_read('input_data/brutos/porto/Portos.shp')
portos_geom <- portos_geom %>% select(CODPORTUAR, geometry)
##1.8ADIÇÃO DE DADOS DE LOCALIZAÇÃO(GEOMETRIA)A TABELA DE PORTOS, PARA CONSEGUIR UNIR AS MESORREGIÕES

p<- left_join(p, portos_geom ,by=c("CDTUP"="CODPORTUAR"))

#save(p, file = "tabelaP.rda") desativar o comentário caso deseje salvar
rm(list=ls()[! ls() %in% c("packages","package.check","p")])






##2. INÍCIO DO PROCESSAMENTO DOS DADOS DE MESORREGIÃO E JUNÇÃO DOS DADOS PORTUÁRIOS AS MESORREGIÕES----------------
##arquivo com a geometria das mesorregiões
mesopib <- st_read('input_data/brutos/MESORREGIOES/BR_Mesorregioes_2021.shp')


##base de dados das mesorregiões
chave <- read_excel("input_data/brutos/pib/PIB dos Municípios - base de dados 2010-2019 - Copia.xls")




# 2.1 MANIPULAÇÃO INICIAL DOS DADOS
chave <- chave %>% transmute(NM_MESO= `Nome da Mesorregião`, NM_REGIAO=`Nome da Grande Região`) 
chave <- unique(chave)
##seleção de apenas os nomes das mesorregiões da tabela
chave <- left_join(mesopib, chave, by ="NM_MESO") 
chave <- st_as_sf(chave)
##adição de geometria as mesorregiões
p <- st_as_sf(p)
p<- st_join(p, chave, join = st_intersects)
#união das mesorregiões aos dados portuários por meio de intersecção, agora todo porto tem a sua respectia mesorregião
p <-st_drop_geometry(p)

p <- p %>% filter(complexoportuario != 'Não Classificado')
rm(list=ls()[! ls() %in% c("packages","package.check","p", "mesopib","chave")])
#porém alguns portos não possuiam uma intersecção(o ponto que indica o porto estava fora do polígono de mesorregião), então foi 
##preciso completar manualmente
{
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "Vitória", "Central Espírito-santense", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "Porto Alegre", "Metropolitana de Porto Alegre", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "Maceió", "Leste Alagoano", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "Manaus", "Centro Amazonense", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "Ilhéus", "Sul Baiano", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "Itaqui", "Centro Oriental Rio-grandense", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "Natal", "Leste Potiguar", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "Suape - Recife", "Metropolitana de Recife", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "Paranaguá - Antonina", "Metropolitana de Curitiba", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "Rio de Janeiro -  Niterói", "Metropolitana do Rio de Janeiro", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "Angra dos Reis", "Sul Fluminense", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "São João da Barra", "Norte Fluminense", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "Itajaí", "Vale do Itajaí", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "Aracajú", "Leste Sergipano", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "Imbituba", "Sul Catarinense", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "Itaguaí", "Metropolitana do Rio de Janeiro", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "Itaituba", "Sudoeste Paraense", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "Pecém - Fortaleza", "Metropolitana de Fortaleza", NM_MESO))
  p <- p %>% mutate(NM_MESO = ifelse(complexoportuario == "São Sebastião", "Vale do Paraíba Paulista", NM_MESO))
  p <- p %>% select(-CDTUP, -CD_MESO, -SIGLA, -complexoportuario, -NM_REGIAO) %>% group_by(ano, NM_MESO) %>% summarise_all(sum)
  ##foi preciso realizar a remoção da coluna que indica a grande região, pois com ela não seria possivel realizar o agrupamento dos dados
  ##essa coluna será adicionada novamente depois
}
# 2. ROTINA PARA DADOS LEITURA E PROCESSAMENTO DE PIB E JOIN DAS DUAS TABELAS

# abrindo a base de dados dos pibs novamente, pois anteriormente foi aberto apenas para realizar
# a intersecção das mesorregiões

#manipulação inicial dos dados de pib
pib <- read_excel("input_data/brutos/pib/PIB dos Municípios - base de dados 2010-2019 - Copia.xls")
# 2.2- SELEÇÃO, MANIPULAÇÃO DOS DADOS E RENOMEAÇÃO DAS COLUNAS
pib <- pib %>% select(Ano, `Nome da Grande Região`, `Nome da Mesorregião`,33, 34, 35, 36, 39, 40)
pib <- pib%>% 
  group_by(Ano,`Nome da Mesorregião`,`Nome da Grande Região`) %>% summarise_all(sum) %>% dplyr::rename(ano=Ano, "NM_MESO"=2,"NM_REGIAO"=3,"agropecuaria"= 4,"industria"=5,"servicos1"=6,"servicos2"=7,"pib"=8,"pibpercapita"=9)
p <- left_join(p, pib, by =c("NM_MESO","ano"))
#união dos dados de pib e da tabela com as mesorregiões com os dados portuários
##remoção de valores duplicados e NAs

p <- p %>% filter(NM_MESO != "Centro Oriental Rio-grandense")
p <- p %>% filter(NM_MESO != "Piracicaba")
p <- p %>% filter(NM_MESO != "Madeira-Guaporé")
p <- p %>% filter(NM_MESO != "Centro Amazonense")
p <- p %>% filter(NM_REGIAO != "Centro-oeste")
p <- p %>% filter(NM_MESO != "Baixo Amazonas")
p <- p %>% filter(NM_MESO != "Vale do Juruá")
p <- p %>% filter(NM_MESO != "Sudoeste Paraense")
p <- p %>% filter(NM_MESO != "Bauru")
p <- p %>% filter(NM_MESO != "Oeste Paranaense")
p <- p %>% filter(NM_MESO != "Pantanais Sul Mato-grossense")
p <- st_drop_geometry(p)
p <- unique(p)
p <- left_join(p, chave, by="NM_MESO")
p <- p %>% select(-SIGLA, -NM_REGIAO.x, -CD_MESO,-geometry)
p$pesototal <- rowSums(p[, 10:16])
p$ntotal <- rowSums(p[, 3:9])
###save(p, file='p.rda') DESATIVAR O COMENTÁRIO CASO QUEIRA SALVAR
rm(list=ls()[! ls() %in% c("packages","package.check","p")])
##Cria-se assim a tabela final p onde há os dados de todas as movimentações portuárias 
##E PIBs das Mesorregiões de 2010 a 2019

