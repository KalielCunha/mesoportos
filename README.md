Repositório com os scripts utilizados na realização da pesquisa: movimentação de cargas dos portos brasileiros e seu impacto no pib regional no período de 2010 a 2019, todos os scripts foram feitos dentro do r. Segue uma breve explicação de cada arquivo do repositório:

Script processamento_dados.R 
Este script apresenta o início do trabalho, onde foi utilizado os dados base de movimentações portuárias da antaq(disponível em: http://ea.Antaq.Gov.Br/qvajaxzfc/opendoc.Htm?Document=painel%5cantaq%20-%20anu%c3%a1rio%202014%20-%20v0.9.3.Qvw&lang=pt-br&host=qvs%40graneleiro&anonymous=true) na aba base de dados. E a base do pib dos municípios brasileiros(disponível em: https://www.Ibge.Gov.Br/estatisticas/economicas/contas-nacionais/9088-produto-interno-bruto-dos-municipios.Html?=&t=downloads) a partir dessas duas base de dados(pib e movimentações portuárias) foi realizado a filtragem das colunas utilizadas no trabalho, a manipulação dos dados, e por fim uma união das informações com base no comando st_intersects do pacote sf. Onde as informações foram agrupadas bom base na localização geográfica, uma porcentagem pequena destes dados não foi agrupada e teve que ser agrupada manualmente. Após isso, gerou-se a tabela p. Uma tabela com todos os dados a serem manipulados posteriormente. A base de dados utilizada está disponível no arquivo "drive.Txt", não foi possível colocar neste repositório por conta dos arquivos terem aproximadamente 3.30GB.

Script correlacao.R 
Dentro desse script foi dado o início aos processos de correlação, onde para isso realizou-se dois agrupamentos diferentes, um agrupamento por mesorregião; e um agrupamento por mercadoria movimentada. A partir disso foi realizada a correlação em ambos dataframes por meio de dois parâmetros: n° de operações e peso movimentado. Gerando assim dois dataframes: p_meso -> um dataframe com as correlações de cada mesorregião porém sem especificação de mercadoria;e p_mercadoria -> um dataframe com as correlações de cada mercadoria, mas no recorte nacional. Não se pode realizar uma correlação de mercadorias no recorte de cada mesorregião por conta da baixa amostragem de dados.

Script figuras.R 
Esse script foi o responsável pela criação de figuras, além disso, nele foi realizado a remoção daqueles dados que apresentaram teste de hipótese(p)>0.05. Depois disso foi realizado a criação de figuras, que foram armazenadas na pasta figures, disponível no repositório.
