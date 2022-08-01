
# importanto a função pipe do pacote magritrr
'%>%' <- magrittr::'%>%'  

# Importando os Bases necessárias
dados1 <- data.table::fread('data-raw/Questão 2 - Base 1.txt',sep2 = " ")
dados2 <- data.table::fread('data-raw/Questão 2 - Base 2.txt',sep2 = " ")
dados3 <- data.table::fread('data-raw/Questão 2 - Base 3.txt',sep2 = " ")
dados4 <- data.table::fread('data-raw/Questão 2 - Base 4.txt',sep2 = " ", colClasses = list(integer = 1,Date = 2, integer = 3:12))
dados5 <- data.table::fread('data-raw/Questão 2 - Base 5.txt',sep2 = " ")


# dando uma olhada nos tipos de dados de cada base de dados
dados1 %>% 
  dplyr::glimpse()
dados2 %>% 
  dplyr::glimpse()
dados3 %>% 
  dplyr::glimpse()
dados4 %>% 
  dplyr::glimpse()
dados5 %>% 
  dplyr::glimpse()


# visualizando as prmeira linhas de cada base de dados
dados1 %>% 
  head()
dados2 %>% 
  head()
dados3 %>% 
  head()
dados4 %>% 
  head()
dados5 %>% 
  head()

# verificando a presença de dados faltantes nas bases

apply(dados1, 2, function(x){sum(is.na(x))}) 
apply(dados2, 2, function(x){sum(is.na(x))}) 
apply(dados3, 2, function(x){sum(is.na(x))}) 
apply(dados4, 2, function(x){sum(is.na(x))}) 
apply(dados5, 2, function(x){sum(is.na(x))}) 

# não foi encontrado dados faltantes nas bases.


# verificando se existe dados duplicadas nas bases de dados

dados1[dados1 %>% 
                 duplicated() == TRUE,]

#ID_CONTA 1385626

dados1 %>% 
  dplyr::filter(ID_CONTA == '1385626')

dados2[dados2 %>% 
         duplicated() == TRUE,]

#ID_CONTA 1385626

dados2 %>% 
  dplyr::filter(ID_CONTA == '1385626')


dados3[dados3 %>% 
         duplicated() == TRUE,]

dados3 %>% 
  dplyr::filter(ID_CONTA == '1385626')

#ID_CONTA 1385626

dados4[dados4 %>% 
         duplicated() == TRUE,]
#ID_CONTA 1385626

dados4 %>% 
  dplyr::filter(ID_CONTA == '1385626')

dados5[dados5 %>% 
         duplicated() == TRUE,]

#ID_CONTA 1385626

dados5 %>% 
  dplyr::filter(ID_CONTA == '1385626')

# Em todos os conjuntos de dados identificamos que temos observações duplicadas relacionadas
# ao ID_CONTA 1385626, são duas observações para este id em cada conjunto.

# removendo observações duplicadas

dados1 <- dados1 %>% 
  dplyr::distinct()

dados2 <- dados2 %>% 
  dplyr::distinct()

dados3 <- dados3 %>% 
  dplyr::distinct()

dados4 <- dados4 %>% 
  dplyr::distinct()

dados5 <- dados5 %>% 
  dplyr::distinct()

# criando a base de modelagem

base_modelagem <- dados5 %>% 
  dplyr::left_join(
    dados1,
    by = c("ID_CONTA","DT_ACORDO")
  ) %>% 
  dplyr::left_join(
    dados2,
    by = c("ID_CONTA","DT_ACORDO")
  ) %>% 
  dplyr::left_join(
    dados3,
    by = c("ID_CONTA","DT_ACORDO")
  ) %>% 
  dplyr::left_join(
    dados4 %>% 
      dplyr::mutate(DT_ACORDO = data.table::as.IDate(DT_ACORDO)),
    by = c("ID_CONTA","DT_ACORDO")
  )


# salvando a base análitica na pasta data
data.table::fwrite(base_modelagem, 'data/Base_modelagem.txt')