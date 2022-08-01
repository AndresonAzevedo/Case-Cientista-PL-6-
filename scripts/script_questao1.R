

dados <- data.table::fread('data-raw/Questão 1 - Base.txt',sep2 = " ")

'%>%' <- magrittr::'%>%' 

# verificando os tipos das variáveis
dados %>% 
  tibble::as_tibble() %>% 
  dplyr::glimpse()

# verificando os dados faltante na base
apply(dados,2,function(x){sum(is.na(x))})

# não temos dados faltantes

# verificando o range de datas
min(dados$DT_VENCIMENTO)
max(dados$DT_VENCIMENTO)



# Q_1.1: percentual de faturas emitidas por mês no qual os clientes não pegaram
# a fatura anterior

dados %>% 
  dplyr::mutate(mes_num = lubridate::month(DT_VENCIMENTO), 
                MES = toupper(format(DT_VENCIMENTO,"%b"))) %>% 
  dplyr::group_by(mes_num, MES) %>% 
  dplyr::summarise(TT_FATURAS = length(ID_CONTA),
                   TT_ANT_NPAGAS = sum(ifelse(DS_ROLAGEM == 'FX1',1,0)),
                   '%' = TT_ANT_NPAGAS/TT_FATURAS*100) %>% 
  dplyr::arrange(mes_num) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(c(MES,TT_FATURAS, TT_ANT_NPAGAS,'%')) %>% 
  knitr::kable(format = "html", digits = 2, align = "c") %>%
  kableExtra::kable_classic()

# Q_1.2: Tendo como referência todos os clientes que tiveram fatura 
# emitida no mês de setembro, gere uma base para esses clientes com
# os seguintes valores calculados:

## Coletando os ID_CONTA do mês de setembro
id_setembro <- dados %>% 
  dplyr::mutate(mes_num = lubridate::month(DT_VENCIMENTO), 
                MES = toupper(format(DT_VENCIMENTO,"%b"))) %>% 
  dplyr::filter(MES == 'SET') %>% 
  dplyr::select(ID_CONTA)

## Calculando as métricas para os clientes com fatura emitida no mês de setembro

novas_variaveis <- dados %>% 
  dplyr::filter(ID_CONTA %in% id_setembro$ID_CONTA
  ) %>% 
  dplyr::mutate(mes_num = lubridate::month(DT_VENCIMENTO), 
                MES = toupper(format(DT_VENCIMENTO,"%b"))) %>% 
  dplyr::filter(mes_num >= max(mes_num)-6 & mes_num < 9) %>% 
  dplyr::group_by(ID_CONTA) %>% 
  dplyr::summarise(QTD_FATURAS_ULT_6M = length(ID_CONTA),
                   VL_MEDIO_FATURA = mean(VL_FATURA),
                   QTD_FATURAS_ULT_6M_FX1 = sum(ifelse(DS_ROLAGEM == 'FX1',1,0))
  )


## coletando usuários que tiveram fatura emitida em setembro
dados_set <- dados %>% 
  dplyr::mutate(mes_num = lubridate::month(DT_VENCIMENTO), 
                MES = toupper(format(DT_VENCIMENTO,"%b"))) %>% 
  dplyr::filter(MES == 'SET')


dados_final <- dados_set %>% 
  dplyr::select(ID_CONTA,DS_ROLAGEM,DT_VENCIMENTO) %>% 
  dplyr::inner_join(
    novas_variaveis,
    by = "ID_CONTA"
  )

# salvando base de dados no pasta 'data'
data.table::fwrite(dados_final, file = 'data/base_usuarios_setembro.txt')

# apresentando as primeiras linhas da nova base

dados_final %>% 
  head() %>%
  knitr::kable(format = "html", digits = 2, align = "c") %>%
  kableExtra::kable_classic()

# Q.1.3: Utilizando como referência a base calculada na questão anterior, 
# identifiquei qual das 3 variáveis calculadas tem o maior potêncial de 
# preditivo em relação a variável DS_ROLAGEM do mês de setembro

# Calculando valor
dados_final %>% 
  dplyr::mutate(RESPOSTA = ifelse(DS_ROLAGEM == "FX0",0,1)) %>% 
  dplyr::select(-c(ID_CONTA,DT_VENCIMENTO,DS_ROLAGEM)) %>% 
  cor() %>% 
  knitr::kable(format = "html", digits = 2, align = "c") %>%
  kableExtra::kable_classic()
