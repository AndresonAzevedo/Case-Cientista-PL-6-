

'%>%' <- magrittr::'%>%' 

## 2.1 Qual o percentual de adesão mensal
## por faixa de atraso (Histórico) ?

dados <- data.table::fread('data-raw/Questão 2 - Base 1.txt',sep2 = " ")

dados %>% 
  dplyr::mutate(
    faixa_atraso = cut(NU_DIAS_ATRASO, 12)
  ) %>% 
  dplyr::mutate(mes_num = lubridate::month(DT_ACORDO), 
                MES = paste(format(DT_ACORDO,"%b"),format(DT_ACORDO,"%y"),sep="/")) %>% 
  dplyr::group_by(faixa_atraso, mes_num, MES) %>% 
  dplyr::summarise(
    n = length(RESPOSTA),
    adesao = sum(RESPOSTA, na.rm = TRUE),
    '%' = adesao/n*100) %>% 
  dplyr::arrange(mes_num) %>%
  dplyr::ungroup() %>% 
  dplyr::select(-c(n,adesao, mes_num)) %>% 
  tidyr::spread(key = "MES", value = "%") %>% 
  dplyr::select(faixa_atraso,`nov/18`, `mar/19`, `abr/19`, `jun/19`) %>% 
  knitr::kable(format = "html", digits = 2, align = "c") %>%
  kableExtra::kable_classic()

# link para leitura terminar esse tópico

https://efic.com.br/indicadores-de-cobranca/