

SELECT ID_PESSOA, COUNT(ID_VENDA) as QTD_COMPRAS 
FROM f_Vendas
	JOIN d_Tempo ON f_Vendas.ID_TEMPO = d_Tempo.ID_TEMPO
WHERE NU_MES = 3
and NU_ANO = 2020
GROUP BY 1