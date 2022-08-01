

SELECT f_vendas.ID_PESSOA, d_pessoa.NM_PESSOA,d_Tempo.DT_REF, f_Vendas.VL_VENDA
FROM f_Vendas
	JOIN d_Pessoa ON f_Vendas.ID_PESSOA = d_pessoa.ID_PESSOA
	JOIN d_Tempo ON f_Vendas.ID_TEMPO = d_Tempo.ID_TEMPO
WHERE NU_MES = 1
AND NU_ANO = 2020
AND DS_UF = CE