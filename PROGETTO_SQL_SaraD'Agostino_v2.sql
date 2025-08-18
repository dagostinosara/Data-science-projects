-- overview delle tabelle iniziali
select * from banca.cliente;
select * from banca.conto where id_cliente = '11';
select * from banca.tipo_conto;
select * from banca.tipo_transazione;
select * from banca.transazioni;

-- creazione della tabella temporanea
CREATE TEMPORARY TABLE banca.aggregated_info_prj AS
	-- info base
select distinct cl.id_cliente, cl.nome, cl.cognome,
	-- età
round(datediff(CURRENT_DATE(), cl.data_nascita)/365.25) as eta,
	-- Numero totale di conti posseduti.
count(distinct cn.id_conto) as n_conti,
	-- Numero di transazioni in uscita ed entrata su tutti i conti.
count(case when tipt.segno = '-' then tipt.segno else null end)  n_trans_uscita,
count(case when tipt.segno = '+' then tipt.segno else null end)  n_trans_entrata,
	-- Importo totale transato in uscita ed entrata su tutti i conti.
sum(case when tipt.segno = '-' then tr.importo else null end)  tot_importo_trans_uscita,
sum(case when tipt.segno = '+' then tr.importo else null end)  tot_importo_trans_entrata,
	-- Numero di conti posseduti per tipologia (un indicatore per ogni tipo di conto).
count(distinct case when tipc.desc_tipo_conto = 'Conto Base' then cn.id_conto else null end) tot_conto_base,
count(distinct case when tipc.desc_tipo_conto = 'Conto Business' then cn.id_conto else null end) tot_conto_business,
count(distinct case when tipc.desc_tipo_conto = 'Conto Privati' then cn.id_conto else null end) tot_conto_priv,
count(distinct case when tipc.desc_tipo_conto = 'Conto Famiglie' then cn.id_conto else null end) tot_conto_fam,
	-- Numero di transazioni in uscita ed entrata per tipologia di conto (un indicatore per tipo di conto).
count(case when tipc.desc_tipo_conto = 'Conto Base' and tipt.segno = '-' then tr.id_tipo_trans else null end) n_trans_uscita_conto_base,
count(case when tipc.desc_tipo_conto = 'Conto Base' and tipt.segno = '+' then tr.id_tipo_trans else null end) n_trans_entrata_conto_base,
count(case when tipc.desc_tipo_conto = 'Conto Business' and tipt.segno = '-' then tr.id_tipo_trans else null end) n_trans_uscita_conto_business,
count(case when tipc.desc_tipo_conto = 'Conto Business' and tipt.segno = '+'  then tr.id_tipo_trans else null end) n_trans_entrata_conto_business,
count(case when tipc.desc_tipo_conto = 'Conto Privati' and tipt.segno = '-' then tr.id_tipo_trans else null end) n_trans_uscita_conto_priv,
count(case when tipc.desc_tipo_conto = 'Conto Privati' and tipt.segno = '+'  then tr.id_tipo_trans else null end) n_trans_entrata_conto_priv,
count(case when tipc.desc_tipo_conto = 'Conto Famiglie' and tipt.segno = '-'  then tr.id_tipo_trans else null end) n_trans_uscita_conto_fam,
count(case when tipc.desc_tipo_conto = 'Conto Famiglie'and tipt.segno = '+'  then tr.id_tipo_trans else null end) n_trans_entrata_conto_fam,
	-- Importo transato in uscita e in entrata per tipologia di conto (un indicatore per tipo di conto).
sum(case when tipc.desc_tipo_conto = 'Conto Base' and tipt.segno = '-' then tr.importo else 0 end) importo_uscita_base,
sum(case when tipc.desc_tipo_conto = 'Conto Business'  and tipt.segno = '-' then tr.importo else 0 end) importo_uscita_business,
sum(case when tipc.desc_tipo_conto = 'Conto Privati'  and tipt.segno = '-' then tr.importo else 0 end) importo_uscita_priv,
sum(case when tipc.desc_tipo_conto = 'Conto Famiglie'  and tipt.segno = '-' then tr.importo else 0 end) importo_uscita_fam,
sum(case when tipc.desc_tipo_conto = 'Conto Base' and tipt.segno = '+' then tr.importo else 0 end) importo_entrata_base,
sum(case when tipc.desc_tipo_conto = 'Conto Business'  and tipt.segno = '+' then tr.importo else 0 end) importo_entrata_business,
sum(case when tipc.desc_tipo_conto = 'Conto Privati'  and tipt.segno = '+' then tr.importo else 0 end) importo_entrata_priv,
sum(case when tipc.desc_tipo_conto = 'Conto Famiglie'  and tipt.segno = '+' then tr.importo else 0 end) importo_entrata_fam
from banca.cliente cl
left join banca.conto cn on cl.id_cliente = cn.id_cliente
left join banca.tipo_conto tipc on cn.id_tipo_conto = tipc.id_tipo_conto
left join banca.transazioni tr on cn.id_conto = tr.id_conto
left join banca.tipo_transazione tipt on tr.id_tipo_trans = tipt.id_tipo_transazione
group by 1,2,3,4;

-- visualizzazione del risultato
select * from banca.aggregated_info_prj;



