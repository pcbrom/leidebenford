##################################################################################
# LEI DE BENFORD APLICADA A DICIONARIOS
# AUTOR: PEDRO CARVALHO BROM
# EMAIL: SUPERMETRICA@GMAIL.COM
# FACEBOOK: www.facebook.com/pedraodeexatas
# CV LATTES: lattes.cnpq.br/0154064396756002
##################################################################################

rm(list=ls(all=T))

# definindo a pasta de trabalho
setwd("~/Dropbox/Trabalho e Estudo/SuperMetrica/Lei de Benford/Dicionario")

# ler o arquivo txt
local = "~/Dropbox/Trabalho e Estudo/SuperMetrica/Lei de Benford/Dicionario/Dicionário.txt"
dados = read.table(local, header = F, sep = "\t", blank.lines.skip = T, 
                   encoding = "UTF-8", fill = T, quote = NULL, skipNul = T, skip = 2452)

# remover acentuacao
dados = as.character(dados$V1)
rm_accent = function(x) iconv(x, to = "ASCII//TRANSLIT")
dados = rm_accent(dados)

# ajustando para caracter e colocando a linha final de interesse
head(dados)
tail(dados)
inicio.excluir = grep('zythogala', dados, fixed = T ) + 1
final.excluir = length(dados)
excluir = inicio.excluir:final.excluir
dados = dados[-excluir]

# identificar inicio de cada palavra
buscar = paste0('^', letters)

# contar quantas palavras iniciam com cada letra
freq = rep(1, length(buscar))
for (i in 1:length(buscar)) {
  freq[i] = length(dados[grep(buscar[i], dados)])
}

# ordenando decrescente
palavras = as.data.frame(freq)
row.names(palavras) = letters
sort.data.frame = function(x, decreasing = FALSE, by = 1, ... ){
  f = function(...) order(..., decreasing = decreasing)
  i = do.call(f, x[by])
  x[i, , drop = FALSE]
}
palavras = sort.data.frame(palavras, by = 'freq', decreasing = T)

# medidas resumo e plotagem do grafico de barras pra avaliar se a media e 
# maior do que a mediana
summary(palavras)
library(lattice)
barplot(t(palavras), main = 'Frequência de palavras iniciadas com\n as letras do alfabeto'
        , ylim = c(0, 25000))

# analise de dados pela Lei de Benford
library(benford.analysis)
bfd_pop = benford(palavras$freq, number.of.digits = 1)
bfd_pop
plot(bfd_pop)
