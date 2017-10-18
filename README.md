# MyFitnessPal

Eu me achava gordo e decidi começar uma dieta em 2016. Como anotei meu consumo calórico e meu peso diariamente a partir de 17 de fevereiro, eu disponho de uma quantidade de dados razoável para análise.

O arquivo `codigo.r` possui a rotina que utilizei para gerar os gráficos do meu desempenho, bem como o modelo estatístico que ajustei a estes dados.

O arquivo `data.csv` possui meus dados de peso e consumo calórico, sendo que estes estão desatualizados.

O arquivo `grafico01.pdf` tem um exemplo de gráfico que pode ser obtido com meu código. Note que ele possui duas tendências de evolução do peso (Curto e Longo Prazos) e o peso real.

Também é possível gerar os boxplots mensais do peso. O resultado pode ser visto em `grafico02.pdf`.

Reportei estatísticas descritivas a respeito da evolução do meu peso nestes meses.

Por fim, ajustei um modelo de previsão do peso futuro utilizando séries temporais. Este gráficos ainda não foi implementado utilizando o pacote `ggplot2`.

Utilize o tutorial disponível em https://lanroth.com/exportMyFitnessPal.html para expertar seus próprios resultados, caso também utilize o MyFitnessPal.

