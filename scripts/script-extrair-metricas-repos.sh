#!/bin/bash

BASE_PATH=/home/vinicius/dev-projects/java/unisinos/tcc/repositorios-processamento/repos-originais
FILE_COUNT=$BASE_PATH/count-tot-lines.txt
#vai para pasta base com repositorios
cd $BASE_PATH

#Print quantidades de commits de um repo na branch principal
echo '********************************************************************************'
echo ' Quantidades de commits de um repo na branch principal'
echo '********************************************************************************'
find . -maxdepth 1 -type d \( ! -name . \) -exec bash -c "cd '{}' && pwd && git rev-list --count HEAD" \;

echo '********************************************************************************'
echo ' Quantidade de contribuidores de um repo - qtde de usuarios que fizeram commit'
echo '********************************************************************************'
#Print a quantidade de contribuidores de um repo - qtde de usuarios que fizeram commit
find . -maxdepth 1 -type d \( ! -name . \) -exec bash -c "cd '{}' && pwd && git shortlog -sne HEAD | wc -l" \;

echo '********************************************************************************'
echo ' Quantidade de linhas de codigo fonte '
echo '********************************************************************************'
#Conta quantidade de linhas de codigo fonte
#sudo apt install cloc
#opcao 1 - dentro de cada repo executar (saida completa):
#cloc $(git ls-files)
#cloc --git HEAD
#opcao 2 - dentro de cada repo executar (print somente o total de linhas):
#cloc $(git ls-files) | grep SUM: | awk '{print $5}'
#cloc --git HEAD | grep SUM: | awk '{print $5}'
find . -maxdepth 1 -type d \( ! -name . \) -exec bash -c "cd '{}' && pwd && cloc --git HEAD | grep SUM: | awk '{print \$5}'" \;
