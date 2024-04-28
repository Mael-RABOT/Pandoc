#!/bin/bash

tests=(
  "./mypandoc -i tests/example.xml -f json -o tests/exemple_test.json -e xml"
  "./mypandoc -i tests/example.json -f xml -o tests/exemple_test.xml -e json"
  "./mypandoc -i tests/example.json -f json -o tests/exemple_test.json -e json"
  "./mypandoc -i tests/example.xml -f xml -o tests/exemple_test.xml -e xml"
)

success_count=0
fail_count=0

green='\033[0;32m'
red='\033[0;31m'
nc='\033[0m'

echo "Execution des tests..."
echo

for i in "${tests[@]}"
do
  eval "$i"

  format=$(echo $i | awk '{for(i=1;i<=NF;i++) if($i=="-f") {print $(i+1); break}}')

  if [ "$format" == "json" ]; then
    python tests/clean_json.py tests/exemple_test.json
  elif [ "$format" == "xml" ]; then
    python tests/clean_xml.py tests/exemple_test.xml
  fi

  if diff -q tests/example."$format"  tests/exemple_test."$format" > /dev/null
  then
    echo -e "${green}Test réussi : $i${nc}"
    success_count=$((success_count+1))
  else
    echo -e "${red}Test échoué : $i${nc}"
    fail_count=$((fail_count+1))
  fi
  rm -f tests/exemple_test."$format"
done

echo
echo "Récapitulatif des tests :"
echo -e "Nombre de tests réussis : ${green}$success_count${nc}"
echo -e "Nombre de tests échoués : ${red}$fail_count${nc}"