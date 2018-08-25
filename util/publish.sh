#!/bin/bash
# Convert Jupyter notebooks to HTML files and upload them to cloud storage.
# Notebooks: "main.ipynb", "main/*.ipynb".
# HTML files are saved to "www/".

mkdir -p www
rm -r www/*

sed 's/.ipynb/.html/' main.ipynb \
    | jupyter nbconvert --stdin --output-dir=www --output=main
for nb_file in $(ls main/*.ipynb); do
    name=$(basename -s .ipynb $nb_file)
    sed 's/.ipynb/.html/' $nb_file \
        | jupyter nbconvert --stdin --output-dir=www/main --output=$name
done

gsutil cp -r www/* gs://info-group-public/rurec/www/
