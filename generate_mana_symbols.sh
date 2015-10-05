#!/bin/sh

for px in 15 40; do
    mkdir -p static/img/mana/$px

    for i in manasymbols/*.svg; do
        inkscape -z -e static/img/mana/$px/$(basename $i .svg).png -w $px -h $px $i;
    done;
done
