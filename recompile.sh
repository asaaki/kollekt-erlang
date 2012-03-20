#!/bin/bash
for src in `ls *.erl`; do
  erlc $src
done