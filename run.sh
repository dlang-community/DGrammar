./graphgen d.txt
for i in $(ls *.dot); do dot -Tpng $i > $(basename $i .dot).png; done
cat begin.txt > out.html
for i in $(ls *.png); do echo "<img src=\"" $i "\"/><br/>" >> out.html; done
cat end.txt >> out.html
