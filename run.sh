./graphgen d.txt
for i in $(ls *.dot); do dot -Tpng $i > $(basename $i .dot).png; done
