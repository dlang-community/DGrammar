LIBDPARSE_PATH=../dscanner/libdparse

dmd -D ${LIBDPARSE_PATH}/src/std/d/parser.d\
	-I${LIBDPARSE_PATH}/src\
	${LIBDPARSE_PATH}/macros.ddoc -c

cat begin.txt > grammar.html
xmllint --html --xpath "//pre[@class='grammar']" parser.html >> grammar.html
cat end.txt >> grammar.html
