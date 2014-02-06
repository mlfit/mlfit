all:

install:
	crant -SMCit

test-%: install
	Rscript -e 'library(methods); library(testthat); library(MultiLevelIPF); test_file("tests/testthat/'"$@"'.R")'
