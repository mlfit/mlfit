all:

install:
	crant -SMCi

test-%: install
	Rscript -e 'library(testthat); library(MultiLevelIPF); test_file("tests/testthat/'"$@"'.R")'
