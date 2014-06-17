all: inst

gh-pages:
	git subtree split --prefix website --branch gh-pages

inst: inst/NEWS.Rd

inst/NEWS.Rd: NEWS.md
	Rscript -e "tools:::news2Rd('$<', '$@')"
	sed -r -i 's/`([^`]+)`/\\code{\1}/g' $@

.FORCE:
