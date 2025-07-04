FIG_DIRS := $(sort $(dir $(wildcard figures/*/)))
PDFS := $(addsuffix plot.pdf,$(FIG_DIRS))
DATAS := $(addsuffix data.csv,$(FIG_DIRS))

.PHONY: rebuild clean all

# Default target: build every figure
all: $(DATAS) $(PDFS)

# Build data.csv inside each figure directory
figures/%/data.csv: figures/%/generate_data.R
	@echo "📊  Generating $@"
	@cd $(@D) && Rscript --vanilla $(<F)

# Build plot.pdf inside each figure directory
figures/%/plot.pdf: figures/%/plot.R figures/%/data.csv
	@echo "🎨  Drawing $@"
	@cd $(@D) && Rscript --vanilla $(<F)

rebuild: clean all

clean:
	@echo "🧹  Cleaning generated data and figures"
	@rm -f $(DATAS) $(PDFS)
