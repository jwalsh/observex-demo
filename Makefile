# Makefile for generating diagrams, utility files, and running simulator

# Variables
SHELL := /bin/bash
MMDC := mmdc  # Assumes mermaid-cli is installed and 'mmdc' is in PATH
DOCS_DIR := internal-developer-platform/docs
OUTPUT_DIR := output
GUILE := guile

# Find all .mmd files in the docs directory
MMD_FILES := $(wildcard $(DOCS_DIR)/*.mmd)
PNG_FILES := $(patsubst $(DOCS_DIR)/%.mmd,$(OUTPUT_DIR)/%.png,$(MMD_FILES))

# Default target
all: files.txt tree.txt simulator.txt $(PNG_FILES) 

# Rule to convert .mmd files to .png
$(OUTPUT_DIR)/%.png: $(DOCS_DIR)/%.mmd | $(OUTPUT_DIR)
	$(MMDC) -i $< -o $@

# Create output directory if it doesn't exist
$(OUTPUT_DIR):
	mkdir -p $(OUTPUT_DIR)

# Generate files.txt
files.txt: show_text_files.sh
	./show_text_files.sh > $@

# Generate tree.txt
tree.txt:
	tree -L 3 > $@

# Generate simulator.txt
simulator.txt: observex-simulator.scm
	$(GUILE) --no-auto-compile observex-simulator.scm 2>&1 | tee $@

# Run simulator interactively
run-simulator:
	$(GUILE) --no-auto-compile -l .init.scm -l main.scm

# Clean generated files
clean:
	rm -rf $(OUTPUT_DIR)
	rm -f files.txt tree.txt simulator.txt

.PHONY: all clean run-simulator