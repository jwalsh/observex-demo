# Makefile for generating diagrams, utility files, running simulator, and Jaeger mock server

# Variables
SHELL := /bin/bash
MMDC := mmdc  # Assumes mermaid-cli is installed and 'mmdc' is in PATH
DOCS_DIR := internal-developer-platform/docs
OUTPUT_DIR := output
GUILE := guile
VENV := venv
PYTHON := $(VENV)/bin/python
PIP := $(VENV)/bin/pip

# Find all .mmd files in the docs directory
MMD_FILES := $(wildcard $(DOCS_DIR)/*.mmd)
PNG_FILES := $(patsubst $(DOCS_DIR)/%.mmd,$(OUTPUT_DIR)/%.png,$(MMD_FILES))

# Default target
all: files.txt tree.txt simulator.txt $(PNG_FILES) 

# Virtual environment
$(VENV)/bin/activate: requirements.txt
	python3 -m venv $(VENV)
	$(PIP) install -r requirements.txt

# Jaeger Mock (Docker version)
jaeger-mock:
	docker run -d -p 14268:14268 -p 16686:16686 jaegertracing/all-in-one:latest

# Jaeger Debugging (Python mock server)
jaeger-server-mock: $(VENV)/bin/activate
	$(PYTHON) jaeger_server_mock.py

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

# Clean generated files and virtual environment
clean:
	rm -rf $(OUTPUT_DIR)
	rm -f files.txt tree.txt simulator.txt
	rm -rf $(VENV)
	find . -type f -name '*.pyc' -delete
	find . -type d -name '__pycache__' -delete

.PHONY: all clean run-simulator jaeger-mock jaeger-server-mock
