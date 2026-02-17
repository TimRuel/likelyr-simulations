# ===============================
# likelyr-simulations Makefile
# ===============================

ifeq ($(OS),Windows_NT)
  SHELL := C:/rtools45/usr/bin/bash.exe
  .SHELLFLAGS := -lc
endif

.EXPORT_ALL_VARIABLES:

# -------------------------------------------------
# Paths
# -------------------------------------------------
ROOT := $(shell pwd)
CONFIG_DIR := config
EXPERIMENTS_DIR := experiments
SCRIPTS_DIR := scripts
JOBS_DIR := jobs

# -------------------------------------------------
# Phony targets
# -------------------------------------------------
.PHONY: help gen setup submit experiment analyze status dry-run clean test-iter

# -------------------------------------------------
# Help
# -------------------------------------------------
help:
	@echo ""
	@echo "Experiment workflow (config-driven):"
	@echo "  make experiment EXP_CONFIG=<path/to/experiment.yml>"
	@echo ""
	@echo "Individual steps:"
	@echo "  make gen        EXP_CONFIG=..."
	@echo "  make setup      EXP_CONFIG=..."
	@echo "  make submit     EXP_CONFIG=..."
	@echo "  make analyze    EXP_CONFIG=..."
	@echo ""
	@echo "Local testing:"
	@echo "  make test-iter  SIM_CONFIG=... [ITER=1]"
	@echo ""
	@echo "Utilities:"
	@echo "  make dry-run    EXP_CONFIG=..."
	@echo "  make status"
	@echo "  make clean      (refuses)"
	@echo ""

# -------------------------------------------------
# Expand design → simulation configs
# -------------------------------------------------
gen:
ifndef EXP_CONFIG
	$(error EXP_CONFIG must be set, e.g. EXP_CONFIG=config/multinom/baseline_logit/experiment.yml)
endif
	@echo "▶ Expanding experiment design"
	bash $(JOBS_DIR)/expand_design.sh $(EXP_CONFIG)

# -------------------------------------------------
# Materialize experiment + simulations
# -------------------------------------------------
setup: gen
	@echo "▶ Initializing experiment and simulations"
	bash $(JOBS_DIR)/init_exp.sh $(EXP_CONFIG)

# -------------------------------------------------
# Submit simulation jobs to SLURM
# -------------------------------------------------
submit:
ifndef EXP_CONFIG
	$(error EXP_CONFIG must be set, e.g. EXP_CONFIG=config/multinom/baseline_logit/experiment.yml)
endif
	@echo "▶ Submitting experiment to Slurm"
	@EXP_CFG_DIR=$(patsubst %/,%,$(dir $(EXP_CONFIG))); \
	EXP_REL=$(patsubst $(CONFIG_DIR)/%,%,$$EXP_CFG_DIR); \
	EXP_RUN_DIR=$(EXPERIMENTS_DIR)/$$EXP_REL; \
	bash $(JOBS_DIR)/submit_exp.sh $$EXP_RUN_DIR/experiment.yml

# -------------------------------------------------
# Full experiment
# -------------------------------------------------
experiment: setup submit
	@echo "✔ Experiment launched from $(EXP_CONFIG)"

# -------------------------------------------------
# Analyze experiment
# -------------------------------------------------
analyze:
ifndef EXP_CONFIG
	$(error EXP_CONFIG must be set, e.g. EXP_CONFIG=config/multinom/baseline_logit/experiment.yml)
endif
	@EXP_CFG_DIR=$(patsubst %/,%,$(dir $(EXP_CONFIG))); \
	EXP_REL=$(patsubst $(CONFIG_DIR)/%,%,$$EXP_CFG_DIR); \
	EXP_RUN_DIR=$(EXPERIMENTS_DIR)/$$EXP_REL; \
	echo "▶ Analyzing experiment"; \
	test -f $$EXP_RUN_DIR/experiment.yml || \
	  (echo "❌ Missing experiment snapshot. Run make setup first." && exit 1); \
	module purge && module load R/4.5.1 && \
	Rscript $(SCRIPTS_DIR)/analyze_sims.R $$EXP_RUN_DIR/experiment.yml

# -------------------------------------------------
# Test single iteration locally (no Slurm)
# -------------------------------------------------
test-iter:
ifndef SIM_CONFIG
	$(error SIM_CONFIG must be set, e.g. SIM_CONFIG=experiments/<exp>/<sim>/simulation.yml)
endif
	@echo "▶ Running local test iteration"
	@ITER_INDEX=$(or $(ITER),1); \
	echo "  • simulation: $(SIM_CONFIG)"; \
	echo "  • iteration:  $$ITER_INDEX"; \
	bash $(JOBS_DIR)/test_iter.sh $(SIM_CONFIG) $$ITER_INDEX

# -------------------------------------------------
# Dry run (no side effects, predictive)
# -------------------------------------------------
dry-run:
ifndef EXP_CONFIG
	$(error EXP_CONFIG must be set, e.g. EXP_CONFIG=config/multinom/baseline_logit/experiment.yml)
endif
	@EXP_CFG_DIR=$(patsubst %/,%,$(dir $(EXP_CONFIG))); \
	EXP_REL=$(patsubst $(CONFIG_DIR)/%,%,$$EXP_CFG_DIR); \
	EXP_RUN_DIR=$(EXPERIMENTS_DIR)/$$EXP_REL; \
	echo "▶ DRY RUN"; \
	echo ""; \
	echo "Experiment config:"; \
	echo "  $(EXP_CONFIG)"; \
	echo ""; \
	echo "Would expand design:"; \
	echo "  bash $(JOBS_DIR)/expand_design.sh $(EXP_CONFIG)"; \
	echo ""; \
	echo "Would initialize experiment:"; \
	echo "  bash $(JOBS_DIR)/init_exp.sh $(EXP_CONFIG)"; \
	echo ""; \
	echo "Would submit Slurm jobs:"; \
	echo "  bash $(JOBS_DIR)/submit_exp.sh $$EXP_RUN_DIR/experiment.yml"; \
	echo ""; \
	echo "Local iteration test:"; \
	echo "  make test-iter SIM_CONFIG=experiments/<exp>/<sim>/simulation.yml [ITER=1]"; \
	echo ""; \
	echo "✔ Dry run complete"

# -------------------------------------------------
# Slurm queue
# -------------------------------------------------
status:
	squeue -u $$USER

# -------------------------------------------------
# Cleanup (blocked)
# -------------------------------------------------
clean:
	@echo "❌ Refusing to delete experiments automatically."
	@false
