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
ROOT            := $(shell pwd)
CONFIG_DIR      := config
EXPERIMENTS_DIR := experiments
SCRIPTS_DIR     := scripts
JOBS_DIR        := jobs

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
	@echo "  make experiment EXP_CONFIG=<path/to/exp_vX.yml>"
	@echo ""
	@echo "Individual steps:"
	@echo "  make gen        EXP_CONFIG=..."
	@echo "  make setup      EXP_CONFIG=..."
	@echo "  make submit     EXP_CONFIG=..."
	@echo "  make analyze    EXP_CONFIG=..."
	@echo ""
	@echo "Local testing:"
	@echo "  make test-iter  SIM_CONFIG=... [ITER=1]"
	@echo "  Note: make setup must be run first to build model specs."
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
	$(error EXP_CONFIG must be set, e.g. EXP_CONFIG=config/multinom/logit_simpson/exp_v1.yml)
endif
	@echo "▶ Expanding experiment design"
	bash $(JOBS_DIR)/expand_design.sh $(EXP_CONFIG)

# -------------------------------------------------
# Materialize experiment + simulations
# -------------------------------------------------
setup:
ifndef EXP_CONFIG
	$(error EXP_CONFIG must be set, e.g. EXP_CONFIG=config/multinom/logit_simpson/exp_v1.yml)
endif
	@$(MAKE) gen EXP_CONFIG=$(EXP_CONFIG)
	@echo "▶ Initializing experiment and simulations"
	bash $(JOBS_DIR)/init_exp.sh $(EXP_CONFIG)

# -------------------------------------------------
# Submit simulation jobs to SLURM
# -------------------------------------------------
submit:
ifndef EXP_CONFIG
	$(error EXP_CONFIG must be set, e.g. EXP_CONFIG=config/multinom/logit_simpson/exp_v1.yml)
endif
	@echo "▶ Submitting experiment to Slurm"
	bash $(JOBS_DIR)/submit_exp.sh $(EXP_CONFIG)

# -------------------------------------------------
# Full experiment (setup + submit)
# -------------------------------------------------
experiment:
ifndef EXP_CONFIG
	$(error EXP_CONFIG must be set, e.g. EXP_CONFIG=config/multinom/logit_simpson/exp_v1.yml)
endif
	@$(MAKE) setup EXP_CONFIG=$(EXP_CONFIG)
	@$(MAKE) submit EXP_CONFIG=$(EXP_CONFIG)
	@echo "✔ Experiment launched from $(EXP_CONFIG)"

# -------------------------------------------------
# Analyze experiment
# -------------------------------------------------
analyze:
ifndef EXP_CONFIG
	$(error EXP_CONFIG must be set, e.g. EXP_CONFIG=config/multinom/logit_simpson/exp_v1.yml)
endif
	@echo "▶ Analyzing simulations"
	bash $(JOBS_DIR)/analyze_all_sims.sh $(EXP_CONFIG)

# -------------------------------------------------
# Test single iteration locally (slurm-emulation mode)
# Saves to <sim>/iterations/iter_XXXX/model.rds so that
# analyze_sim.R can be run on models without any changes.
# Requires: make setup must have been run first.
# -------------------------------------------------
test-iter:
ifndef SIM_CONFIG
	$(error SIM_CONFIG must be set, e.g. SIM_CONFIG=experiments/<path>/<version>/<sim>/<sim>.yml)
endif
	@echo "▶ Running local test iteration"
	@ITER_INDEX=$(or $(ITER),1); \
	echo "  • simulation: $(SIM_CONFIG)"; \
	echo "  • iteration:  $$ITER_INDEX"; \
	Rscript $(SCRIPTS_DIR)/test_iter.R $(SIM_CONFIG) $$ITER_INDEX

# -------------------------------------------------
# Dry run (no side effects, predictive)
# -------------------------------------------------
dry-run:
ifndef EXP_CONFIG
	$(error EXP_CONFIG must be set, e.g. EXP_CONFIG=config/multinom/logit_simpson/exp_v1.yml)
endif
	@echo "▶ DRY RUN"
	@echo ""
	@echo "Experiment config: $(EXP_CONFIG)"
	@echo ""
	@echo "Steps that would run:"
	@echo "  1. bash $(JOBS_DIR)/expand_design.sh $(EXP_CONFIG)"
	@echo "  2. bash $(JOBS_DIR)/init_exp.sh $(EXP_CONFIG)"
	@echo "  3. bash $(JOBS_DIR)/submit_exp.sh $(EXP_CONFIG)"
	@echo ""
	@echo "To test a single iteration after setup:"
	@echo "  make test-iter SIM_CONFIG=experiments/<path>/<version>/<sim>/<sim>.yml [ITER=1]"
	@echo ""
	@echo "✔ Dry run complete"

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