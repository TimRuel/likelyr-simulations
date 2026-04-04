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
R_DIR           := R
BIN_DIR         := bin

# -------------------------------------------------
# Phony targets
# -------------------------------------------------
.PHONY: help gen setup submit experiment analyze status dry-run clean test-sim

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
	@echo "  make test-sim   SIM_CONFIG=..."
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
	bash $(BIN_DIR)/expand_design.sh $(EXP_CONFIG)

# -------------------------------------------------
# Materialize experiment + simulations
# -------------------------------------------------
setup:
ifndef EXP_CONFIG
	$(error EXP_CONFIG must be set, e.g. EXP_CONFIG=config/multinom/logit_simpson/exp_v1.yml)
endif
	@$(MAKE) gen EXP_CONFIG=$(EXP_CONFIG)
	@echo "▶ Initializing experiment and simulations"
	bash $(BIN_DIR)/init_exp.sh $(EXP_CONFIG)

# -------------------------------------------------
# Submit simulation jobs to SLURM
# -------------------------------------------------
submit:
ifndef EXP_CONFIG
	$(error EXP_CONFIG must be set, e.g. EXP_CONFIG=config/multinom/logit_simpson/exp_v1.yml)
endif
	@echo "▶ Submitting experiment to Slurm"
	bash $(BIN_DIR)/submit_exp.sh $(EXP_CONFIG)

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
	bash $(BIN_DIR)/analyze_sim.sh $(EXP_CONFIG)

# -------------------------------------------------
# Test single simulation locally
# Writes test_sim.yml, builds test model, runs iteration.
# All output saved to <sim>/test_iteration/
# Requires: make setup must have been run first.
# -------------------------------------------------
test-sim:
ifndef SIM_CONFIG
	$(error SIM_CONFIG must be set, e.g. SIM_CONFIG=experiments/<path>/<version>/sim_XX/sim_XX.yml)
endif
	bash $(BIN_DIR)/test_sim.sh $(SIM_CONFIG)

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
	@echo "  1. bash $(BIN_DIR)/expand_design.sh $(EXP_CONFIG)"
	@echo "  2. bash $(BIN_DIR)/init_exp.sh $(EXP_CONFIG)"
	@echo "  3. bash $(BIN_DIR)/submit_exp.sh $(EXP_CONFIG)"
	@echo ""
	@echo "To test a simulation after setup:"
	@echo "  make test-sim SIM_CONFIG=experiments/<path>/<version>/sim_XX/sim_XX.yml"
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