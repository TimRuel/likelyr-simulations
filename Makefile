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
R_DIR           := R
BIN_DIR         := bin

# -------------------------------------------------
# Phony targets
# -------------------------------------------------
.PHONY: help gen setup submit experiment analyze-sim analyze-exp bundle results submit-analysis status dry-run clean test-sim download

# -------------------------------------------------
# Help
# -------------------------------------------------
help:
	@echo ""
	@echo "Experiment workflow (config-driven):"
	@echo "  make experiment  EXP_CONFIG=<path/to/exp_vX.yml>"
	@echo ""
	@echo "Individual steps:"
	@echo "  make gen         EXP_CONFIG=..."
	@echo "  make setup       EXP_CONFIG=..."
	@echo "  make submit      EXP_CONFIG=..."
	@echo ""
	@echo "Analysis (on Quest):"
	@echo "  make analyze-sim SIM_CONFIG=<path/to/sim_XX/sim_XX.yml>"
	@echo "  make analyze-exp EXP_CONFIG=<path/to/exp_vX.yml>"
	@echo "  make bundle      EXP_CONFIG=<path/to/exp_vX.yml>"
	@echo "  Analyzer is chosen by experiment\$$kind in the yaml:"
	@echo "    simulation (default) → frequency properties across iterations"
	@echo "    application          → per-dataset estimates, CIs and curves"
	@echo "  bundle collapses all sim_*/analysis into one analysis/bundle.rds."
	@echo ""
	@echo "Local testing:"
	@echo "  make test-sim    SIM_CONFIG=..."
	@echo "  Runs simulation.iterations test iterations locally using test: overrides."
	@echo "  Note: make setup must be run first to build model specs."
	@echo ""
	@echo "Download from Quest:"
	@echo "  make download    EXP=multinom/ne_entropy/exp_v13"
	@echo "  Downloads analysis/bundle.rds only (one file). Add MODE=tree to"
	@echo "  pull every sim_*/analysis folder instead."
	@echo "  Must be run from local machine with Northwestern VPN active."
	@echo ""
	@echo "End-to-end results refresh:"
	@echo "  Application experiment (light — login node is fine):"
	@echo "    Quest: make results         EXP_CONFIG=<path/to/exp_vX.yml>"
	@echo "  Simulation experiment (tens of thousands of models — needs Slurm):"
	@echo "    Quest: make submit-analysis EXP_CONFIG=<path/to/exp_vX.yml>"
	@echo "  Then, locally with VPN active:"
	@echo "    Local: make download        EXP=<family/estimand/exp_vX>"
	@echo ""
	@echo "  Note: this repo lives in \$$HOME; experiment data lives on project"
	@echo "  storage. Every target reads experiment\$$exp_dir from the yaml, so"
	@echo "  always run Make from the repo and let the config locate the data."
	@echo ""
	@echo "Utilities:"
	@echo "  make dry-run     EXP_CONFIG=..."
	@echo "  make status"
	@echo "  make clean       (refuses)"
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
# Analyze a single simulation
# -------------------------------------------------
analyze-sim:
ifndef SIM_CONFIG
	$(error SIM_CONFIG must be set, e.g. SIM_CONFIG=<exp_dir>/sim_XX/sim_XX.yml)
endif
	bash $(BIN_DIR)/analyze_sim.sh $(SIM_CONFIG)

# -------------------------------------------------
# Analyze all simulations in an experiment
# -------------------------------------------------
analyze-exp:
ifndef EXP_CONFIG
	$(error EXP_CONFIG must be set, e.g. EXP_CONFIG=config/multinom/logit_simpson/exp_v1.yml)
endif
	bash $(BIN_DIR)/analyze_exp.sh $(EXP_CONFIG)

# -------------------------------------------------
# Bundle an experiment's analysis into one file
#
# Collapses every sim_*/analysis output into
# <exp_dir>/analysis/bundle.rds. Run after analyze-exp; this is what
# `make download` transfers and what the dissertation figures read.
# -------------------------------------------------
bundle:
ifndef EXP_CONFIG
	$(error EXP_CONFIG must be set, e.g. EXP_CONFIG=config/multinom/ne_entropy/exp_v13/exp_v13.yml)
endif
	bash $(BIN_DIR)/bundle_exp.sh $(EXP_CONFIG)

# -------------------------------------------------
# Analyze + bundle inline (run on Quest)
#
# Suitable for APPLICATION experiments (~20 sims x 1 iteration). For a
# SIMULATION experiment this reads tens of thousands of model.rds files
# serially — use submit-analysis instead so it runs as a Slurm array
# rather than on a login node.
# -------------------------------------------------
results:
ifndef EXP_CONFIG
	$(error EXP_CONFIG must be set, e.g. EXP_CONFIG=config/multinom/ne_entropy/exp_v13/exp_v13.yml)
endif
	@$(MAKE) analyze-exp EXP_CONFIG=$(EXP_CONFIG)
	@$(MAKE) bundle EXP_CONFIG=$(EXP_CONFIG)
	@echo "✔ Results ready to download for $(EXP_CONFIG)"

# -------------------------------------------------
# Analyze + bundle via Slurm (run on Quest)
#
# One array task per simulation, then a dependent bundle job. This is the
# right target for simulation experiments.
# -------------------------------------------------
submit-analysis:
ifndef EXP_CONFIG
	$(error EXP_CONFIG must be set, e.g. EXP_CONFIG=config/multinom/logit_simpson/exp_v3/exp_v3.yml)
endif
	bash $(BIN_DIR)/submit_analysis.sh $(EXP_CONFIG)

# -------------------------------------------------
# Test single simulation locally
# -------------------------------------------------
# Test simulation locally
# Applies test: overrides, builds model, runs N iterations.
# All output saved to <exp_dir>/sim_XX/test_sim/
# Requires: make setup must have been run first.
# -------------------------------------------------
test-sim:
ifndef SIM_CONFIG
	$(error SIM_CONFIG must be set, e.g. SIM_CONFIG=<config_dir>/sim_XX.yml)
endif
	bash $(BIN_DIR)/test_sim.sh $(SIM_CONFIG)

# -------------------------------------------------
# Download analysis results from Quest to local
#
# Must be run from local machine with Northwestern VPN active.
# Requires SSH access to Quest.
#
# Usage:
#   make download EXP=multinom/ne_entropy/exp_v13
#   make download EXP=... MODE=tree     (per-sim files instead of bundle)
#
# Default MODE=bundle pulls only analysis/bundle.rds. Results stay in
# this repo's experiments/ tree; the dissertation reads them from here.
# -------------------------------------------------
download:
ifndef EXP
	$(error EXP must be set, e.g. make download EXP=multinom/ne_entropy/exp_v13)
endif
	bash $(BIN_DIR)/download_analysis.sh "$(EXP)" "$(if $(LOCAL),$(LOCAL),$(EXP))" "$(if $(MODE),$(MODE),bundle)"

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
	@echo "Paths are read directly from the config file (exp_dir, logs_dir, specs_dir)."
	@echo ""
	@echo "Steps that would run:"
	@echo "  1. bash $(BIN_DIR)/expand_design.sh $(EXP_CONFIG)"
	@echo "  2. bash $(BIN_DIR)/init_exp.sh $(EXP_CONFIG)"
	@echo "  3. bash $(BIN_DIR)/submit_exp.sh $(EXP_CONFIG)"
	@echo ""
	@echo "To analyze after runs complete:"
	@echo "  make analyze-sim SIM_CONFIG=<exp_dir>/sim_XX/sim_XX.yml"
	@echo "  make analyze-exp EXP_CONFIG=$(EXP_CONFIG)"
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