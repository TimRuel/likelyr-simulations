# ===============================
# likelyr-simulations Makefile
# ===============================

ifeq ($(OS),Windows_NT)
  SHELL := C:/rtools45/usr/bin/bash.exe
  .SHELLFLAGS := -lc
endif

.EXPORT_ALL_VARIABLES:

JOBS_DIR := jobs
CONFIG ?= config/exp_negbin_rws_nr_v1.yml

.PHONY: help setup submit status clean

help:
	@echo "Available targets:"
	@echo "  setup        Run experiment setup (once)"
	@echo "  submit       Submit Slurm simulation array"
	@echo "  status       Check Slurm job queue"
	@echo "  clean        Remove simulation outputs (DANGEROUS)"
	@echo ""
	@echo "Variables:"
	@echo "  CONFIG       Path to experiment config (default: $(CONFIG))"

# -------------------------------------------------
# One-time experiment setup
# -------------------------------------------------
setup:
	bash $(JOBS_DIR)/setup_experiment.sh $(CONFIG)

# -------------------------------------------------
# Submit simulation array
# -------------------------------------------------
submit:
	sbatch $(JOBS_DIR)/run_simulation.sh $(CONFIG)

# -------------------------------------------------
# Check Slurm queue
# -------------------------------------------------
status:
	squeue -u $$USER

# -------------------------------------------------
# Cleanup (optional, dangerous)
# -------------------------------------------------
clean:
	@echo "❌ This will delete all simulation outputs for the experiment defined in:"
	@echo "   $(CONFIG)"
	@echo "⚠️  Refusing to run automatically. Delete manually if you really mean it."
	@false
