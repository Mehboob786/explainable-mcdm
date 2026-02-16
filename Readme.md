# Explainable Multi-Criteria Decision-Making with Dynamic Weights

## Overview

This project is a research-oriented Haskell prototype for explaining Multi-Criteria Decision-Making (MCDM) outcomes when decision weights change over time.

The core goal is interpretability: the system shows not only which alternative wins, but also how each criterion contributes to ranking changes.

## Motivation

Classical MCDM methods such as the Weighted Sum Model (WSM) return aggregate scores but often do not provide transparent reasoning.

This project extends explanation-focused MCDM by supporting dynamic weights and by exposing per-criterion contribution changes across time steps.

## Current Features

- Weighted Sum Model (WSM) scoring for alternatives
- Per-criterion contribution analysis between alternatives
- Time-step comparison of contributions under changing weights
- Hardened domain model with safer types (`Criterion`, `Alternative`, `Weight`)
- Validation: non-negative weights
- Validation: weight normalization check (sum approximately `1.0`)
- Validation: duplicate weight detection
- Validation: duplicate score-entry detection (`Alternative`, `Criterion`)
- Validation: missing score detection for weighted criteria

## Project Structure

```text
app/
├── Main.hs         -- Example scenario and execution flow
├── Model.hs        -- Domain types + validation rules
├── Scoring.hs      -- Weighted Sum Model implementation
└── Explanation.hs  -- Contribution and explanation logic
```

## Installation

### 1. Install GHC and Cabal (recommended: ghcup)

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | bash
source ~/.zshrc
```

### 2. Verify tools

```bash
ghc --version
cabal --version
```

## How to Run

From the project root:

```bash
cabal update
cabal run
```

If model validation fails, the program prints a list of validation errors and skips scoring/explanation output.

## Example Output Sections

- Scores at time `T0`
- Scores at time `T1`
- Explanation of per-criterion contribution changes (`Cost`, `Quality`, `Speed`)

## Next Improvements

- Automated ranking-flip explanations (top drivers)
- Time-series weight trajectories beyond two time points
- Unit/property tests for validation and scoring invariants
