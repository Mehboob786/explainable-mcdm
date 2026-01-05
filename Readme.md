# Explainable Multi-Criteria Decision-Making with Dynamic Weights

## Overview

This project presents a small, research-oriented prototype for **explaining results of multi-criteria decision-making (MCDM)** in settings where **decision weights evolve over time**. The focus is on *transparency* and *interpretability*, rather than predictive accuracy or large-scale optimization.

The prototype extends explanation-based MCDM techniques by tracing how **per-criterion contributions** change as weights are updated, thereby justifying why one alternative becomes preferred over another.

---

## Motivation

Classical MCDM methods such as the **Weighted Sum Model (WSM)** compute aggregate scores for alternatives but typically provide limited insight into *why* a particular alternative is preferred. This lack of explanation can reduce user trust, especially in high-stakes decision-support systems.

Recent work by **Erwig & Kumar (2025)** introduces a framework for explaining MCDM outcomes by maintaining fine-grained representations of intermediate values and generating comparison-based explanations. However, most existing approaches assume **static decision weights**.

This project explores the following question:

> *How can explanation-based MCDM be extended to scenarios where decision weights change dynamically over time?*

---

## Approach

The prototype implements:

- A **formal model** of alternatives, criteria, scores, and weights
- The **Weighted Sum Model (WSM)** for aggregating scores
- **Fine-grained tracking of per-criterion contributions**
- A simple model of **weight evolution**
- **Comparison-based explanations** that highlight how and why rankings change

All components are implemented using **pure functional programming in Haskell**, emphasizing immutability, compositionality, and clarity.

---

## Example Scenario

Two alternatives (`A` and `B`) are evaluated against three criteria:

- Cost
- Quality
- Speed

At time `T0`, the decision weights favor quality.  
At time `T1`, cost becomes more important.

The system produces:

- Aggregate scores at each time step
- Explanations such as:

> *"Criterion 'Cost' increased its contribution from 0.60 to 0.80, amplifying Alternative A’s advantage and leading to a ranking change."*

This makes explicit **which criteria caused the ranking shift and why**.

---

## Project Structure

app/
├── Main.hs -- Example scenarios and execution
├── Model.hs -- Core domain definitions
├── Scoring.hs -- Weighted Sum Model implementation
├── Explanation.hs -- Explanation logic



---

## How to Run

### Requirements
- GHC (tested with GHC 9.6.7)
- Cabal

### Run the project

From the project root:

```bash
cabal run
