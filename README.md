© 2025 The Regents of the University of Michigan  
Carson Dudley — University of Michigan

---

## Overview

**Simulation-Grounded Neural Networks (SGNNs)** are a general framework for training neural networks on mechanistic simulations to perform forecasting, inference, regression, and classification in scientific domains. SGNNs enable robust, interpretable, and data-efficient learning even in noisy, low-resource, or unobservable settings. The approach has been validated across epidemiology, chemistry, ecology, and social science.

SGNNs unify mechanistic reasoning and deep learning by treating simulations as flexible supervisory signals—rather than fixed priors or surrogates—enabling both zero-shot generalization and mechanistic interpretability via *back-to-simulation attribution*.

---

## Repository Contents

| File / Directory                | Description                                                                 |
|-------------------------------|-----------------------------------------------------------------------------|
| `chem_yield_zeroshot.ipynb`   | SGNN evaluation on chemical reaction yield prediction (zero-shot mode)     |
| `deaths.csv`                  | Example real-world mortality dataset (COVID-19, for forecasting task)      |
| `dengue_br.csv`               | Dengue case data for out-of-domain generalization benchmarking              |
| `einn_evals.ipynb`            | PINN/EINN baseline evaluations for disease forecasting                      |
| `generation_flu_death_hosp.ipynb` | Simulation script for multi-wave infectious disease trajectories       |
| `generation_v5_human.ipynb`   | Unified disease simulator with realistic observational effects             |
| `sgnn_forecaster.ipynb`       | Script for training SGNN disease forecaster                                 |
| `hybrid_chem_model_best.ipynb`| Architecture for chemistry SGNN hybrid (wide + deep) model                  |
| `r0_est.py`                   | SGNN-based estimation of $R_0$ from early outbreak curves                   |
| `source_id.ipynb`             | SGNN training and evaluation for source inference in diffusion cascades    |
| `README.md`                   | This file                                                                  |
| `LICENSE.txt`                 | Licensing information (see below)                                          |
| `NOTICES.txt`                 | Third-party notices and copyright                                          |

---

## Getting Started

### Installation

This repo requires Python 3.10+ and PyTorch 2.0+.


### Running the Code

Clone the repo and run any of the notebooks (`.ipynb`) in Jupyter or VS Code. For example:

- Run `generation_v5_human.ipynb` to generate synthetic infectious disease data.
- Use `source_id.ipynb` to evaluate SGNNs on cascade source inference.
- Run `chem_yield_zeroshot.ipynb` to evaluate zero-shot performance on chemical reactions.
- For reproduction number inference from early outbreak curves, run: python r0_est.py


Each notebook is self-contained and includes example runs on provided datasets.

---

## Documentation

The SGNN framework, simulation engines, architectures, and evaluation protocols are fully documented in our paper:

> **Simulation as Supervision: Mechanistic Pretraining for Scientific Discovery**  
> *Carson Dudley, Reiden Magdaleno, Christopher Harding, Marisa Eisenberg* (2025)  
> [Preprint link coming soon]



---

## References & Related Work

- Dudley et al., *Simulation as Supervision: Mechanistic Pretraining for Scientific Discovery* (2025) — [preprint pending]
- PINNs: Raissi et al. (2019), *Journal of Computational Physics*
- DEFSI: Hegde et al. (2020), *Nature Communications*
- PFNs: Schneider et al. (2022), *ICLR*

---

## Contact

**Carson Dudley**  
PhD Student
University of Michigan  
📧 cdud@umich.edu  
🌐 [carsondudley1.github.io](https://carsondudley1.github.io)

---

## License

This project is licensed under the MIT License. See [`LICENSE.txt`](LICENSE.txt) for full terms.  
© 2025 The Regents of the University of Michigan  
Carson Dudley — University of Michigan

---

## Notices

See [`NOTICES.txt`](NOTICES.txt) for third-party software attributions and licenses.

---

## Community & Roadmap

Community-building and roadmap planning are in progress. Future directions for SGNNs include:

- Domain-specific finetuning pipelines
- Active learning for simulation generation
- Real-time deployment interfaces
- Expanded simulator libraries across scientific fields
- Counterfactual/scenario predictions with SGNNs
- SGNNs + RL for optimal policies

If you're interested in collaborating or contributing to the SGNN ecosystem, please get in touch or watch this repository for updates.
