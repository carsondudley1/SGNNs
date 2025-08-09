© 2025 The Regents of the University of Michigan  
Carson Dudley — University of Michigan

---

## Overview

**Simulation-Grounded Neural Networks (SGNNs)** are a general framework for training neural networks on mechanistic simulations to perform forecasting, inference, regression, and classification in scientific domains. SGNNs enable robust, interpretable, and data-efficient learning even in noisy, low-resource, or unobservable settings. The approach has been validated across epidemiology, chemistry, ecology, and social science.

SGNNs unify mechanistic reasoning and deep learning by treating simulations as flexible supervisory signals—rather than fixed priors or surrogates—enabling both zero-shot generalization and mechanistic interpretability via *back-to-simulation attribution*.

## 🚀 Try It Instantly

👉 [Run the Colab Tutorial]([https://colab.research.google.com/drive/1Epuq-6ZGUM67FOfWHnLGkld4-cb8EDW0?usp=sharing](https://colab.research.google.com/drive/1VpNm31JyZLVDnz9-qTpTkvCSjeMy3qpG#scrollTo=R8iy8THz9zjs&uniqifier=1))  
_No installation or coding required._

---

## Repository Contents

| File / Directory                | Description                                                                 |
|-------------------------------|-----------------------------------------------------------------------------|
| `sgnn_tutorial.ipynb` | End-to-end SGNN tutorial: stochastic multi-mechanism simulators (SIR, SEIR, SEAIR) + observation model + single-pass training and evaluation |
| `chem_yield_zeroshot.ipynb`   | SGNN evaluation on chemical reaction yield prediction (zero-shot mode)     |
| `deaths.csv`                  | Example real-world mortality dataset (COVID-19, for forecasting task)      |
| `dengue_br.csv`               | Dengue case data for out-of-domain generalization benchmarking              |
| `einn_evals.ipynb`            | PINN/EINN baseline evaluations for disease forecasting                      |
| `generation_flu_death_hosp.ipynb` | Simulation script for multi-wave infectious disease trajectories       |
| `generation_v5_human.ipynb`   | Unified disease simulator with realistic observational effects             |
| `sgnn_forecaster.ipynb`       | Script for training SGNN disease forecaster                                 |
| `hybrid_chem_model_best.ipynb`| SGNN pretraining + fine-tuning for chem yield prediction                   |
| `r0_est.py`                   | SGNN-based estimation of $R_0$ from early outbreak curves                   |
| `source_id.ipynb`             | SGNN training and evaluation for source inference in diffusion cascades    |
| `README.md`                   | This file                                                                  |
| `LICENSE.txt`                 | Licensing information (see below)                                          |
| `NOTICES.txt`                 | Third-party notices and copyright                                          |
| `FigureCode.R`                | Code for generating schematic and main figures in manuscript               |
| `Appendix.R`                  | Code for generating figure in appendix                                     |

---

## Getting Started

### Installation

This repo requires Python 3.10+ and PyTorch 2.0+ for model implementation. The repo also requires R 4.5.1 for MacOS.


**Recommended starting point for new users:**

sgnn_tutorial.ipynb — End-to-end SGNN workflow tutorial:

- Stochastic SIR, SEIR, and SEAIR simulators with day-by-day Poisson sampling.
- One-pass synthetic pretraining: generate enough data for a single epoch instead of looping over fixed samples.
- Observation model: under-reporting, reporting delays, and overdispersion for realistic training data.
- Lightweight CNN forecaster trained on case data.
- Validation on held-out synthetic data with forecast plots.


### Running the Code

Clone the repo and run any of the notebooks (`.ipynb`) in Jupyter or VS Code. For example:

- Run `generation_v5_human.ipynb` to generate synthetic infectious disease data.
- Use `source_id.ipynb` to evaluate SGNNs on cascade source inference.
- Run `hybrid_chem_model.ipynb` to evaluate SGNN performance on chemical reactions.
- For reproduction number inference from early outbreak curves, run: python r0_est.py


Each notebook is self-contained and includes example runs on provided datasets.

---

## Documentation

The SGNN framework, simulation engines, architectures, and evaluation protocols are fully documented in our paper:

> **Simulation as Supervision: Mechanistic Pretraining for Scientific Discovery**  
> *Carson Dudley, Reiden Magdaleno, Christopher Harding, Marisa Eisenberg* (2025)  
> [[Preprint]](https://arxiv.org/abs/2507.08977)



---

## References & Related Work

- Dudley et al., *Simulation as Supervision: Mechanistic Pretraining for Scientific Discovery* (2025) — (https://arxiv.org/abs/2507.08977)
- PINNs: Raissi et al. (2019), *Journal of Computational Physics*
- DEFSI: Wang et al. (2019), *AAAI*
- PFNs: Müller et al. (2022), *ICLR*

---

## Contact

**Carson Dudley**  
PhD Student
University of Michigan  
📧 cdud@umich.edu  
🌐 [carsondudley1.github.io](https://carsondudley1.github.io)

---

## License

See [`LICENSE.txt`](LICENSE.txt).  
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
