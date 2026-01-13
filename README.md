# Bayesian Prior Distribution Classifier

An interactive Shiny R application that allows users to draw probability distributions and uses machine learning to classify which Bayesian prior distribution was drawn. This tool bridges the gap between visual intuition and formal statistical distributions by converting hand-drawn curves into identified probability distributions with their parameters.

## Overview

This application provides an innovative interface for learning and working with Bayesian prior distributions. Users can:

1. **Draw** probability distribution curves on an interactive canvas
2. **Classify** the drawn distribution automatically using image classification
3. **Explore** various parametric prior distributions interactively
4. **Learn** the mathematical properties and practical applications of different distributions

The core concept: **Drawing → Parameter Estimation** — transform your visual intuition of a distribution into a formal statistical model with estimated parameters.

## Key Features

### Interactive Drawing Interface
- Canvas-based drawing tool powered by Signature Pad
- Real-time visualization of probability distributions
- Clear and save functionality for iterative exploration

### Comprehensive Distribution Support

**Continuous Univariate Distributions:**
- Normal, Log-Normal, Uniform
- Exponential, Gamma, Student-t
- Beta, Cauchy, Half-Cauchy
- Inverse-Gamma, Inverse-Chi-Squared
- Logit-Normal

**Discrete Univariate Distributions:**
- Bernoulli, Binomial, Beta-Binomial
- Poisson, Negative-Binomial
- Discrete-Uniform

**Multivariate Distributions:**
- Multivariate Normal, Multivariate t
- Dirichlet, Multinomial
- Wishart, Inverse-Wishart
- LKJ Correlation

### Distribution Classifier
- Machine learning model trained on thousands of distribution images
- Organized training data by distribution type in `/train` directory
- Test dataset for validation in `/test` directory
- Classification graph structure in `/image_classifier`

### Educational Tools
- Dynamic parameter adjustment with sliders
- PDF (Probability Density Function) and CDF (Cumulative Distribution Function) plots
- Mathematical formulae for each distribution
- Code generation in multiple languages (R, Python, Stan, Julia, Matlab, Mathematica, C++)
- LaTeX representations
- Practical usage examples and tips

## Installation

### Prerequisites

```r
# Required R version
R >= 4.0.0

# Install required packages
install.packages(c(
  "shiny",
  "dplyr",
  "ggplot2",
  "shinyjs",
  "magick",
  "LaplacesDemon",
  "logitnorm",
  "actuar",
  "reshape",
  "mvtnorm",
  "ggExtra",
  "grid",
  "gridExtra",
  "DirichletReg",
  "scatterplot3d",
  "markdown",
  "tidyverse"
))
```

### Running the Application

```r
# Clone the repository
git clone https://github.com/hdbt/Bayesian_Prior_Viz.git
cd Bayesian_Prior_Viz

# Run the Shiny app
R -e "shiny::runApp('app.R')"
```

Or open `app.R` in RStudio and click "Run App".

## Usage

### Basic Workflow

1. **Select Distribution Category**: Choose from Continuous, Discrete, or Multivariate distributions
2. **Choose Specific Distribution**: Select the distribution you want to explore
3. **Adjust Parameters**: Use the sliders to modify distribution parameters
4. **Draw on Canvas**: Draw your interpretation of the distribution curve
5. **Save Drawing**: Click "Save" to capture and classify your drawing
6. **View Classification**: The system will identify which distribution family your drawing resembles

### Drawing Interface

The drawing canvas overlays the distribution plot, allowing you to:
- Draw with your mouse or touch input
- Compare your intuition against the actual distribution
- Save multiple attempts to build training data
- Clear and retry as needed

### Exploring Distributions

Navigate through tabs to:
- **Plot of PDF**: Visualize the probability density function
- **Plot of CDF**: View the cumulative distribution function
- **Formulae**: See mathematical definitions
- **LaTeX**: Get LaTeX code for papers
- **Code**: Generate code in your preferred language
- **Practical Tips**: Learn when to use each distribution

## Project Structure

```
Bayesian_Prior_Viz/
├── app.R                    # Main Shiny application
├── functions.R              # Custom distribution functions
├── formulae.R              # Mathematical formulae for distributions
├── plotting.R              # Plotting functions
├── PDF.R                   # PDF calculation functions
├── CDF.R                   # CDF calculation functions
├── code_*.R                # Code generation for different languages
├── example_uses.R          # Practical examples and tips
├── train/                  # Training images organized by distribution
│   ├── normal/
│   ├── beta/
│   ├── gamma/
│   ├── cauchy/
│   ├── exponential/
│   ├── half_cauchy/
│   ├── inverse_chi_squared/
│   ├── inverse_gamma/
│   ├── logit_normal/
│   ├── t/
│   └── uniform/
├── test/                   # Test images for validation
├── image_classifier/       # Image classification model
│   └── graph              # Model graph structure
├── www/                    # Web assets
│   ├── signature_pad.umd.js
│   └── css files
└── README.md              # This file
```

## Technical Details

### Distribution Functions

The application includes custom implementations for distributions not available in base R:
- `dCustomHalfCauchy()`: Half-Cauchy distribution
- `dCustomInverseChiSquared()`: Inverse Chi-Squared distribution
- `dCustomBetaBinomial()`: Beta-Binomial distribution

### Image Classification

The classifier uses:
- Neural network architecture defining the data flow through three components: ImageInput (receives image data) → ImageBlock (feature extraction) → ClassificationHead (distribution classification)
- Images stored as PNG files with two naming patterns:
  - Training data: `{DistributionName}_{id}.png` (organized in folders by distribution type)
  - Generated samples: `image_data_{dist}_{params}_end_{target}_{id}_{dist}.png` (where the first `{dist}` is the current distribution shown, and the second is the actual target class)
- Training data organized by distribution type in subdirectories
- Model architecture stored in JSON format describing the block structure and connections

### Code Generation

Dynamic code generation supports:
- R (base stats, LaplacesDemon)
- Python (scipy, numpy)
- Stan (probabilistic programming)
- Julia (Distributions.jl)
- Matlab (Statistics Toolbox)
- Mathematica (statistical functions)
- C++ (Boost libraries)

## Educational Applications

This tool is valuable for:

- **Statistics Education**: Visual learning of probability distributions
- **Bayesian Analysis**: Understanding prior selection
- **Research**: Quickly exploring distribution families
- **Communication**: Generating code and visualizations for papers
- **Model Building**: Selecting appropriate priors for Bayesian models

## Example Use Cases

1. **Prior Selection**: Visualize and compare candidate priors for Bayesian models
2. **Teaching**: Interactive demonstrations of distribution properties
3. **Data Generation**: Create training data for distribution classification
4. **Documentation**: Generate distribution plots and code for publications
5. **Experimentation**: Test intuitions about distribution shapes and parameters

## Contributing

Contributions are welcome! Areas for enhancement:
- Additional distribution families
- Improved classification models
- More language code generators
- Extended practical examples
- Enhanced drawing interface features

## Credits

This project builds on:
- **The Distribution Zoo** by Ben Lambert and Fergus Cooper
- Reference: Lewandowski, D., Kurowicka, D., & Joe, H. (2009). "Generating random correlation matrices based on vines and extended onion method." Journal of Multivariate Analysis, 100, 1989-2001.

## License

Please refer to the repository for license information.

## Support

For issues, questions, or contributions, please open an issue on the GitHub repository.

---

**Remember**: The best prior is the one that best represents your beliefs before seeing the data. This tool helps you explore and understand your options!
