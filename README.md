# Simulation of a Queueing System at an Airport

## Project Description

This repository includes 8 previous versions ranging from the most primitive to the final version included here. The seventh version incorporates Monte Carlo methods for enhanced simulation accuracy.

This repository includes 8 previous versions ranging from the most primitive to the final version included here.

This repository contains a simulation of a queueing system at an airport, modeled in R. Different stages of the passenger boarding process are simulated: arrival, check-in, security, passport control, and boarding. The simulation employs probability distributions to represent service times and arrivals, including the use of a truncated normal and a Weibull distribution.

## Requirements

To run the code, you need to have R installed. You can install R from:
[R Project](https://www.r-project.org/)

## Code Structure

- **Sample generation:** Use of the Box-Muller algorithm to generate values from truncated normal distributions.
- **Event definition:** Modeling of different phases of the airport process.
- **Simulation logic:** Event control based on a simulation clock.
- **Results visualization:** Histograms and goodness-of-fit tests to evaluate the model.

## Execution

To run the simulation, simply open `Version_final.R` in R and execute the full script.

```r
source("Version_final.R")
```

## Expected Results

At the end of the simulation, the final state of each queue and how many passengers have reached each phase of the system will be printed on the screen.

Example output:

```
At the stopping moment, there were X people in the first stage
At the stopping moment, there were Y people in the second stage
...
```

## Future Improvements

- Implement dynamic visualization of the queueing system.
- Explore variations in service times to analyze sensitivity.
- Compare with simulation models in Python.

## Author

This project was developed as part of a queueing system simulation task.

## License

This project is freely available under the MIT license.

This repository includes 8 previous versions ranging from the most primitive to the final version included here.

