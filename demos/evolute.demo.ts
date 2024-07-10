import _ from "lodash";
import { IPopulation, ISpecimen } from "../src/@types";
import { iterate } from "../src/iterate";

function createRandomSpecimen(
  length: number,
  minValue: number,
  maxValue: number
): ISpecimen {
  return {
    sequence: Array.from(
      { length },
      () => Math.floor(Math.random() * (maxValue - minValue + 1)) + minValue
    ),
    minValue,
    maxValue,
    score: Infinity,
  };
}

let population: IPopulation = {
  specimens: Array.from({ length: 5 }, () => createRandomSpecimen(100, 0, 100)),
  config: {
    populationSize: 5,
    numOffspringPerParent: 10,
    numCrosses: 50,
  },
};

let generation = 0;
let bestScore = Infinity;
while (generation < 1000 && bestScore > 0) {
  population = iterate(population, (s) => s.reduce((acc, x) => acc + x, 0));
  bestScore = _.minBy(population.specimens, (s) => s.score)!.score;
  if (generation % 10 === 0)
    console.log(`After generation ${generation}, best score is ${bestScore}`);
  generation++;
}

console.log(_.minBy(population.specimens, (s) => s.score)?.sequence.join());
