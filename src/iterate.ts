import _ from "lodash";
import { IPopulation, ISpecimen } from "./@types";

export function iterate(
  population: IPopulation,
  fitnessFunction: (spec: number[]) => number
): IPopulation {
  const newSpecimenSets = population.specimens.map((s): ISpecimen[] =>
    new Array(population.config.numOffspringPerParent).fill(0).map(() => {
      const newSequence = [...s.sequence];
      const index = Math.floor(Math.random() * newSequence.length);
      newSequence[index] = _.clamp(newSequence[index] + (Math.random() > 0.5 ? -1 : 1), s.minValue, s.maxValue);
      return {
        sequence: newSequence,
        minValue: s.minValue,
        maxValue: s.maxValue,
        score: fitnessFunction(newSequence),
      };
    })
  );

  const newSpecimens = _.flatten(newSpecimenSets);

  return {
    specimens: getBestNSpecimens(
      [...population.specimens, ...newSpecimens],
      population.config.populationSize
    ),
    config: population.config,
  };
}

export function getBestNSpecimens(
  specimens: ISpecimen[],
  n: number
): ISpecimen[] {
  let maxScore = Math.max(...specimens.map((specimen) => specimen.score));
  return specimens.reduce((acc: ISpecimen[], curr: ISpecimen) => {
    if (acc.length < n) {
      acc.push(curr);
      if (curr.score > maxScore) {
        maxScore = curr.score;
      }
    } else if (curr.score < maxScore) {
      const maxScoreIndex = acc.findIndex(
        (specimen) => specimen.score === maxScore
      );
      acc[maxScoreIndex] = curr;
      maxScore = Math.max(...acc.map((specimen) => specimen.score));
    }
    return acc;
  }, []);
}
