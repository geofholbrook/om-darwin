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
      newSequence[index] = _.clamp(
        newSequence[index] + _.random(-3, 3),
        s.minValue,
        s.maxValue
      );
      return {
        sequence: newSequence,
        minValue: s.minValue,
        maxValue: s.maxValue,
        score: fitnessFunction(newSequence),
      };
    })
  );

  const newSpecimens = _.flatten(newSpecimenSets);
  const crossSpecimens = Array.from({
    length: population.config.numCrosses,
  }).map(() =>
    uniformCrossover(
      _.sample([...population.specimens, ...newSpecimens])!,
      _.sample([...population.specimens, ...newSpecimens])!
    )
  );
  crossSpecimens.forEach((s) => (s.score = fitnessFunction(s.sequence)));

  return {
    specimens: getBestNSpecimens(
      [...population.specimens, ...newSpecimens, ...crossSpecimens],
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

export function uniformCrossover(
  specimen1: ISpecimen,
  specimen2: ISpecimen
): ISpecimen {
  const newSequence = specimen1.sequence.map((gene, i) =>
    Math.random() > 0.5 ? gene : specimen2.sequence[i]
  );
  return {
    sequence: newSequence,
    minValue: specimen1.minValue,
    maxValue: specimen1.maxValue,
    score: Infinity,
  };
}
