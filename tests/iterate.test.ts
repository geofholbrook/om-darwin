import { expect, test } from "bun:test";
import { IPopulation } from "../src/@types";
import { iterate } from "../src/iterate";
import _ from "lodash";

test("single iteration", () => {
  const fitnessFunction = (spec: number[]) =>
    spec.reduce((acc, curr) => acc + Math.abs(5 - curr), 0);
  const pop: IPopulation = {
    specimens: [
      [5, 5, 0],
      [0, 5, 5],
      [5, 0, 5], // all of these are one mutation away from being perfect
    ].map((s) => ({ sequence: s, maxValue: 10, score: fitnessFunction(s) })),
    config: { populationSize: 3, numOffspringPerParent: 10 },
  };
  const newPop = iterate(pop, fitnessFunction);
  expect(newPop.specimens).toHaveLength(pop.config.populationSize);
  expect(
    _.sum(newPop.specimens.map((s) => s.score)) / pop.config.populationSize
  ).toBeLessThan(5); // very unlikely to fail
  console.log(newPop.specimens);
});
