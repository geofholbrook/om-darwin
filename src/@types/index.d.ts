export interface ISpecimen {
    sequence: number[];
    minValue: number;
    maxValue: number;
    score: number;
}
export interface IPopulation {
    specimens: ISpecimen[];
    config: {
        populationSize: number;
        numOffspringPerParent: number;
        numCrosses: number;
    };
}
