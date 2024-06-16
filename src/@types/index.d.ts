export interface ISpecimen {
    sequence: number[];
    maxValue: number;
    score: number;
}
export interface IPopulation {
    specimens: ISpecimen[];
    config: {
        populationSize: number;
        numOffspringPerParent: number;
    };
}
