export interface EditorFunctions {
  id: number;
  name: string;
  function?: {
    content: string;
  };
}

const editorFunctions: Array<EditorFunctions> = [
  {
    id: 1,
    name: "function 1",
  },
  {
    id: 2,
    name: "function 2",
  },
  {
    id: 3,
    name: "function 3",
  },
];

export const getAllFunctions = (): Array<EditorFunctions> => {
  return editorFunctions;
};

export const getOneFunction = (id: number): EditorFunctions | undefined => {
  return editorFunctions.find((editorFunction) => editorFunction.id === id);
};

export const createFunction = (editorFunction: EditorFunctions): void => {
  editorFunctions.push(editorFunction);
};

export const updateFunction = (editorFunction: EditorFunctions): void => {
  const index = editorFunctions.findIndex((u) => u.id === editorFunction.id);
  editorFunctions[index] = editorFunction;
};

export const deleteFunction = (id: number): void => {
  const index = editorFunctions.findIndex((u) => u.id === id);
  editorFunctions.splice(index, 1);
};
