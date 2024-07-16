export interface EditorFunction {
  id: number;
  name: string;
  function?: {
    content: string;
  };
}

const editorFunctions: EditorFunction[] = [
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

export const getAllFunctions = (): EditorFunction[] => {
  return editorFunctions;
};

export const getOneFunction = (id: number): EditorFunction | undefined => {
  return editorFunctions.find((editorFunction) => editorFunction.id === id);
};

export const createFunction = (editorFunction: EditorFunction): void => {
  editorFunctions.push(editorFunction);
};

export const updateFunction = (editorFunction: EditorFunction): void => {
  const index = editorFunctions.findIndex(
    (editorFunction) => editorFunction.id === editorFunction.id
  );
  editorFunctions[index] = editorFunction;
};

export const deleteFunction = (id: number): void => {
  const index = editorFunctions.findIndex(
    (editorFunction) => editorFunction.id === id
  );
  editorFunctions.splice(index, 1);
};
