export interface EditorFunction {
  name: string;
  function?: {
    content: string;
  };
}

const editorFunctions: EditorFunction[] = [
  {
    name: "function-1",
  },
  {
    name: "function-2",
  },
  {
    name: "function-3",
  },
];

export const getAllFunctions = (): EditorFunction[] => {
  return editorFunctions;
};

export const getOneFunction = (name: string): EditorFunction | undefined => {
  return editorFunctions.find((editorFunction) => editorFunction.name === name);
};

export const createFunction = (editorFunction: EditorFunction): void => {
  editorFunctions.push(editorFunction);
};

export const updateFunction = (functionName: EditorFunction): void => {
  const index = editorFunctions.findIndex((n) => n.name === functionName.name);
  console.log(index);
  editorFunctions[index] = functionName;
};

export const deleteFunction = (name: string): void => {
  const index = editorFunctions.findIndex(
    (editorFunction) => editorFunction.name === name
  );
  editorFunctions.splice(index, 1);
};
