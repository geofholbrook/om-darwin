import { Request, Response } from "express";
import {
  EditorFunction,
  getAllFunctions,
  getOneFunction,
  createFunction,
  updateFunction,
  deleteFunction,
} from "../repository/functions.editor";

export const getAllFunctionsController = (
  req: Request,
  res: Response
): void => {
  const editorFunctions: EditorFunction[] = getAllFunctions();
  res.status(200).json({ editorFunctions });
};

export const getOneFunctionController = (req: Request, res: Response): void => {
  const name: string = req.params.name;
  const editorFunction: EditorFunction | undefined = getOneFunction(name);
  if (editorFunction) {
    res.status(200).json({ editorFunction });
  } else {
    res.status(404).json({ message: "Function not found" });
  }
};

export const createFunctionController = (req: Request, res: Response): void => {
  const editorFunction: EditorFunction = req.body;
  createFunction(editorFunction);
  res.status(201).json({
    message: "Function created",
    editorFunction,
  });
};

export const updateFunctionController = (req: Request, res: Response): void => {
  const name: string = req.params.name;
  const update: EditorFunction = req.body;
  update.name = name;
  updateFunction(update);
  res.status(200).json({
    message: "Function updated",
    user: update,
  });
};

export const deleteFunctionController = (req: Request, res: Response): void => {
  const name = req.params.name;
  deleteFunction(name);
  res.status(200).json({
    message: `${name} deleted`,
  });
};
