import { Router, Request, Response } from "express";
import {
  createFunctionController,
  deleteFunctionController,
  getAllFunctionsController,
  getOneFunctionController,
  updateFunctionController,
} from "../controllers";

export const editorFunctionsRouter = Router();

editorFunctionsRouter.get("/", getAllFunctionsController);

editorFunctionsRouter.get("/:name", getOneFunctionController);

editorFunctionsRouter.post("/", createFunctionController);

editorFunctionsRouter.put("/:name", updateFunctionController);

editorFunctionsRouter.delete("/:name", deleteFunctionController);
