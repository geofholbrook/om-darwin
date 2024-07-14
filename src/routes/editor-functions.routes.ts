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

editorFunctionsRouter.get("/:id", getOneFunctionController);

editorFunctionsRouter.post("/", createFunctionController);

editorFunctionsRouter.put("/:id", updateFunctionController);

editorFunctionsRouter.delete("/:id", deleteFunctionController);
