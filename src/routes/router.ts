import { Router } from "express";
import { mainRouter } from "./main.routes";
import { editorFunctionsRouter } from "./editor-functions.routes";

export const router = Router();

router.use("/", mainRouter);
router.use("/editor-functions", editorFunctionsRouter);
