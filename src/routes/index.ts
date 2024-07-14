import { Router } from "express";
import { mainRouter } from "./main.routes";
import { editorFunctionsRouter } from "./editor-functions.routes";

const router = Router();

router.use("/", mainRouter);
router.use("/editor-functions", editorFunctionsRouter);

export default router;
