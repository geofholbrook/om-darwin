import express from "express";
import { readFileSync } from "fs";
import path from "path";
import { editorFunctionsRouter } from "./routes/editor-functions.routes";
import { mainRouter } from "./routes/main.routes";
import { demoRouter } from "./routes/demo.routes";

export const app = express();

app.use(express.json());

app.use((req, res, next) => {
  console.log(`[${new Date().toISOString()}] ${req.method} ${req.url}`);
  next();
});

app.use(express.static(path.join(__dirname, "public")));

app.use("/", mainRouter);
app.use("/editor-functions", editorFunctionsRouter);
app.use("/", demoRouter)