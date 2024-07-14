import express from "express";
import { readFileSync } from "fs";
import path from "path";
import router from "../routes";

const app = express(); // essential
app.use(express.json()); // essential = enables JSON body parser

app.use((req, res, next) => {
  console.log(`[${new Date().toISOString()}] ${req.method} ${req.url}`);
  next();
});

app.use(express.static(path.join(__dirname, "public"))); // ???

function getJSFilePath(filename: string) {
  if (!process.env.JS_PATH) {
    throw new Error("JS_PATH not set");
  }
  return path.join(process.env.JS_PATH, filename);
}

// Endpoint for opening a file
app.get("/open/:filename", (req, res) => {
  const filepath = getJSFilePath(req.params.filename);
  const content = readFileSync(filepath).toString();
  res.json({ filename: req.params.filename, content });
});

// Endpoint for saving a file
app.post("/save", express.json(), (req, res) => {
  const { filename, content } = req.body;

  console.log(`Saving file: ${filename} with content: ${content}`);
  res.json({ success: true });
});

// Endpoint for saving a file as
app.post("/saveAs", express.json(), (req, res) => {
  const { filename, content } = req.body;
  // Placeholder logic for saving a file as
  console.log(`Saving file as: ${filename} with content: ${content}`);
  res.json({ success: true });
});

app.post("/test-function", async (req, res) => {
  try {
    const body = req.body as {
      fnString: string;
      arguments: unknown[];
    };
    const fn = eval(`(${body.fnString})`);
    const result = fn(...(body.arguments || []));
    console.log(`returning result: ${result}`);
    res.end(JSON.stringify(result) + "\n");
  } catch (error: any) {
    console.error(error.message);
    res.status(500).json({ error: "Internal Server Error" });
  }
});

app.use("/", router);

app.listen(32794); // essential
console.log("listening on 32794...");
