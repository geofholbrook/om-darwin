import express from "express";
import bodyParser from "body-parser";

const app = express();
app.use(express.json());

app.use((req, res, next) => {
    console.log(`[${new Date().toISOString()}] ${req.method} ${req.url}`);
    next();
});

app.get("/", async (req, res) => {
  res.json({ message: "om-darwin 2.0 server (root endpoint)" });
});

app.post("/test-function", async (req, res) => {
  try {
    const body = req.body as {
      fnString: string;
      arguments: unknown[];
    };

    const fn = eval(`(${body.fnString})`);
    const result = fn(...(body.arguments || []));
    res.json({ result });
  } catch (error: any) {
    console.error(error.message);
    res.status(500).json({ error: "Internal Server Error" });
  }
});

app.listen(32794);
console.log("listening on 32794...");
