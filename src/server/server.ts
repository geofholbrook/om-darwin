import express from "express";
import bodyParser from "body-parser"

const app = express();
app.use(express.json());

app.get("/", async (req, res) => {
  res.json({ message: "om-darwin 2.0 server (root endpoint)" });
});

app.post("/function", async (req, res) => {
  const body = req.body as {
    fnString: string;
    arguments: unknown[];
  };

  const fn = eval(`(${body.fnString})`);
  const result = fn(...body.arguments);
  res.json({ result });
});

app.listen(32794);
console.log("listening on 32794...");
