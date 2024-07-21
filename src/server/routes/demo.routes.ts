import { Router, Request, Response } from "express";

export const demoRouter = Router();

demoRouter.post("/test-function", async (req, res) => {
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