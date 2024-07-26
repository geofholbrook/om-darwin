import { describe, test, expect } from "bun:test";

import supertest from "supertest";
import { app as originalApp } from "../src/server/server";

const app = supertest(originalApp);

describe("tests concerning the js function repository", () => {
  test("root endpoint", async () => {
    const response = await app.get("/");
    expect(response.status).toBe(200);
  });

  test("create and fetch with the /editor-functions route", async () => {
    await app.post("/editor-functions").send({
      name: "foo",
      content: "console.log('hello world');",
    });

    const response = await app.get("/editor-functions/foo");
    expect(response.status).toBe(200);
    expect(response.body.editorFunction.name).toBe("foo");
  });

  test("update an existing function with /editor-functions", async () => {
    await app.post("/editor-functions").send({
      name: "foobar",
      content: "console.log('hello world');",
    });

    await app.put("/editor-functions/foobar").send({
      name: "foobar",
      content: "console.log('goodbye world');",
    });

    const response = await app.get("/editor-functions/foobar");
    expect(response.status).toBe(200);
    expect(response.body.editorFunction.content).toBe(
      "console.log('goodbye world');"
    );
  });

  
});
