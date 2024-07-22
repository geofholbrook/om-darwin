import { UserConfig, defineConfig } from "vite";
import react from "@vitejs/plugin-react";

export default defineConfig(() => {
  const viteConfig: UserConfig = {
    plugins: [
      react({
        jsxImportSource: "@emotion/react",
        babel: {
          plugins: ["@emotion/babel-plugin"],
        },
      }),
    ],
    resolve: {
      alias: [{ find: "~", replacement: "/src" }],
    },

    server: {
      port: 32794,
    },
  };

  return viteConfig;
});
