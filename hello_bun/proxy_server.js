///usr/bin/env bun "$0" "$@" ; exit $?

import { serve } from "bun";

const proxy = "http://127.0.0.1:55556";

const server = serve({
  port: 8000,
  fetch: async (req) => {
    const uri = req.url.replace("http://localhost:8000", "https://github.com");
    console.log(`Do request: [${req.method}] ${uri}`);
    req.headers.set("Host", "github.com");
    req.headers.set("User-Agent", "curl");
    try {
      return await fetch(
        uri,
        {
          verbose: true,
          proxy: proxy, // TODO: Error on windows
          method: req.method,
          headers: req.headers,
          body: req.body,
        }
      );
    } catch (err) {
      console.error(`Error: ${err}`)
      return new Response(err, { status: 500 });
    }
  },
});

console.log(`Proxy server is running on ${server.hostname}:${server.port}`);
