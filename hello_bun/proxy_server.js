///usr/bin/env bun "$0" "$@" ; exit $?

import { serve } from "bun";

const proxy = process.env.HTTPS_PROXY

const server = serve({
  port: 8000,
  fetch: async (req) => {
    var target = URL.parse(req.url).searchParams.get("target");
    if (target == null || target == "") {
      return new Response("Param not found: target", { code: 400 })
    }
    target = URL.parse(target);
    console.log(`Do request: [${req.method}] ${target}`);
    req.headers.set("User-Agent", "curl");
    req.headers.set("host", target.host)
    try {
      return await fetch(
        target,
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
