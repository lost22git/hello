#!/usr/bin/env -S deno -N

type DnsType = "A" | "AAAA" | 1 | 28;

// deno-lint-ignore no-unused-vars
function description(type: DnsType): string {
  switch (type) {
    case 1:
    case "A":
      return "A";
    case 28:
    case "AAAA":
      return "AAAA";
    default:
      throw `unknown type: ${type}`;
  }
}

interface DnsQuestion {
  name: string;
  type: DnsType;
}

interface DnsAnswer {
  name: string;
  type: DnsType;
  TTL: number;
  data: string;
}

interface DnsRequest {
  name: string;
  type: DnsType;
}

interface DnsResponse {
  Status: number;
  TC: boolean;
  RD: boolean;
  RA: boolean;
  AD: boolean;
  CD: boolean;
  Question: Array<DnsQuestion>;
  Answer: Array<DnsAnswer>;
}

interface DnsResolveOptions {
  client?: Deno.HttpClient;
}

async function dnsResolve(
  request: DnsRequest | string,
  options?: DnsResolveOptions,
): Promise<DnsResponse> {
  const { name, type } = typeof request === "string"
    ? { name: request, type: "A" }
    : request;
  const url = `https://cloudflare-dns.com/dns-query?name=${name}&type=${type}`;
  const headers = { "Accept": "application/dns-json" };
  const resp = await fetch(url, {
    headers: headers,
    client: options?.client,
  });
  return resp.json();
}

const proxyUrl = "http://localhost:9933";
const client = Deno.createHttpClient({ proxy: { url: proxyUrl } });
const result = await dnsResolve("google.com", {
  client: client,
});
console.log(Deno.inspect(result));
