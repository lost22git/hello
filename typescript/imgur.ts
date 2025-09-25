#!/usr/bin/env -S deno -N -R

interface UploadResultErrorData {
  error: string;
}
interface UploadResultOkData {
  id: string;
  deletehash: string;
  type: string;
  width: number;
  height: number;
  size: number;
  link: string;
  datetime: number;
}

interface UploadResult {
  success: boolean;
  status: number;
  data: UploadResultErrorData | UploadResultOkData;
}

async function upload(file: string, proxyUrl?: string): Promise<UploadResult> {
  const client = proxyUrl
    ? Deno.createHttpClient({ proxy: { url: proxyUrl! } })
    : undefined;

  const url = "https://api.imgur.com/3/image?client_id=546c25a59c58ad7";
  const headers = { "Referer": "https://imgur.com/" };

  // formadata
  const data = await Deno.readFile(file);
  const formdata = new FormData();
  formdata.set("image", new Blob([data]));

  const resp = await fetch(url, {
    method: "POST",
    headers: headers,
    body: formdata,
    client: client,
  });
  return resp.json();
}

// === Main ===

const file = Deno.args[0];
const result = await upload(file);

// check if has the property: link
if ("link" in result.data) {
  console.log(result.data.link);
}
