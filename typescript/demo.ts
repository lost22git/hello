#!/usr/bin/env -S node 

interface Info {
  code: string;
  title: string;
  publishedDate: Date;
}

const info: Info = {
  code: "111",
  title: "sdfasd",
  publishedDate: new Date(),
};

console.log(JSON.stringify(info, null, 4));
