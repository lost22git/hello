///usr/bin/env bun test "$0" "$@" ; exit $?

import { $ } from "bun";
import { expect, test } from "bun:test";

test("run a successful shell cmd", async () => {
  const {stdout, stderr, exitCode} = await $`echo "hi"`.quiet();
  expect(stdout.toString()).toEqual("hi\n");
});

test("run a failed shell cmd", async () => {
  try {
    await $`bunnn`.quiet();
    expect(true).toBe(false)
  } catch(e) {
    const {stdout, stderr, exitCode} = e
    expect(exitCode).toEqual(1);
    expect(stdout.toString()).toEqual("");
    expect(stderr.toString()).toEqual("");
  }
});

test("run a failed shell cmd args", async () => {
  try {
    await $`bun -pp`.quiet();
    expect(true).toBe(false)
  } catch({stdout, stderr, exitCode}) {
    expect(exitCode).toEqual(1);
    expect(stdout.toString()).toEqual("");
    expect(stderr.toString()).toEqual(expect.stringContaining("error"));
  }
});

test("text(): if exitCode == 0, return stdout else throw error", async () => {
  // ok
  const stdoutText = await $`echo hi`.quiet().text();
  expect(stdoutText).toEqual("hi\n");

  // err
  try {
    await $`bunnn`.quiet().text();
    expect(true).toBe(false)
  } catch (e) {
    expect(e.exitCode).toEqual(1);
    expect(e.stdout.toString()).toEqual("");
    expect(e.stderr.toString()).toEqual("");
  }
});

test("text(base64)", async () => {
  const base64 = await $`echo hi`.quiet().text("base64");
  expect(base64).toEqual("aGkK");
});

test("run a failed cmd args and redirect stderr to stdout", async () => {
  try {
    await $`bun -pp 2>&1`.quiet();
    expect(true).toBe(false)
  } catch({stdout, stderr, exitCode}) {
    expect(exitCode).toEqual(1);
    expect(stdout.toString()).toEqual(expect.stringContaining("error"));
    expect(stderr.toString()).toEqual("");
  }
});

test("run a failed shell cmd and nothrow error", async () => {
  const {stdout, stderr, exitCode} = await $`bunnn`.quiet().nothrow();
  expect(exitCode).toEqual(1);
  expect(stdout.toString()).toEqual("");
  expect(stderr.toString()).toEqual("");
});

test("run a failed shell cmd args and nothrow error", async () => {
  const {stdout, stderr, exitCode} = await $`bun -pp`.quiet().nothrow();
  expect(exitCode).toEqual(1);
  expect(stdout.toString()).toEqual("");
  expect(stderr.toString()).toEqual(expect.stringContaining("error"));
});
