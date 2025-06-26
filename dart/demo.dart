#!/usr/bin/env dart

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:math';
import 'dart:typed_data';

void printTitle(String title) {
  print('---$title${"-" * (33 - 3 - title.length)}');
}

void main(List<String> args) async {
  printTitle('Bytes');
  bytesDemo();
  printTitle('Endian');
  endianDemo();
  printTitle('Enum');
  enumDemo();
  printTitle('Process');
  await processDemo();
}

void bytesDemo() {
  var rand = Random();

  // build from [ByteBuilder]
  var bb = BytesBuilder();
  for (var i = 0; i < 10; i++) {
    bb.addByte(rand.nextInt(256));
  }
  var bsFromBuilder = bb.toBytes();
  print('bytes (ByteBuilder): $bsFromBuilder');

  // copy from [List<Int>]
  var intList = List.generate(10, (i) => rand.nextInt(256), growable: false);
  var bsFromList = Uint8List.fromList(intList);
  print('bytes (List.generate): $bsFromList');

  // byte data
  var bd = ByteData(10);
  for (var i = 0; i < 10; i++) {
    bd.setUint8(i, rand.nextInt(256));
  }
  print('bytes (ByteData): ${bd.buffer.asUint8List(0, 10)}');
}

void endianDemo() {
  var intValue = 257;
  var bd = ByteData(8);

  bd.setInt32(0, intValue);
  print('$intValue (BE): ${bd.buffer.asUint8List(0, 4)}');

  bd.setInt32(0, intValue, Endian.little);
  print('$intValue (LE): ${bd.buffer.asUint8List(0, 4)}');

  print('offsetInBytes: ${bd.offsetInBytes}');
  print('lengthInBytes: ${bd.lengthInBytes}');
  print('elementSizeInBytes: ${bd.elementSizeInBytes}');
}

enum State { init, pending, running, pause, end }

void enumDemo() {
  for (var state in State.values) {
    print('[$state]name: ${state.name}; index: ${state.index}');
  }
}

Future<void> processDemo() async {
  var shellScript = 'cat ./demo.dart | wc';
  print('run shell script: $shellScript');
  var p = await Process.start('sh', ['-c', shellScript]);
  var code = await p.exitCode;
  print('code: $code');
  print('out:');
  await p.stdout
      .transform(const Utf8Decoder())
      .transform(const LineSplitter())
      .map((line) => line.trim())
      .forEach(print);

  print('err:');
  await p.stderr
      .transform(const Utf8Decoder())
      .transform(const LineSplitter())
      .map((line) => line.trim())
      .forEach(print);
}
