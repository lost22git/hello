import 'dart:convert';
import 'dart:io';

import 'helper.dart';

main(List<String> args) async {
  printTitle('Process');
  await processDemo();
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
