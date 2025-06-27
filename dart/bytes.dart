import 'dart:math';
import 'dart:typed_data';

import 'helper.dart';

main(List<String> args) {
  printTitle('Bytes');
  bytesDemo();
  printTitle('Endian');
  endianDemo();
}

bytesDemo() {
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

endianDemo() {
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
