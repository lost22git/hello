import 'dart:isolate';

import 'helper.dart';

main(List<String> args) async {
  printTitle('Isolate');
  await isolateDemo();
  printTitle('Actor');
  await actorDemo();
}

Future<void> isolateDemo() async {
  print('current isolate: ${currentIsolateName()}');
  var fibResult = await Isolate.run(() {
    print('current isolate (fib): ${currentIsolateName()}');
    return fib(11);
  }, debugName: 'fib');
  print('fibResult: ${fibResult}');
}

String currentIsolateName() {
  return Isolate.current.debugName ?? "unnamed";
}

int fib(int n) {
  if (n == 0) return 1;
  if (n == 1) return 1;
  return fib(n - 1) + fib(n - 2);
}

Future<void> actorDemo() async {
  var recvPort = ReceivePort('fibResultReceivePort');

  void entryPoint((SendPort, int) initData) {
    var (sendPort, n) = initData;
    print('current isolate: ${currentIsolateName()}');
    var result = fib(n);
    Isolate.exit(sendPort, ('last', result));
  }

  try {
    await Isolate.spawn(
      entryPoint,
      (recvPort.sendPort, 11),
      debugName: 'fib',
      onExit: recvPort.sendPort,
      onError: recvPort.sendPort,
    );

    await for (var e in recvPort) {
      print('receive: $e');
      switch (e) {
        case ('last', _):
          recvPort.close();
        default:
      }
    }
  } catch (e) {
    recvPort.close();
    print('error: ${e}');
  }
}
