import 'helper.dart';

main(List<String> args) async {
  printTitle('Enum');
  enumDemo();
}

enum State { init, pending, running, pause, end }

enumDemo() {
  for (var state in State.values) {
    print('[$state]name: ${state.name}; index: ${state.index}');
  }
}
