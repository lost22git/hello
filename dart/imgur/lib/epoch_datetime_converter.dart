import 'package:json_annotation/json_annotation.dart';

enum EpochUnit { second, millisecond }

class EpochDatetimeConverter implements JsonConverter<DateTime, int> {
  final EpochUnit epochUnit;

  const EpochDatetimeConverter({required this.epochUnit});

  const EpochDatetimeConverter.s() : this(epochUnit: EpochUnit.second);
  const EpochDatetimeConverter.ms() : this(epochUnit: EpochUnit.millisecond);

  @override
  DateTime fromJson(int json) => switch (epochUnit) {
    EpochUnit.second => DateTime.fromMillisecondsSinceEpoch(json * 1000, isUtc: true),
    EpochUnit.millisecond => DateTime.fromMillisecondsSinceEpoch(json, isUtc: true),
  };

  @override
  int toJson(DateTime object) => switch (epochUnit) {
    EpochUnit.second => (object.millisecondsSinceEpoch / 1000).toInt(),
    EpochUnit.millisecond => object.millisecondsSinceEpoch,
  };
}
