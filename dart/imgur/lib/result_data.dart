import 'package:imgur/epoch_datetime_converter.dart';
import 'package:json_annotation/json_annotation.dart';

part 'result_data.g.dart';

@JsonSerializable()
class ResultData {
  final String id;
  final String deletehash;
  final String type;
  final int width;
  final int height;
  final int size;
  @EpochDatetimeConverter.s()
  final DateTime datetime;
  final String link;
  final List<String> tags;

  ResultData(
    this.id,
    this.deletehash,
    this.type,
    this.width,
    this.height,
    this.size,
    this.datetime,
    this.link,
    this.tags,
  );

  factory ResultData.fromJson(Map<String, dynamic> json) =>
      _$ResultDataFromJson(json);

  Map<String, dynamic> toJson() => _$ResultDataToJson(this);
}
