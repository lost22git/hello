import 'package:json_annotation/json_annotation.dart';

import 'result_data.dart';

part 'result.g.dart';

@JsonSerializable(explicitToJson: true)
class Result {
  final int status;
  final bool success;
  final ResultData data;

  Result(this.status, this.success, this.data);

  factory Result.fromJson(Map<String, dynamic> json) => _$ResultFromJson(json);

  Map<String, dynamic> toJson() => _$ResultToJson(this);
}
