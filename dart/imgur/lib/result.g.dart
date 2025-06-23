// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'result.dart';

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

Result _$ResultFromJson(Map<String, dynamic> json) => Result(
  (json['status'] as num).toInt(),
  json['success'] as bool,
  ResultData.fromJson(json['data'] as Map<String, dynamic>),
);

Map<String, dynamic> _$ResultToJson(Result instance) => <String, dynamic>{
  'status': instance.status,
  'success': instance.success,
  'data': instance.data.toJson(),
};
