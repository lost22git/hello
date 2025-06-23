// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'result_data.dart';

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

ResultData _$ResultDataFromJson(Map<String, dynamic> json) => ResultData(
  json['id'] as String,
  json['deletehash'] as String,
  json['type'] as String,
  (json['width'] as num).toInt(),
  (json['height'] as num).toInt(),
  (json['size'] as num).toInt(),
  const EpochDatetimeConverter.s().fromJson((json['datetime'] as num).toInt()),
  json['link'] as String,
  (json['tags'] as List<dynamic>).map((e) => e as String).toList(),
);

Map<String, dynamic> _$ResultDataToJson(ResultData instance) =>
    <String, dynamic>{
      'id': instance.id,
      'deletehash': instance.deletehash,
      'type': instance.type,
      'width': instance.width,
      'height': instance.height,
      'size': instance.size,
      'datetime': const EpochDatetimeConverter.s().toJson(instance.datetime),
      'link': instance.link,
      'tags': instance.tags,
    };
