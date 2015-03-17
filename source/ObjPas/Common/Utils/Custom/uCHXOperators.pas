unit uCHXOperators;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  rseInvalidTPoint = '"%s" is an invalid TPoint.';
  rseInvalidTRect = '"%s" is an invalid TRect.';

// TPoint related
// --------------
operator = (const a, b: TPoint): boolean;
operator := (const Value : string) : TPoint;
operator := (const Value : TPoint) : string;

// TRect related
operator = (const a, b: TRect): boolean;
operator := (const Value : string) : TRect;
operator := (const Value : TRect) : string;

implementation

operator = (const a, b: TPoint): boolean;
begin
  Result := PointsEqual(a,b);
end;

operator := (const Value: string): TPoint;
var
  Position: integer;
begin
  try
    Position := Pos(',', Value);
    Result.x := StrToInt(Trim(Copy(Value, 1, Position - 1)));
    Result.y := StrToInt(Trim(Copy(Value, Position + 1, MaxInt)));
  except
    raise EConvertError.CreateFmt(rseInvalidTPoint, [Value]);
  end;
end;

operator := (const Value: TPoint): string;
begin
  Result := IntToStr(Value.x) + ',' + IntToStr(Value.y);
end;

operator = (const a, b: TRect): boolean;
begin
  Result := (a.TopLeft = b.TopLeft) and (a.BottomRight = b.BottomRight);
end;

operator := (const Value: string): TRect;
var
  Position: integer;
  tmpStr: string;
begin
  try
    tmpStr := Value;
    Position := Pos(',', tmpStr);
    Result.Left := StrToInt(Trim(Copy(tmpStr, 1, Position - 1)));

    tmpStr := Trim(Copy(tmpStr, Position + 1, MaxInt));
    Position := Pos(',', tmpStr);
    Result.Top := StrToInt(Trim(Copy(tmpStr, 1, Position - 1)));

    tmpStr := Trim(Copy(tmpStr, Position + 1, MaxInt));
    Position := Pos(',', tmpStr);
    Result.Right := StrToInt(Trim(Copy(tmpStr, 1, Position - 1)));

    Result.Bottom := StrToInt(Trim(Copy(tmpStr, Position + 1, MaxInt)));
  except
    raise EConvertError.CreateFmt(rseInvalidTRect, [Value]);
  end;
end;

operator := (const Value: TRect): string;
begin
  Result := IntToStr(Value.Left) + ',' + IntToStr(Value.Top) + ',' +
    IntToStr(Value.Right) + ',' + IntToStr(Value.Bottom);
end;

end.

