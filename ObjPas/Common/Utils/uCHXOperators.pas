unit uCHXOperators;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

// TPoint related
operator = (const a, b: TPoint): boolean;

implementation

operator = (const a, b: TPoint): boolean;
begin
  Result := (a.x = b.x) and (a.y = b.y)
end;

end.

