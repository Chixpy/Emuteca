{
[Info]
Test unit for TestUses.pas
[Data]
Name=Chixpy
Version=0.01
Date=20230811
[Changes]
0.01 - 20230811
  * Initial version
[EndInfo]
}
unit uTest2;

interface

procedure Test2;

implementation

procedure Test2;
begin
  WriteLn('Test2');
  WriteLn('  Unit in 'Units' subfolder from main program folder.');
end;