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
unit uTest3;

interface

procedure Test3;

implementation

procedure Test3;
begin
  WriteLn('Test3');
  WriteLn('  Unit in "..\Units" folder from the main program folder.');
end;
