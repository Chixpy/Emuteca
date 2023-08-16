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
unit uTest1;

interface

procedure Test1;

implementation

procedure Test1;
begin
  WriteLn('Test1');
  WriteLn('  Unit in same folder that main program...');
  WriteLn('  ¡¡¡This is the WRONG FILE in "Units" subfolder!!!');
end;