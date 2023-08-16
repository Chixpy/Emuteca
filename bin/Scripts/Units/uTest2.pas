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

procedure Test3;

implementation

procedure Test2;
begin
  WriteLn('Test2');
  WriteLn('  Unit in "Units" subfolder from main program folder.');
  WriteLn('  ¡¡¡This is the WRONG FILE in "..\Units" folder!!!');
end;