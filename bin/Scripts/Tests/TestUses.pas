{
[Info]
This script tests searching unit
[Data]
Name=Chixpy
Version=0.01
Date=20230811
[Changes]
0.01 - 20230811
  * Initial version
[EndInfo]
}
program TestCHXBasic;

uses uTest1, uTest2, uTest3;

begin
  Test1;
  WriteLn('');
  Test2;
  WriteLn('');
  Test3;
  WriteLn('');
  WriteLn('¡Finished!');
end.
