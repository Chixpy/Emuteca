{
[Info]
Some Emuteca info.
[Data]
Name=Chixpy
Version=0.01
Date=20171010
[Changes]

[EndInfo]
}
program EmutecaInfo;
var
  aSL: TStringList;
begin
  aSL := TStringList.Create;
  WriteLn('Some info:');
  WriteLn('');
  WriteLn('EMUTECA')
  WriteLn('  TempFolder:' + Emuteca.TempFolder);

  Emuteca.SoftManager.AssingAllTo(aSL);
  WriteLn('  Software: ' + IntToStr(aSL.Count));

  aSl.Free;
end.
