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
  WriteLn('  Software: ' + aSL.Count);

  aSl.Destroy;
end.
