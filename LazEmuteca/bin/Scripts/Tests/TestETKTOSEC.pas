{
[Info]
This script tests uTOSECUtils.
[Author]
Name=Chixpy
Date=20171009
[Script]
}
program TestCHXStrUtils;

//uses uTOSECUtils;

{$I '../Units/uTOSECUtils.pas'}
var
  i: integer;
  aStr: string;
  TOSECTests: array of string;

begin
  WriteLn('Hello!');
  WriteLn('');
  WriteLn('This Script will test uTOSECUtils.');
  WriteLn('');

  TOSECTests := [
    'Soft Title, The v 1 (demo) (Year)(Publisher)(A3000)(MCGA)(Country)' +
    '(Language)(SW)(Devstatus)(Media Type)(Media Label)[cr][f][h][m]' +
    '[p][t][tr][o][u][v][b][a][!][more info]',
    'Soft Title (Year)(Publisher)',
    'Soft Title v10, The (Year)(Publisher)',
    'Soft Title, The v10 (Year)(Publisher)',
    'Soft Title v 10, The (Year)(Publisher)',
    'Soft Title, The v 10 (Year)(Publisher)'
    ];


  WriteLn(krsCSVSoftHeader);
  WriteLn('');

  i := Low(TOSECTests);
  while i <= High(TOSECTests) do
  begin
    WriteLn(TOSECTests[i]);
    aStr := TOSECExtractInfo(IntToStr(i) + TOSECIDSep + TOSECTests[i]);
    WriteLn(aStr);
    WriteLn('');
    Inc(i);
  end;  


  WriteLn('');
  WriteLn('DONE');
  WriteLn('----');

end.
